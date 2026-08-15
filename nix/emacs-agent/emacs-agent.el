;;; emacs-agent.el --- Emacs-owned coding-agent control API -*- lexical-binding: t -*-

;;; Commentary:

;; A small JSON API over the local Ghostel tracker.  External callers enter
;; through `emacs-agent-api-call-base64'; all buffer and terminal operations
;; remain inside the running Emacs daemon.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-track)

(declare-function ghostel "ghostel" (&optional arg))
(declare-function ghostel-active-screen-text "ghostel" ())
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function ghostel--copy-all-text "ghostel-module" (term))
(declare-function pm-agent--cwd "pm-agent" (project))
(defvar ghostel--pid)
(defvar ghostel--term)
(defvar ghostel-buffer-name)
(defvar pm-executable)

(defgroup emacs-agent nil
  "Control coding agents running in local Ghostel terminals."
  :group 'applications)

(defcustom emacs-agent-shell-start-delay 0.5
  "Seconds to wait before submitting a launch command to a new shell."
  :type 'number
  :group 'emacs-agent)

(define-error 'emacs-agent-api-error "Emacs agent API error")

(defconst emacs-agent--name-regexp "\\`[a-z][a-z0-9_-]\\{0,31\\}\\'")
(defconst emacs-agent--kinds '("claude" "codex" "cursor"))
(defconst emacs-agent--key-aliases
  '(("esc" . "escape") ("escape" . "escape")
    ("enter" . "return") ("return" . "return")
    ("tab" . "tab") ("backspace" . "backspace")
    ("delete" . "delete") ("insert" . "insert")
    ("up" . "up") ("down" . "down")
    ("left" . "left") ("right" . "right")
    ("home" . "home") ("end" . "end")
    ("pageup" . "prior") ("pgup" . "prior")
    ("pagedown" . "next") ("pgdown" . "next")))

(defun emacs-agent--raise (code message &optional details)
  "Raise an API error with CODE, MESSAGE, and optional DETAILS."
  (signal 'emacs-agent-api-error (list code message details)))

(defun emacs-agent--request-value (key request &optional default)
  "Return KEY from REQUEST, falling back to DEFAULT."
  (if-let* ((entry (assq key request))) (cdr entry) default))

(defun emacs-agent--buffer-focused-p (buffer)
  "Return non-nil when BUFFER is selected in the selected frame."
  (and (buffer-live-p buffer)
       (eq buffer (window-buffer (selected-window)))))

(defun emacs-agent--timestamp (milliseconds)
  "Convert MILLISECONDS since the epoch to UTC RFC3339."
  (when (and milliseconds (> milliseconds 0))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                        (seconds-to-time (/ milliseconds 1000.0)) t)))

(defun emacs-agent--public-record (record)
  "Return the public representation of tracker RECORD."
  (let* ((id (alist-get 'id record))
         (buffer (emacs-agent-track-buffer-for-id id))
         (session-id (alist-get 'vendor_session_id record)))
    `((id . ,id)
      (run_id . ,(alist-get 'run_id record))
      (name . ,(alist-get 'name record))
      (kind . ,(or (alist-get 'kind record) (alist-get 'agent record)))
      (buffer_name . ,(and buffer (buffer-name buffer)))
      (project . ,(alist-get 'project record))
      (cwd . ,(alist-get 'cwd record))
      (status . ,(or (alist-get 'status record) "unknown"))
      (focused . ,(if (emacs-agent--buffer-focused-p buffer) t :json-false))
      (vendor_session_id . ,(unless (string-empty-p (or session-id "")) session-id))
      (title . ,(alist-get 'title record))
      (title_source . ,(alist-get 'title_source record))
      (activity . ,(alist-get 'activity record))
      (last_activity_at . ,(emacs-agent--timestamp
                            (alist-get 'last_activity_at record)))
      (revision . ,(or (alist-get 'revision record) 1)))))

(defun emacs-agent--records-for-target (target)
  "Return live tracker records matching TARGET."
  (let ((by-id (and target (gethash target emacs-agent-track--sessions))))
    (if by-id (list by-id)
      (seq-filter (lambda (record) (equal target (alist-get 'name record)))
                  (emacs-agent-track-sessions)))))

(defun emacs-agent--resolve (target)
  "Resolve TARGET to one unique live tracker record."
  (unless (and (stringp target) (not (string-empty-p target)))
    (emacs-agent--raise "invalid_request" "target must be a non-empty string"))
  (pcase (emacs-agent--records-for-target target)
    ('() (emacs-agent--raise "not_found" (format "agent not found: %s" target)))
    (`(,record) record)
    (_ (emacs-agent--raise "ambiguous_target"
                           (format "agent name is not unique: %s" target)))))

(defun emacs-agent--record-buffer (record)
  "Return RECORD's live Ghostel buffer or raise an error."
  (or (emacs-agent-track-buffer-for-id (alist-get 'id record))
      (emacs-agent--raise "agent_exited" "agent buffer is no longer live")))

(defun emacs-agent--name-reserved-p (name)
  "Return non-nil when NAME belongs to a live or starting agent."
  (seq-some
   (lambda (buffer)
     (with-current-buffer buffer
       (and (equal emacs-agent-name name)
            (or emacs-agent-start-pending
                (and emacs-agent-id (gethash emacs-agent-id
                                             emacs-agent-track--sessions))))))
   (buffer-list)))

(defun emacs-agent--validate-name (name)
  "Validate NAME and ensure it is not reserved."
  (unless (and (stringp name)
               (let ((case-fold-search nil))
                 (string-match-p emacs-agent--name-regexp name)))
    (emacs-agent--raise
     "invalid_name"
     "name must match [a-z][a-z0-9_-]{0,31}"))
  (when (emacs-agent--name-reserved-p name)
    (emacs-agent--raise "name_in_use" (format "agent name is in use: %s" name))))

(defun emacs-agent--validate-kind (kind)
  "Validate and return KIND."
  (unless (member kind emacs-agent--kinds)
    (emacs-agent--raise "invalid_request"
                        "kind must be claude, codex, or cursor"))
  kind)

(defun emacs-agent--codex-profile-present-p (args)
  "Return non-nil when ARGS explicitly select a Codex profile."
  (seq-some (lambda (arg)
              (or (equal arg "-p") (equal arg "--profile")
                  (string-prefix-p "--profile=" arg)))
            args))

(defun emacs-agent--launch-argv (kind project args)
  "Build a shell argv for KIND in PROJECT with forwarded ARGS."
  (let* ((agent-args (if (and (equal kind "codex")
                              (not (emacs-agent--codex-profile-present-p args)))
                         (append '("--profile" "emacs-agent") args)
                       args))
         (pm (or (and (boundp 'pm-executable) pm-executable) "pm")))
    (append (list pm "agent" kind "--project" project)
            (and agent-args (cons "--" agent-args)))))

(defun emacs-agent--display-buffer (buffer)
  "Display BUFFER in the most recently selected visible client frame."
  (let ((frame (or (and (frame-visible-p (selected-frame)) (selected-frame))
                   (seq-find #'frame-visible-p (frame-list)))))
    (unless frame
      (emacs-agent--raise "no_client_frame" "no visible Emacs client frame"))
    (select-frame-set-input-focus frame)
    (with-selected-frame frame (pop-to-buffer buffer))))

(defun emacs-agent--start (request)
  "Start the agent described by REQUEST and return its provisional identity."
  (require 'ghostel)
  (require 'pm-agent)
  (let* ((name (emacs-agent--request-value 'name request))
         (kind (emacs-agent--validate-kind
                (emacs-agent--request-value 'kind request)))
         (project (emacs-agent--request-value 'project request))
         (args (or (emacs-agent--request-value 'args request) '()))
         (focus (eq (emacs-agent--request-value 'focus request) t)))
    (emacs-agent--validate-name name)
    (unless (and (stringp project) (not (string-empty-p project)))
      (emacs-agent--raise "invalid_request" "project must be a non-empty string"))
    (unless (and (listp args) (seq-every-p #'stringp args))
      (emacs-agent--raise "invalid_request" "args must be an array of strings"))
    (let* ((cwd (pm-agent--cwd project))
           (default-directory cwd)
           (ghostel-buffer-name (format "*agent: %s*" name))
           (buffer (progn
                     (unless (file-directory-p cwd)
                       (emacs-agent--raise "not_found"
                                           (format "pm project not found: %s" project)))
                     (ghostel t)))
           id command)
      (with-current-buffer buffer
        (setq-local emacs-agent-name name
                    emacs-agent-start-pending t)
        (setq id (emacs-agent-track-ensure-buffer-id)
              command (mapconcat #'shell-quote-argument
                                 (emacs-agent--launch-argv kind project args) " ")))
      (run-at-time
       emacs-agent-shell-start-delay nil
       (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (condition-case err
                 (progn (ghostel-paste-string command)
                        (ghostel-send-key "return"))
               (error
                (setq emacs-agent-start-pending nil)
                (message "emacs-agent: launch failed: %s"
                         (error-message-string err))))))))
      (when focus (emacs-agent--display-buffer buffer))
      `((type . "start") (id . ,id) (name . ,name) (kind . ,kind)
        (project . ,project) (buffer_name . ,(buffer-name buffer))))))

(defun emacs-agent--get (request)
  "Return one agent selected by REQUEST."
  `((type . "agent")
    (agent . ,(emacs-agent--public-record
               (emacs-agent--resolve
                (emacs-agent--request-value 'target request))))))

(defun emacs-agent--list ()
  "Return every live local agent."
  (let ((records (mapcar #'emacs-agent--public-record
                         (emacs-agent-track-sessions))))
    (setq records
          (sort records
                (lambda (left right)
                  (string< (or (alist-get 'name left) (alist-get 'id left))
                           (or (alist-get 'name right) (alist-get 'id right))))))
    `((type . "agents") (agents . ,(vconcat records)))))

(defun emacs-agent--prompt (request)
  "Submit REQUEST's text to its target agent."
  (let* ((record (emacs-agent--resolve
                  (emacs-agent--request-value 'target request)))
         (text (emacs-agent--request-value 'text request))
         (buffer (emacs-agent--record-buffer record)))
    (unless (stringp text)
      (emacs-agent--raise "invalid_request" "text must be a string"))
    (with-current-buffer buffer
      (ghostel-paste-string text)
      (ghostel-send-key "return"))
    `((type . "agent") (agent . ,(emacs-agent--public-record record)))))

(defun emacs-agent--tail-lines (text count)
  "Return the last COUNT logical lines of TEXT."
  (let* ((lines (split-string (or text "") "\n" nil))
         (drop (max 0 (- (length lines) count))))
    (mapconcat #'identity (nthcdr drop lines) "\n")))

(defun emacs-agent--read (request)
  "Read terminal text selected by REQUEST."
  (let* ((record (emacs-agent--resolve
                  (emacs-agent--request-value 'target request)))
         (source (or (emacs-agent--request-value 'source request) "recent"))
         (lines (or (emacs-agent--request-value 'lines request) 80))
         (buffer (emacs-agent--record-buffer record))
         text)
    (unless (and (integerp lines) (> lines 0))
      (emacs-agent--raise "invalid_request" "lines must be a positive integer"))
    (with-current-buffer buffer
      (setq text
            (pcase source
              ("recent" (and (bound-and-true-p ghostel--term)
                              (ghostel--copy-all-text ghostel--term)))
              ((or "visible" "detection") (ghostel-active-screen-text))
              (_ (emacs-agent--raise
                  "invalid_request" "source must be recent, visible, or detection")))))
    `((type . "read") (source . ,source) (lines . ,lines)
      (text . ,(emacs-agent--tail-lines text lines))
      (agent . ,(emacs-agent--public-record record)))))

(defun emacs-agent--key-spec (alias)
  "Translate key ALIAS into a Ghostel (KEY . MODIFIERS) pair."
  (let ((down (downcase alias)))
    (cond
     ((assoc down emacs-agent--key-aliases)
      (cons (cdr (assoc down emacs-agent--key-aliases)) nil))
     ((string-match "\\`ctrl\\+\\(.\\)\\'" down)
      (cons (match-string 1 down) "ctrl"))
     ((string-match "\\`f\\([1-9]\\|1[0-2]\\)\\'" down)
      (cons down nil))
     (t (emacs-agent--raise "invalid_request"
                            (format "unknown key alias: %s" alias))))))

(defun emacs-agent--send-keys (request)
  "Send REQUEST's key aliases to its target."
  (let* ((record (emacs-agent--resolve
                  (emacs-agent--request-value 'target request)))
         (keys (emacs-agent--request-value 'keys request))
         (buffer (emacs-agent--record-buffer record)))
    (unless (and (listp keys) keys (seq-every-p #'stringp keys))
      (emacs-agent--raise "invalid_request" "keys must be a non-empty array"))
    (with-current-buffer buffer
      (dolist (alias keys)
        (pcase-let ((`(,key . ,mods) (emacs-agent--key-spec alias)))
          (ghostel-send-key key mods))))
    `((type . "agent") (agent . ,(emacs-agent--public-record record)))))

(defun emacs-agent--focus (request)
  "Focus REQUEST's target and mark its completion seen."
  (let* ((record (emacs-agent--resolve
                  (emacs-agent--request-value 'target request)))
         (buffer (emacs-agent--record-buffer record))
         (id (alist-get 'id record)))
    (emacs-agent--display-buffer buffer)
    (emacs-agent-track-mark-seen id)
    `((type . "agent")
      (agent . ,(emacs-agent--public-record
                 (or (gethash id emacs-agent-track--sessions) record))))))

(defun emacs-agent--stop (request)
  "Signal REQUEST's target foreground process group."
  (let* ((record (emacs-agent--resolve
                  (emacs-agent--request-value 'target request)))
         (buffer (emacs-agent--record-buffer record))
         (expected-run (alist-get 'run_id record))
         (expected-kind (alist-get 'kind record))
         current-run foreground kind pgrp)
    (with-current-buffer buffer
      (setq current-run emacs-agent-track--run-id
            foreground (and (bound-and-true-p ghostel--pid)
                            (emacs-agent-track--foreground-processes ghostel--pid))
            kind (emacs-agent-track--classify-processes foreground)
            pgrp (and foreground (plist-get (car foreground) :pgrp)))
      (unless (and (equal current-run expected-run)
                   (equal kind expected-kind)
                   (integerp pgrp) (> pgrp 1)
                   (equal pgrp emacs-agent-track--foreground-pgrp))
        (emacs-agent--raise "run_replaced"
                            "agent process changed before it could be stopped"))
      (condition-case err
          (signal-process (- pgrp) 'SIGTERM)
        (error (emacs-agent--raise "internal_error"
                                   (format "failed to stop process group: %s"
                                           (error-message-string err))))))
    `((type . "stop") (agent . ,(emacs-agent--public-record record)))))

(defun emacs-agent--cancel-start (request)
  "Release a provisional name reservation described by REQUEST."
  (let* ((id (emacs-agent--request-value 'id request))
         (buffer (emacs-agent-track-buffer-for-id id)))
    (when (and buffer (not (gethash id emacs-agent-track--sessions)))
      (with-current-buffer buffer
        (setq emacs-agent-name nil emacs-agent-start-pending nil)))
    `((type . "cancel_start") (id . ,id))))

(defun emacs-agent--dispatch (request)
  "Dispatch decoded API REQUEST."
  (pcase (emacs-agent--request-value 'op request)
    ("start" (emacs-agent--start request))
    ("get" (emacs-agent--get request))
    ("list" (emacs-agent--list))
    ("prompt" (emacs-agent--prompt request))
    ("read" (emacs-agent--read request))
    ("send_keys" (emacs-agent--send-keys request))
    ("focus" (emacs-agent--focus request))
    ("stop" (emacs-agent--stop request))
    ("cancel_start" (emacs-agent--cancel-start request))
    (_ (emacs-agent--raise "invalid_request" "unknown operation"))))

(defun emacs-agent--encode-response (response)
  "JSON/base64 encode RESPONSE for `emacsclient --eval'."
  (base64-encode-string
   (encode-coding-string
    (json-serialize response :null-object nil :false-object :json-false)
    'utf-8)
   t))

;;;###autoload
(defun emacs-agent-api-call-base64 (encoded-request)
  "Execute base64 JSON ENCODED-REQUEST and return a base64 JSON envelope."
  (emacs-agent--encode-response
   (condition-case err
       (let* ((json (decode-coding-string
                     (base64-decode-string encoded-request) 'utf-8))
              (request (json-parse-string json :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object :json-false)))
         `((ok . t) (result . ,(emacs-agent--dispatch request))))
     (emacs-agent-api-error
      `((ok . :json-false)
        (error . ((code . ,(nth 1 err))
                  (message . ,(nth 2 err))
                  (details . ,(nth 3 err))))))
     (error
      `((ok . :json-false)
        (error . ((code . "internal_error")
                  (message . ,(error-message-string err))
                  (details . nil))))))))

(provide 'emacs-agent)

;;; emacs-agent.el ends here
