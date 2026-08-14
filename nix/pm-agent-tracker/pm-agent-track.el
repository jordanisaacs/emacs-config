;;; pm-agent-track.el --- Local Ghostel agent registry -*- lexical-binding: t -*-

;;; Commentary:

;; Foreground process groups determine whether an agent is alive; Ghostel's
;; native active screen and OSC evidence determine state.  Agent hooks attach
;; identity only.  This keeps tracking local to Emacs and makes hidden Ghostel
;; buffers just as observable as displayed ones.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'pm-agent-rules)
(require 'pm-commands)
(require 'pm-project)

(declare-function ghostel-active-screen-text "ghostel" ())
(declare-function ghostel-terminal-title "ghostel" ())
(declare-function ghostel-terminal-progress "ghostel" ())
(defvar ghostel-terminal-update-generation)
(defvar ghostel-terminal-update-hook)
(defvar ghostel--pid)
(defvar pm-agent-buffer-id)
(defvar server-name)

(defgroup pm-agent-track nil
  "Local coding-agent state derived from Ghostel terminals."
  :group 'pm)

(defcustom pm-agent-track-process-interval 0.3
  "Seconds between foreground-process scans."
  :type 'number :group 'pm-agent-track)

(defcustom pm-agent-track-update-delay 0.075
  "Seconds used to coalesce bursts of terminal updates."
  :type 'number :group 'pm-agent-track)

(defvar pm-agent-track-change-hook nil
  "Hook run after the local registry changes.")
(defvar pm-agent-track--sessions (make-hash-table :test 'equal))
(defvar pm-agent-track--process-timer nil)
(defvar pm-agent-track--title-timers (make-hash-table :test 'equal))
(defvar pm-agent-track--setup-done nil)

(defvar-local pm-agent-track--identity nil)
(defvar-local pm-agent-track--update-timer nil)
(defvar-local pm-agent-track--idle-timer nil)
(defvar-local pm-agent-track--idle-start nil)
(defvar-local pm-agent-track--idle-confirmations 0)
(defvar-local pm-agent-track--last-generation -1)
(defvar-local pm-agent-track--last-foreground nil)
(defvar-local pm-agent-track--last-activity-at nil)

(defconst pm-agent-track--idle-delay 0.1)
(defconst pm-agent-track--idle-required 3)
(defconst pm-agent-track--idle-cap 0.7)

(defun pm-agent-track-sessions ()
  "Return live local sessions as a list of alists."
  (let (sessions)
    (maphash (lambda (_key value) (push value sessions)) pm-agent-track--sessions)
    sessions))

(defun pm-agent-track-buffer-for-id (buffer-id)
  "Return the live buffer identified by BUFFER-ID."
  (and buffer-id
       (seq-find (lambda (buffer)
                   (equal (buffer-local-value 'pm-agent-buffer-id buffer) buffer-id))
                 (buffer-list))))

(defun pm-agent-track--notify ()
  "Notify registry consumers."
  (run-hooks 'pm-agent-track-change-hook))

(defun pm-agent-track--now-ms ()
  "Return wall-clock time in milliseconds."
  (floor (* 1000 (float-time))))

(defun pm-agent-track--process-record (pid)
  "Return the process fields needed for PID, or nil if it exited."
  (when-let* ((attributes (process-attributes pid)))
    (list :pid pid
          :ppid (alist-get 'ppid attributes)
          :pgrp (alist-get 'pgrp attributes)
          :tpgid (alist-get 'tpgid attributes)
          :comm (or (alist-get 'comm attributes) "")
          :cmdline (or (alist-get 'args attributes) ""))))

(defun pm-agent-track--foreground-processes (shell-pid)
  "Return SHELL-PID's foreground process-group leader.

The foreground group id is also its leader's pid.  Agent launch wrappers put
the agent name in that leader's command line, so two targeted native process
lookups provide the lifecycle and classification evidence we need.  Avoid a
global /proc sweep here: on process-heavy hosts it can starve Emacs's event
loop before the first frame becomes interactive."
  (when-let* ((shell (pm-agent-track--process-record shell-pid)))
    (let ((tpgid (plist-get shell :tpgid)))
      (when (and (> tpgid 0) (/= tpgid (plist-get shell :pgrp)))
        (when-let* ((leader (pm-agent-track--process-record tpgid)))
          (list leader))))))

(defun pm-agent-track--record-agent (record)
  "Classify one process RECORD as an agent, if possible."
  (let ((comm (downcase (plist-get record :comm)))
        (text (downcase (plist-get record :cmdline))))
    (cond
     ((or (string= comm "agent") (string-match-p "cursor-agent\\|cursor.*/agent" text))
      "cursor")
     ((or (string-match-p "claude" comm)
          (string-match-p "claude-code\\|@anthropic-ai/claude\\|[/ ]claude\\(?:[ /.-]\\|$\\)" text))
      "claude")
     ((or (string= comm "codex")
          (string-match-p "[/ ]codex\\(?:[ /.-]\\|$\\)\\|openai.*codex" text))
      "codex")
     (t nil))))

(defun pm-agent-track--classify-processes (records)
  "Classify agent wrappers and descendants in RECORDS."
  (seq-some #'pm-agent-track--record-agent records))

(defun pm-agent-track--foreground-label (records)
  "Return a concise activity label for foreground RECORDS."
  (when records
    (plist-get (car (sort (copy-sequence records)
                          (lambda (a b) (< (plist-get a :pid) (plist-get b :pid)))))
               :comm)))

(defun pm-agent-track--project (identity)
  "Resolve a pm project name for IDENTITY or the current buffer."
  (let* ((cwd (or (alist-get 'cwd identity) default-directory))
         (container (and cwd (pm--container-of cwd))))
    (and container (file-name-nondirectory (directory-file-name container)))))

(defun pm-agent-track--progress-text ()
  "Return current Ghostel OSC progress using Herdr's numeric representation."
  (when-let* ((progress (ghostel-terminal-progress)))
    (format "%d;%s"
            (pcase (car progress)
              ('remove 0) ('set 1) ('error 2) ('indeterminate 3) ('pause 4) (_ -1))
            (or (cdr progress) ""))))

(defun pm-agent-track--selected-p ()
  "Return non-nil when the current buffer is selected."
  (eq (current-buffer) (window-buffer (selected-window))))

(defun pm-agent-track--token (agent session-id)
  "Build a stable identity token from AGENT and SESSION-ID."
  (concat (or agent "") "\0" (or session-id "")))

(defun pm-agent-track--flatten-groups (groups)
  "Flatten sectioned `pm agent ls' GROUPS."
  (apply #'append (mapcar (lambda (group) (alist-get 'sessions group)) groups)))

(defun pm-agent-track--schedule-title (key token attempt)
  "Schedule transcript title lookup ATTEMPT for KEY and TOKEN."
  (when (< attempt 3)
    (when-let* ((old (gethash key pm-agent-track--title-timers)))
      (when (timerp old) (cancel-timer old)))
    (puthash
     key
     (run-at-time
      (nth attempt '(0.5 1.0 2.0)) nil
      (lambda ()
        (remhash key pm-agent-track--title-timers)
        (when-let* ((record (gethash key pm-agent-track--sessions)))
          (when (equal token (pm-agent-track--token
                              (alist-get 'agent record)
                              (alist-get 'vendor_session_id record)))
            (let ((project (alist-get 'project record))
                  (agent (alist-get 'agent record)))
              (when (and project agent)
                (pm--run-async
                 (list "agent" "ls" "--project" project "--agent" agent
                       "--limit" "50" "--json")
                 (lambda (groups)
                   (pm-agent-track--title-result key token groups attempt))
                 :on-error (lambda (&rest _)
                             (pm-agent-track--schedule-title
                              key token (1+ attempt))))))))))
     pm-agent-track--title-timers)))

(defun pm-agent-track--title-result (key token groups attempt)
  "Apply title lookup GROUPS for KEY and TOKEN, or retry ATTEMPT."
  (when-let* ((record (gethash key pm-agent-track--sessions)))
    (when (equal token (pm-agent-track--token
                        (alist-get 'agent record)
                        (alist-get 'vendor_session_id record)))
      (let ((row (seq-find
                  (lambda (candidate)
                    (and (equal (alist-get 'agent candidate) (alist-get 'agent record))
                         (equal (alist-get 'session_id candidate)
                                (alist-get 'vendor_session_id record))))
                  (pm-agent-track--flatten-groups groups))))
        (if (and row (not (string-empty-p (or (alist-get 'title row) ""))))
            (progn
              (setf (alist-get 'title record) (alist-get 'title row))
              (puthash key record pm-agent-track--sessions)
              (pm-agent-track--notify))
          (pm-agent-track--schedule-title key token (1+ attempt)))))))

(defun pm-agent-track--clear-idle ()
  "Clear the current buffer's pending idle confirmation."
  (when (timerp pm-agent-track--idle-timer) (cancel-timer pm-agent-track--idle-timer))
  (setq pm-agent-track--idle-timer nil pm-agent-track--idle-start nil
        pm-agent-track--idle-confirmations 0))

(defun pm-agent-track--schedule-idle ()
  "Schedule the next 100ms idle confirmation."
  (unless (timerp pm-agent-track--idle-timer)
    (let ((buffer (current-buffer)))
      (setq pm-agent-track--idle-timer
            (run-at-time
             pm-agent-track--idle-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq pm-agent-track--idle-timer nil)
                   (pm-agent-track--scan-buffer t t)))))))))

(defun pm-agent-track--remove (key &optional clear-identity)
  "Remove registry KEY and optionally CLEAR-IDENTITY in the current buffer."
  (pm-agent-track--clear-idle)
  (when clear-identity (setq pm-agent-track--identity nil))
  (when-let* ((timer (gethash key pm-agent-track--title-timers)))
    (when (timerp timer) (cancel-timer timer))
    (remhash key pm-agent-track--title-timers))
  (when (remhash key pm-agent-track--sessions) (pm-agent-track--notify)))

(defun pm-agent-track--fallback-title (agent project)
  "Return a temporary title for AGENT and PROJECT."
  (if project (format "%s — %s" agent project) agent))

(defun pm-agent-track--publish (key agent detection foreground)
  "Publish DETECTION for AGENT and FOREGROUND under KEY."
  (let* ((existing (gethash key pm-agent-track--sessions))
         (identity pm-agent-track--identity)
         (session-id (or (alist-get 'session_id identity) ""))
         (project (pm-agent-track--project identity))
         (semantic (plist-get detection :state))
         (previous (alist-get 'semantic_status existing))
         (same-session (and existing (equal (alist-get 'agent existing) agent)
                            (equal (alist-get 'vendor_session_id existing) session-id)))
         (status (cond
                  ((not (equal semantic "idle")) semantic)
                  ((and same-session (equal (alist-get 'status existing) "done")) "done")
                  ((or (null existing) (equal previous "idle") (pm-agent-track--selected-p))
                   "idle")
                  (t "done")))
         (title (if same-session (alist-get 'title existing)
                  (pm-agent-track--fallback-title agent project)))
         (record `((buffer_id . ,key) (agent . ,agent)
                   (vendor_session_id . ,session-id) (project . ,project)
                   (title . ,title) (status . ,status)
                   (semantic_status . ,semantic)
                   (rule_id . ,(plist-get detection :rule-id))
                   (activity . ,foreground)
                   (last_activity_at . ,(or pm-agent-track--last-activity-at
                                            (pm-agent-track--now-ms))))))
    (puthash key record pm-agent-track--sessions)
    (when (and (not same-session) (not (string-empty-p session-id)))
      (pm-agent-track--schedule-title key (pm-agent-track--token agent session-id) 0))
    (unless (equal existing record) (pm-agent-track--notify))))

(defun pm-agent-track--accept (key agent detection foreground confirmation)
  "Stabilize DETECTION for KEY before publishing it."
  (if (plist-get detection :skip-state-update)
      (unless (gethash key pm-agent-track--sessions)
        (pm-agent-track--publish key agent '(:state "idle" :rule-id nil) foreground))
    (let* ((existing (gethash key pm-agent-track--sessions))
           (previous (alist-get 'semantic_status existing))
           (next (plist-get detection :state))
           (plain-idle (and (equal previous "working") (equal next "idle")
                            (not (plist-get detection :visible-idle)))))
      (cond
       ((not plain-idle)
        (pm-agent-track--clear-idle)
        (pm-agent-track--publish key agent detection foreground))
       ((null pm-agent-track--idle-start)
        (setq pm-agent-track--idle-start (float-time)
              pm-agent-track--idle-confirmations 0)
        (pm-agent-track--schedule-idle))
       ((not confirmation) nil)
       ((or (>= (- (float-time) pm-agent-track--idle-start) pm-agent-track--idle-cap)
            (>= (cl-incf pm-agent-track--idle-confirmations) pm-agent-track--idle-required))
        (pm-agent-track--clear-idle)
        (pm-agent-track--publish key agent detection foreground))
       (t (pm-agent-track--schedule-idle))))))

(defun pm-agent-track--scan-buffer (&optional force confirmation)
  "Scan the current Ghostel buffer's foreground process."
  (when (and (derived-mode-p 'ghostel-mode) (not (file-remote-p default-directory)))
    (let* ((key (bound-and-true-p pm-agent-buffer-id))
           (pid (bound-and-true-p ghostel--pid))
           (foreground (and pid (pm-agent-track--foreground-processes pid))))
      (when key
        (if (null foreground)
            (progn (setq pm-agent-track--last-foreground nil)
                   (pm-agent-track--remove key t))
          (let* ((agent (or (alist-get 'agent pm-agent-track--identity)
                            (pm-agent-track--classify-processes foreground)))
                 (generation (or (bound-and-true-p ghostel-terminal-update-generation) 0))
                 (foreground-id (sort (mapcar (lambda (record) (plist-get record :pid))
                                              foreground) #'<))
                 (changed (not (equal foreground-id pm-agent-track--last-foreground))))
            (setq pm-agent-track--last-foreground foreground-id)
            (if (null agent)
                (pm-agent-track--remove key nil)
              (when (or force changed (/= generation pm-agent-track--last-generation)
                        (null (gethash key pm-agent-track--sessions)))
                (setq pm-agent-track--last-generation generation)
                (pm-agent-track--accept
                 key agent
                 (pm-agent-rules-detect agent (ghostel-active-screen-text)
                                        (ghostel-terminal-title)
                                        (pm-agent-track--progress-text))
                 (pm-agent-track--foreground-label foreground) confirmation)))))))))

(defun pm-agent-track--process-tick ()
  "Refresh every Ghostel buffer's foreground process group."
  (condition-case err
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'ghostel-mode)
            (pm-agent-track--scan-buffer))))
    (error (message "pm agent tracker: process scan failed: %s"
                    (error-message-string err)))))

(defun pm-agent-track--terminal-updated ()
  "Coalesce a Ghostel terminal update into a state scan."
  (setq pm-agent-track--last-activity-at (pm-agent-track--now-ms))
  (unless (timerp pm-agent-track--update-timer)
    (let ((buffer (current-buffer)))
      (setq pm-agent-track--update-timer
            (run-at-time
             pm-agent-track-update-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq pm-agent-track--update-timer nil)
                   (pm-agent-track--scan-buffer t)))))))))

(defun pm-agent-track--buffer-killed ()
  "Remove current Ghostel buffer from the registry."
  (when-let* ((key (bound-and-true-p pm-agent-buffer-id)))
    (when (timerp pm-agent-track--update-timer) (cancel-timer pm-agent-track--update-timer))
    (pm-agent-track--remove key t)))

(defun pm-agent-track--ghostel-buffer-setup ()
  "Install buffer-local tracking cleanup."
  (add-hook 'kill-buffer-hook #'pm-agent-track--buffer-killed nil t))

(defun pm-agent-track-mark-seen (buffer-id)
  "Change BUFFER-ID's done state back to idle."
  (when-let* ((record (gethash buffer-id pm-agent-track--sessions)))
    (when (equal (alist-get 'status record) "done")
      (setf (alist-get 'status record) "idle")
      (puthash buffer-id record pm-agent-track--sessions)
      (pm-agent-track--notify))))

(defun pm-agent-track--selected-buffer-changed ()
  "Mark a selected Ghostel completion as seen."
  (when-let* ((buffer (window-buffer (selected-window)))
              (key (buffer-local-value 'pm-agent-buffer-id buffer)))
    (pm-agent-track-mark-seen key)))

(defun pm-agent-track--server-name ()
  "Return this Emacs server's socket name."
  (if (and (boundp 'server-name) (stringp server-name)) server-name "server"))

(defun pm-agent-track--seed-server ()
  "Inject this Emacs server name into a Ghostel child environment."
  (setenv "PM_META_SERVER" (pm-agent-track--server-name)))

(defun pm-agent-track--seed-server-filter (environment)
  "Add this Emacs server to an agent ENVIRONMENT list."
  (cons (format "PM_META_SERVER=%s" (pm-agent-track--server-name))
        (seq-remove (lambda (entry) (string-prefix-p "PM_META_SERVER=" entry)) environment)))

;;;###autoload
(defun pm-agent-track-identity (encoded-json)
  "Attach base64 ENCODED-JSON identity to its live Ghostel buffer."
  (condition-case err
      (let* ((json (decode-coding-string (base64-decode-string encoded-json) 'utf-8))
             (identity (json-parse-string json :object-type 'alist :array-type 'list
                                          :null-object nil :false-object nil))
             (buffer (pm-agent-track-buffer-for-id (alist-get 'buffer_id identity))))
        (when (and buffer (member (alist-get 'agent identity) '("claude" "codex" "cursor")))
          (with-current-buffer buffer
            ;; Identity without native screen evidence is deliberately ignored.
            (when (derived-mode-p 'ghostel-mode)
              (setq pm-agent-track--identity identity)
              (pm-agent-track--scan-buffer t)))))
    (error (message "pm agent tracker: ignored malformed identity: %s"
                    (error-message-string err)))))

;;;###autoload
(defun pm-agent-track-setup ()
  "Start local Ghostel agent tracking."
  (unless pm-agent-track--setup-done
    (setq pm-agent-track--setup-done t)
    (add-hook 'ghostel-mode-hook #'pm-agent-track--ghostel-buffer-setup)
    (add-hook 'ghostel-pre-spawn-hook #'pm-agent-track--seed-server)
    (add-hook 'ghostel-terminal-update-hook #'pm-agent-track--terminal-updated)
    (add-hook 'buffer-list-update-hook #'pm-agent-track--selected-buffer-changed)
    (advice-add 'pm-agent--seed-environment :filter-return
                #'pm-agent-track--seed-server-filter)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'ghostel-mode) (pm-agent-track--ghostel-buffer-setup))))
    (setq pm-agent-track--process-timer
          (run-at-time 0 pm-agent-track-process-interval #'pm-agent-track--process-tick))))

(provide 'pm-agent-track)

;;; pm-agent-track.el ends here
