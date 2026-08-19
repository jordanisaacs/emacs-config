;;; emacs-agent-bookmark.el --- Ephemeral agent recovery bookmarks -*- lexical-binding: t -*-

;;; Commentary:

;; A live coding agent gets one package-owned bookmark after both native
;; process evidence and a vendor session id are known.  The bookmark extends
;; Ghostel's normal record with only agent kind, session id, and last-seen
;; time.  It is deleted when the run ends and survives only daemon shutdown or
;; a crash, making its presence on the next startup an unambiguous resume
;; candidate.  The live tracker and sidebar remain entirely process-backed.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-track)

(declare-function ghostel-bookmark-handler "ghostel-bookmark" (bookmark))
(declare-function ghostel-bookmark-make-record "ghostel-bookmark" ())
(declare-function ghostel-paste-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional modifiers))
(defvar ghostel-command-start-functions)
(defvar ghostel-command-finish-functions)
(defvar ghostel-exit-functions)
(defvar pm-executable)
(defvar emacs-session-before-checkpoint-hook)
(declare-function emacs-session-state-writable-p "emacs-session" ())
(declare-function emacs-session-shutting-down-p "emacs-session" ())

(defgroup emacs-agent-bookmark nil
  "Crash recovery for agents running in Ghostel terminals."
  :group 'applications)

(defcustom emacs-agent-bookmark-prefix "emacs-agent/"
  "Reserved bookmark name prefix for package-owned recovery markers."
  :type 'string
  :group 'emacs-agent-bookmark)

(defcustom emacs-agent-bookmark-resume-delay 0.5
  "Seconds to wait for a restored Ghostel shell before submitting resume."
  :type 'number
  :group 'emacs-agent-bookmark)

(defcustom emacs-agent-bookmark-resume-timeout 60
  "Seconds allowed for an automatically resumed agent to become live."
  :type 'number
  :group 'emacs-agent-bookmark)

(defcustom emacs-agent-bookmark-stale-age (* 30 24 60 60)
  "Seconds after last native evidence before a recovery marker is stale."
  :type 'integer
  :group 'emacs-agent-bookmark)

(defcustom emacs-agent-bookmark-heartbeat-interval (* 5 60)
  "Seconds between last-seen refreshes for long-running live agents."
  :type 'integer
  :group 'emacs-agent-bookmark)

(defvar emacs-agent-bookmark--resume-done nil)
(defvar emacs-agent-bookmark--heartbeat-timer nil)
(defvar emacs-agent-bookmark--installed-p nil)
(defvar emacs-agent-bookmark-mode)

(defvar-local emacs-agent-bookmark--name nil)
(defvar-local emacs-agent-bookmark--kind nil)
(defvar-local emacs-agent-bookmark--session-id nil)
(defvar-local emacs-agent-bookmark--resume-pending-p nil)
(defvar-local emacs-agent-bookmark--resume-submitted-p nil)
(defvar-local emacs-agent-bookmark--resume-running-p nil)
(defvar-local emacs-agent-bookmark--resume-timer nil)
(defvar-local emacs-agent-bookmark--verification-timer nil)

(defconst emacs-agent-bookmark--session-id-regexp
  "\\`[[:alnum:]][[:alnum:].:_-]\\{0,255\\}\\'")

(defun emacs-agent-bookmark--now-ms ()
  "Return wall-clock time in milliseconds."
  (floor (* 1000 (float-time))))

(defun emacs-agent-bookmark--shutting-down-p ()
  "Return non-nil when session shutdown must preserve recovery markers."
  (and (fboundp 'emacs-session-shutting-down-p)
       (emacs-session-shutting-down-p)))

(defun emacs-agent-bookmark--state-writable-p ()
  "Return non-nil when recovery markers may be consumed or published."
  (or (not (fboundp 'emacs-session-state-writable-p))
      (emacs-session-state-writable-p)))

(defun emacs-agent-bookmark-name (kind session-id)
  "Return the managed bookmark name for KIND and SESSION-ID."
  (format "%s%s/%s" emacs-agent-bookmark-prefix kind session-id))

(defun emacs-agent-bookmark-managed-p (bookmark)
  "Return non-nil when BOOKMARK is in the package-owned namespace."
  (let ((name (if (stringp bookmark) bookmark (car-safe bookmark))))
    (and (stringp name)
         (string-prefix-p emacs-agent-bookmark-prefix name))))

(defun emacs-agent-bookmark--valid-session-id-p (session-id)
  "Return non-nil when SESSION-ID is safe to name and pass as one argv item."
  (and (stringp session-id)
       (string-match-p emacs-agent-bookmark--session-id-regexp session-id)))

(defun emacs-agent-bookmark--record-kind (bookmark)
  "Return BOOKMARK's supported agent kind."
  (let ((kind (bookmark-prop-get bookmark 'emacs-agent-kind)))
    (and (member kind '("claude" "codex" "cursor")) kind)))

(defun emacs-agent-bookmark--record-valid-p (bookmark)
  "Return non-nil when BOOKMARK is a well-formed managed recovery marker."
  (let* ((kind (emacs-agent-bookmark--record-kind bookmark))
         (session-id (bookmark-prop-get bookmark 'emacs-agent-session-id))
         (expected (and kind session-id
                        (emacs-agent-bookmark-name kind session-id)))
         (location (bookmark-prop-get bookmark 'location))
         (seen (bookmark-prop-get bookmark 'emacs-agent-last-seen-at)))
    (and (emacs-agent-bookmark-managed-p bookmark)
         (equal (car bookmark) expected)
         (eq (bookmark-prop-get bookmark 'handler)
             'emacs-agent-bookmark-handler)
         (emacs-agent-bookmark--valid-session-id-p session-id)
         (numberp seen)
         (stringp location)
         (not (file-remote-p location))
         (file-directory-p location))))

(defun emacs-agent-bookmark--live-record (kind session-id)
  "Return the live tracker record for KIND and SESSION-ID, if any."
  (seq-find
   (lambda (record)
     (and (equal kind (alist-get 'kind record))
          (equal session-id (alist-get 'vendor_session_id record))))
   (emacs-agent-track-sessions)))

(defun emacs-agent-bookmark-status (bookmark)
  "Classify managed BOOKMARK as `live', `resumable', or `stale'.

Completed and failed runs have no marker: their marker is synchronously
deleted and their history remains vendor/PM-owned."
  (if (not (emacs-agent-bookmark--record-valid-p bookmark)) 'stale
    (let* ((kind (bookmark-prop-get bookmark 'emacs-agent-kind))
           (session-id (bookmark-prop-get bookmark 'emacs-agent-session-id))
           (seen (/ (bookmark-prop-get bookmark 'emacs-agent-last-seen-at)
                    1000.0)))
      (cond
       ((emacs-agent-bookmark--live-record kind session-id) 'live)
       ((> (- (float-time) seen) emacs-agent-bookmark-stale-age) 'stale)
       (t 'resumable)))))

(defun emacs-agent-bookmark--safe-identity (identity)
  "Return the minimal non-executable subset of Ghostel IDENTITY."
  (when (listp identity)
    (delq nil
          (mapcar (lambda (key)
                    (when-let* ((pair (assq key identity)))
                      (copy-tree pair)))
                  '(kind name instance)))))

(defun emacs-agent-bookmark--make-record (record)
  "Derive a minimal Ghostel bookmark record from live tracker RECORD."
  (let* ((buffer-id (alist-get 'id record))
         (buffer (emacs-agent-track-buffer-for-id buffer-id))
         (kind (alist-get 'kind record))
         (session-id (alist-get 'vendor_session_id record)))
    (when (and buffer
               (member kind '("claude" "codex" "cursor"))
               (emacs-agent-bookmark--valid-session-id-p session-id)
               (buffer-local-value 'emacs-agent-track--native-fingerprint
                                   buffer))
      (require 'ghostel-bookmark)
      (with-current-buffer buffer
        (let* ((base (ghostel-bookmark-make-record))
               (properties (cdr base))
               (location (alist-get 'location properties))
               (buffer-name (alist-get 'buf-name properties))
               (identity (emacs-agent-bookmark--safe-identity
                          (alist-get 'identity properties))))
          ;; Construct a whitelist instead of copying Ghostel's record.  This
          ;; keeps future top-level properties out of the private state file.
          `(nil
            (handler . emacs-agent-bookmark-handler)
            (location . ,location)
            (buf-name . ,buffer-name)
            (identity . ,identity)
            (emacs-agent-kind . ,kind)
            (emacs-agent-session-id . ,session-id)
            (emacs-agent-last-seen-at . ,(emacs-agent-bookmark--now-ms))
            (defaults . nil)))))))

(defun emacs-agent-bookmark--attach-buffer (buffer name kind session-id)
  "Associate managed marker NAME/KIND/SESSION-ID with BUFFER."
  (with-current-buffer buffer
    (setq-local emacs-agent-bookmark--name name
                emacs-agent-bookmark--kind kind
                emacs-agent-bookmark--session-id session-id)
    (add-hook 'kill-buffer-hook #'emacs-agent-bookmark--buffer-killed nil t)))

(defun emacs-agent-bookmark--save ()
  "Synchronously persist the current bookmark universe."
  (bookmark-save))

(defun emacs-agent-bookmark--delete (name)
  "Delete managed marker NAME synchronously."
  (when (and (emacs-agent-bookmark-managed-p name)
             (bookmark-get-bookmark name 'noerror))
    (let ((bookmark-save-flag nil))
      (bookmark-delete name t))
    (emacs-agent-bookmark--save)
    ;; Ensure the newest standard Bookmark backup is a tombstone generation
    ;; too: recovery after future corruption must not resurrect a completed run.
    (emacs-agent-bookmark--save)))

(defun emacs-agent-bookmark--clear-buffer (&optional buffer)
  "Delete BUFFER's managed marker unless daemon shutdown is in progress."
  (with-current-buffer (or buffer (current-buffer))
    (when (timerp emacs-agent-bookmark--resume-timer)
      (cancel-timer emacs-agent-bookmark--resume-timer)
      (setq emacs-agent-bookmark--resume-timer nil))
    (when (timerp emacs-agent-bookmark--verification-timer)
      (cancel-timer emacs-agent-bookmark--verification-timer)
      (setq emacs-agent-bookmark--verification-timer nil))
    (unless (emacs-agent-bookmark--shutting-down-p)
      (when emacs-agent-bookmark--name
        (emacs-agent-bookmark--delete emacs-agent-bookmark--name))
      (setq emacs-agent-bookmark--name nil
            emacs-agent-bookmark--kind nil
            emacs-agent-bookmark--session-id nil
            emacs-agent-bookmark--resume-pending-p nil
            emacs-agent-bookmark--resume-submitted-p nil
            emacs-agent-bookmark--resume-running-p nil))))

(defun emacs-agent-bookmark--buffer-killed ()
  "Clean a managed marker when its Ghostel buffer is killed."
  (emacs-agent-bookmark--clear-buffer (current-buffer)))

(defun emacs-agent-bookmark-forget-buffer (buffer)
  "Synchronously forget BUFFER's recovery marker after an explicit stop."
  (when (buffer-live-p buffer)
    (emacs-agent-bookmark--clear-buffer buffer)))

(defun emacs-agent-bookmark--ghostel-exited (buffer _event)
  "Clean BUFFER's managed marker when its Ghostel shell exits."
  (when (buffer-live-p buffer)
    (emacs-agent-bookmark--clear-buffer buffer)))

(defun emacs-agent-bookmark--command-started (buffer)
  "Mark BUFFER's submitted resume command as running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when emacs-agent-bookmark--resume-submitted-p
        (setq emacs-agent-bookmark--resume-running-p t)))))

(defun emacs-agent-bookmark--command-finished (buffer _status)
  "Clean BUFFER's marker after its resumed agent command finishes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when emacs-agent-bookmark--resume-running-p
        (emacs-agent-bookmark--clear-buffer buffer)))))

(defun emacs-agent-bookmark--upsert-live-record (record)
  "Create or refresh the managed marker corresponding to live RECORD."
  (when-let* ((bookmark-record (emacs-agent-bookmark--make-record record))
              (kind (alist-get 'kind record))
              (session-id (alist-get 'vendor_session_id record))
              (name (emacs-agent-bookmark-name kind session-id))
              (buffer (emacs-agent-track-buffer-for-id (alist-get 'id record))))
    (unless (and (equal (buffer-local-value 'emacs-agent-bookmark--name buffer)
                        name)
                 (bookmark-get-bookmark name 'noerror))
      (when-let* ((old-name
                   (buffer-local-value 'emacs-agent-bookmark--name buffer))
                  ((not (equal old-name name))))
        (emacs-agent-bookmark--delete old-name))
      (let ((bookmark-save-flag nil))
        (bookmark-store name (cdr bookmark-record) nil))
      (emacs-agent-bookmark--attach-buffer buffer name kind session-id)
      (emacs-agent-bookmark--save))
    ;; Native identity is now confirmed.  Tracker removal may henceforth
    ;; clean the marker even on shells that do not emit OSC 133 finish marks.
    (with-current-buffer buffer
      (when (timerp emacs-agent-bookmark--resume-timer)
        (cancel-timer emacs-agent-bookmark--resume-timer))
      (when (timerp emacs-agent-bookmark--verification-timer)
        (cancel-timer emacs-agent-bookmark--verification-timer))
      (setq emacs-agent-bookmark--resume-timer nil
            emacs-agent-bookmark--verification-timer nil
            emacs-agent-bookmark--resume-pending-p nil
            emacs-agent-bookmark--resume-submitted-p nil))))

(defun emacs-agent-bookmark--record-for-buffer (buffer)
  "Return BUFFER's current live tracker record."
  (when-let* ((id (buffer-local-value 'emacs-agent-id buffer)))
    (gethash id emacs-agent-track--sessions)))

(defun emacs-agent-bookmark--reconcile ()
  "Reconcile recovery markers against the process-backed live registry."
  (unless (or (emacs-agent-bookmark--shutting-down-p)
              (not (emacs-agent-bookmark--state-writable-p)))
    (dolist (record (emacs-agent-track-sessions))
      (emacs-agent-bookmark--upsert-live-record record))
    (dolist (buffer (buffer-list))
      (when (buffer-local-value 'emacs-agent-bookmark--name buffer)
        (with-current-buffer buffer
          (unless (or (emacs-agent-bookmark--record-for-buffer buffer)
                      ;; An automatically submitted resume is allowed to wait
                      ;; for native evidence until its verification timeout.
                      emacs-agent-bookmark--resume-pending-p
                      emacs-agent-bookmark--resume-submitted-p)
            (emacs-agent-bookmark--clear-buffer buffer)))))))

(defun emacs-agent-bookmark--heartbeat ()
  "Refresh last-seen metadata for all confirmed live managed markers."
  (unless (or (emacs-agent-bookmark--shutting-down-p)
              (not (emacs-agent-bookmark--state-writable-p)))
    (let ((changed nil)
          (now (emacs-agent-bookmark--now-ms)))
      (dolist (record (emacs-agent-track-sessions))
        (emacs-agent-bookmark--upsert-live-record record)
        (let* ((kind (alist-get 'kind record))
               (session-id (alist-get 'vendor_session_id record))
               (name (and (emacs-agent-bookmark--valid-session-id-p session-id)
                          (emacs-agent-bookmark-name kind session-id)))
               (bookmark (and name (bookmark-get-bookmark name 'noerror))))
          (when bookmark
            (bookmark-prop-set bookmark 'emacs-agent-last-seen-at now)
            (setq changed t))))
      (when changed
        (cl-incf bookmark-alist-modification-count)
        (emacs-agent-bookmark--save)))))

(defun emacs-agent-bookmark--resume-argv (kind session-id)
  "Return the in-memory PM resume argv for KIND and SESSION-ID."
  (let ((pm (or (and (boundp 'pm-executable) pm-executable) "pm")))
    (if (equal kind "codex")
        (list pm "agent" "codex" "resume" session-id)
      (list pm "agent" kind "--resume" session-id))))

(defun emacs-agent-bookmark--resume-command (kind session-id)
  "Return a safely quoted shell command to resume KIND/SESSION-ID."
  (mapconcat #'shell-quote-argument
             (emacs-agent-bookmark--resume-argv kind session-id) " "))

(defun emacs-agent-bookmark--verification-timeout (buffer)
  "Delete BUFFER's candidate if no matching live run appeared in time."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq emacs-agent-bookmark--verification-timer nil)
      (unless (emacs-agent-bookmark--live-record
               emacs-agent-bookmark--kind emacs-agent-bookmark--session-id)
        (emacs-agent-bookmark--clear-buffer buffer)))))

;;;###autoload
(defun emacs-agent-bookmark-handler (bookmark)
  "Restore BOOKMARK's Ghostel shell and submit its vendor resume command."
  (unless (emacs-agent-bookmark-managed-p bookmark)
    (user-error "Not a managed agent recovery bookmark"))
  (unless (emacs-agent-bookmark--state-writable-p)
    (user-error "Agent recovery is owned by another live Emacs"))
  (let* ((status (emacs-agent-bookmark-status bookmark))
         (kind (bookmark-prop-get bookmark 'emacs-agent-kind))
         (session-id (bookmark-prop-get bookmark 'emacs-agent-session-id))
         (name (car bookmark))
         buffer)
    (if (eq status 'live)
        (progn
          (setq buffer
                (emacs-agent-track-buffer-for-id
                 (alist-get 'id
                            (emacs-agent-bookmark--live-record kind session-id))))
          (unless buffer (user-error "Live agent buffer disappeared")))
      (unless (eq status 'resumable)
        (user-error "Agent recovery bookmark is stale"))
      (require 'ghostel)
      (require 'ghostel-bookmark)
      (save-current-buffer
        (ghostel-bookmark-handler bookmark)
        (setq buffer (current-buffer)))
      (emacs-agent-bookmark--attach-buffer buffer name kind session-id)
      (with-current-buffer buffer
        (setq emacs-agent-bookmark--resume-pending-p t
              emacs-agent-bookmark--resume-timer
              (run-at-time
               emacs-agent-bookmark-resume-delay nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq emacs-agent-bookmark--resume-timer nil
                           emacs-agent-bookmark--resume-pending-p nil)
                     (condition-case err
                         (progn
                           (setq emacs-agent-bookmark--resume-submitted-p t)
                           (ghostel-paste-string
                            (emacs-agent-bookmark--resume-command
                             kind session-id))
                           (ghostel-send-key "return")
                           (setq emacs-agent-bookmark--verification-timer
                                 (run-at-time
                                  emacs-agent-bookmark-resume-timeout nil
                                  #'emacs-agent-bookmark--verification-timeout
                                  buffer)))
                       (error
                        (message "emacs-agent: resume submission failed: %s"
                                 (error-message-string err))
                        (emacs-agent-bookmark--clear-buffer buffer))))))))))
    (set-buffer buffer)))

(put 'emacs-agent-bookmark-handler 'bookmark-handler-type "Agent")

(defun emacs-agent-bookmark--auto-resume ()
  "Resume every valid package-owned candidate and clean stale markers."
  (bookmark-maybe-load-default-file)
  (if (not (emacs-agent-bookmark--state-writable-p))
      (message "emacs-agent: deferred recovery; session state has a live owner")
    (setq emacs-agent-bookmark--resume-done t)
    (dolist (bookmark (copy-sequence bookmark-alist))
      (when (emacs-agent-bookmark-managed-p bookmark)
        (pcase (emacs-agent-bookmark-status bookmark)
          ('resumable
           (condition-case err
               (save-current-buffer (emacs-agent-bookmark-handler bookmark))
             (error
              (message "emacs-agent: could not resume %s: %s"
                       (car bookmark) (error-message-string err)))))
          ('stale (emacs-agent-bookmark--delete (car bookmark))))))))

(defun emacs-agent-bookmark--install ()
  "Install agent recovery hooks and timers."
  (unless emacs-agent-bookmark--installed-p
    (condition-case err
        (progn
          (emacs-agent-track-setup)
          (add-hook 'emacs-agent-track-change-hook
                    #'emacs-agent-bookmark--reconcile)
          (add-hook 'ghostel-command-start-functions
                    #'emacs-agent-bookmark--command-started)
          (add-hook 'ghostel-command-finish-functions
                    #'emacs-agent-bookmark--command-finished)
          (add-hook 'ghostel-exit-functions
                    #'emacs-agent-bookmark--ghostel-exited)
          (when (boundp 'emacs-session-before-checkpoint-hook)
            (add-hook 'emacs-session-before-checkpoint-hook
                      #'emacs-agent-bookmark--heartbeat))
          (setq emacs-agent-bookmark--heartbeat-timer
                (run-with-timer emacs-agent-bookmark-heartbeat-interval
                                emacs-agent-bookmark-heartbeat-interval
                                #'emacs-agent-bookmark--heartbeat)
                emacs-agent-bookmark--installed-p t)
          (emacs-agent-bookmark--reconcile)
          (unless emacs-agent-bookmark--resume-done
            (if after-init-time
                (emacs-agent-bookmark--auto-resume)
              (add-hook 'emacs-startup-hook
                        #'emacs-agent-bookmark--auto-resume))))
      (error
       (setq emacs-agent-bookmark-mode nil)
       (emacs-agent-bookmark--uninstall)
       (signal (car err) (cdr err))))))

(defun emacs-agent-bookmark--uninstall ()
  "Remove agent recovery hooks and timers without deleting markers."
  (remove-hook 'emacs-agent-track-change-hook
               #'emacs-agent-bookmark--reconcile)
  (remove-hook 'ghostel-command-start-functions
               #'emacs-agent-bookmark--command-started)
  (remove-hook 'ghostel-command-finish-functions
               #'emacs-agent-bookmark--command-finished)
  (remove-hook 'ghostel-exit-functions #'emacs-agent-bookmark--ghostel-exited)
  (when (boundp 'emacs-session-before-checkpoint-hook)
    (remove-hook 'emacs-session-before-checkpoint-hook
                 #'emacs-agent-bookmark--heartbeat))
  (remove-hook 'emacs-startup-hook #'emacs-agent-bookmark--auto-resume)
  (when (timerp emacs-agent-bookmark--heartbeat-timer)
    (cancel-timer emacs-agent-bookmark--heartbeat-timer))
  (setq emacs-agent-bookmark--heartbeat-timer nil
        emacs-agent-bookmark--installed-p nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (timerp emacs-agent-bookmark--resume-timer)
        (cancel-timer emacs-agent-bookmark--resume-timer)
        (setq emacs-agent-bookmark--resume-timer nil))
      (when (timerp emacs-agent-bookmark--verification-timer)
        (cancel-timer emacs-agent-bookmark--verification-timer)
        (setq emacs-agent-bookmark--verification-timer nil))
      (remove-hook 'kill-buffer-hook
                   #'emacs-agent-bookmark--buffer-killed t)
      (dolist (variable '(emacs-agent-bookmark--name
                          emacs-agent-bookmark--kind
                          emacs-agent-bookmark--session-id
                          emacs-agent-bookmark--resume-pending-p
                          emacs-agent-bookmark--resume-submitted-p
                          emacs-agent-bookmark--resume-running-p
                          emacs-agent-bookmark--resume-timer
                          emacs-agent-bookmark--verification-timer))
        (kill-local-variable variable)))))

(with-eval-after-load 'emacs-session
  (when (bound-and-true-p emacs-agent-bookmark-mode)
    (add-hook 'emacs-session-before-checkpoint-hook
              #'emacs-agent-bookmark--heartbeat)))

;;;###autoload
(define-minor-mode emacs-agent-bookmark-mode
  "Persist and automatically resume live Ghostel agent sessions."
  :global t
  :group 'emacs-agent-bookmark
  (if emacs-agent-bookmark-mode
      (emacs-agent-bookmark--install)
    (emacs-agent-bookmark--uninstall)))

;;;###autoload
(defun emacs-agent-bookmark-setup ()
  "Enable `emacs-agent-bookmark-mode'."
  (interactive)
  (emacs-agent-bookmark-mode 1))

(provide 'emacs-agent-bookmark)

;;; emacs-agent-bookmark.el ends here
