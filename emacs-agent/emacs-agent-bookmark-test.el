;;; emacs-agent-bookmark-test.el --- Agent recovery tests -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-agent-bookmark)
(require 'emacs-agent)
(require 'emacs-agent-sidebar)

(defvar pm-executable)
(defvar emacs-session-before-checkpoint-hook nil)

(defmacro emacs-agent-bookmark-test--with-bookmarks (&rest body)
  "Run BODY with an isolated bookmark file and alist."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "emacs-agent-bookmark-test-" t))
          (bookmark-default-file (expand-file-name "bookmarks.el" directory))
          (bookmark-bookmarks-timestamp nil)
          (bookmark-alist nil)
          (bookmark-alist-modification-count 0)
          (bookmark-save-flag nil)
          (emacs-agent-bookmark--resume-done nil))
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun emacs-agent-bookmark-test--record (directory &optional age)
  "Return one managed Codex bookmark rooted at DIRECTORY and AGE seconds old."
  (let* ((session-id "01a00308-6ad2-7632-a6d4-1233d6aa67a5")
         (name (emacs-agent-bookmark-name "codex" session-id)))
    `(,name
      (handler . emacs-agent-bookmark-handler)
      (location . ,(file-name-as-directory directory))
      (buf-name . "*agent recovery*")
      (identity . ((kind . term) (name . "Ghostel") (instance . 3)))
      (emacs-agent-kind . "codex")
      (emacs-agent-session-id . ,session-id)
      (emacs-agent-last-seen-at . ,(floor (* 1000 (- (float-time) (or age 0)))))
      (defaults . nil))))

(ert-deftest emacs-agent-bookmark-derives-minimal-ghostel-record ()
  (skip-unless (require 'ghostel-bookmark nil t))
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq-local emacs-agent-id "buffer-one"
                  emacs-agent-track--native-fingerprint '(42 "start" "codex"))
      (let ((buffer (current-buffer))
            (record '((id . "buffer-one")
                      (kind . "codex")
                      (vendor_session_id . "01a00308-6ad2-7632-a6d4-1233d6aa67a5"))))
        (cl-letf (((symbol-function 'emacs-agent-track-buffer-for-id)
                   (lambda (_) buffer))
                  ((symbol-function 'ghostel-bookmark-make-record)
                   (lambda ()
                     '(nil
                       (handler . ghostel-bookmark-handler)
                       (location . "/tmp/")
                       (buf-name . "*agent*")
                       (future-sensitive-key . "private-top-level")
                       (identity . ((kind . term)
                                    (name . "*agent*")
                                    (project-root . "/private/project")
                                    (command . ("secret" "--token" "value"))
                                    (future-sensitive-key . "private")
                                    (instance . 9)))
                       (defaults . nil)))))
          (let* ((bookmark (emacs-agent-bookmark--make-record record))
                 (identity (bookmark-prop-get bookmark 'identity)))
            (should (eq (bookmark-prop-get bookmark 'handler)
                        'emacs-agent-bookmark-handler))
            (should-not (assq 'command identity))
            (should-not (assq 'future-sensitive-key identity))
            (should-not (bookmark-prop-get bookmark
                                           'future-sensitive-key))
            (should-not (assq 'project-root identity))
            (should (equal (alist-get 'name identity) "*agent*"))
            (should (equal (alist-get 'instance identity) 9))
            (should (equal (bookmark-prop-get bookmark 'emacs-agent-kind)
                           "codex"))
            (should-not (bookmark-prop-get bookmark 'project))
            (should-not (bookmark-prop-get bookmark 'title))))))))

(ert-deftest emacs-agent-bookmark-requires-native-id-evidence ()
  (with-temp-buffer
    (setq-local emacs-agent-id "buffer-two"
                emacs-agent-track--native-fingerprint nil)
    (let ((buffer (current-buffer)))
      (cl-letf (((symbol-function 'emacs-agent-track-buffer-for-id)
                 (lambda (_) buffer)))
        (should-not
         (emacs-agent-bookmark--make-record
          '((id . "buffer-two") (kind . "codex")
            (vendor_session_id . "01a00308-6ad2-7632-a6d4-1233d6aa67a5"))))))))

(ert-deftest emacs-agent-bookmark-reconcile-preserves-pending-resume ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
        cleared)
    (with-temp-buffer
      (setq-local emacs-agent-bookmark--name "emacs-agent/codex/session"
                  emacs-agent-bookmark--resume-pending-p t)
      (let ((buffer (current-buffer)))
        (cl-letf (((symbol-function 'emacs-agent-bookmark--shutting-down-p)
                   (lambda () nil))
                  ((symbol-function 'emacs-agent-bookmark--state-writable-p)
                   (lambda () t))
                  ((symbol-function 'emacs-agent-bookmark--clear-buffer)
                   (lambda (&optional candidate)
                     (push (or candidate (current-buffer)) cleared))))
          (emacs-agent-bookmark--reconcile)
          (should-not cleared)
          (setq emacs-agent-bookmark--resume-pending-p nil)
          (emacs-agent-bookmark--reconcile)
          (should (memq buffer cleared)))))))

(ert-deftest emacs-agent-bookmark-resume-command-is-kind-specific ()
  (let ((was-bound (boundp 'pm-executable))
        (old (and (boundp 'pm-executable) (symbol-value 'pm-executable))))
    (unwind-protect
        (progn
          (set 'pm-executable "/nix/store/pm/bin/pm")
          (should (equal
                   (emacs-agent-bookmark--resume-argv "codex" "session")
                   '("/nix/store/pm/bin/pm" "agent" "codex" "resume" "session")))
          (should (equal
                   (emacs-agent-bookmark--resume-argv "claude" "session")
                   '("/nix/store/pm/bin/pm" "agent" "claude" "--resume" "session")))
          (should (equal
                   (emacs-agent-bookmark--resume-argv "cursor" "session")
                   '("/nix/store/pm/bin/pm" "agent" "cursor" "--resume" "session"))))
      (if was-bound (set 'pm-executable old) (makunbound 'pm-executable)))))

(ert-deftest emacs-agent-bookmark-classifies-live-resumable-and-stale ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let* ((bookmark (emacs-agent-bookmark-test--record directory))
           (kind (bookmark-prop-get bookmark 'emacs-agent-kind))
           (session-id (bookmark-prop-get bookmark 'emacs-agent-session-id))
           (emacs-agent-track--sessions (make-hash-table :test 'equal)))
      (should (eq (emacs-agent-bookmark-status bookmark) 'resumable))
      (puthash "live" `((id . "live") (kind . ,kind)
                         (vendor_session_id . ,session-id))
               emacs-agent-track--sessions)
      (should (eq (emacs-agent-bookmark-status bookmark) 'live))
      (clrhash emacs-agent-track--sessions)
      (let ((old (emacs-agent-bookmark-test--record
                  directory (1+ emacs-agent-bookmark-stale-age))))
        (should (eq (emacs-agent-bookmark-status old) 'stale))))))

(ert-deftest emacs-agent-bookmark-normal-cleanup-but-shutdown-preserves ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let* ((bookmark (emacs-agent-bookmark-test--record directory))
           (name (car bookmark)))
      (setq bookmark-alist (list bookmark))
      (with-temp-buffer
        (setq-local emacs-agent-bookmark--name name)
        (cl-letf (((symbol-function 'emacs-session-shutting-down-p)
                   (lambda () nil)))
          (emacs-agent-bookmark--clear-buffer))
        (should-not (bookmark-get-bookmark name 'noerror))
        (setq bookmark-alist (list bookmark)
              emacs-agent-bookmark--name name)
        (cl-letf (((symbol-function 'emacs-session-shutting-down-p)
                   (lambda () t)))
          (emacs-agent-bookmark--clear-buffer))
        (should (assoc name bookmark-alist))))))

(ert-deftest emacs-agent-bookmark-auto-resume-ignores-user-bookmarks ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let* ((managed (emacs-agent-bookmark-test--record directory))
           (ordinary '("ordinary" (filename . "/tmp/file")))
           (bookmark-alist (list managed ordinary))
           resumed)
      (cl-letf (((symbol-function 'bookmark-maybe-load-default-file) #'ignore)
                ((symbol-function 'emacs-agent-bookmark-status)
                 (lambda (_) 'resumable))
                ((symbol-function 'emacs-agent-bookmark-handler)
                 (lambda (bookmark) (push (car bookmark) resumed))))
        (emacs-agent-bookmark--auto-resume)
        (should (equal resumed (list (car managed))))))))

(ert-deftest emacs-agent-bookmark-auto-resume-retains-transient-failure ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let* ((managed (emacs-agent-bookmark-test--record directory))
           (bookmark-alist (list managed)))
      (cl-letf (((symbol-function 'bookmark-maybe-load-default-file) #'ignore)
                ((symbol-function 'emacs-agent-bookmark-status)
                 (lambda (_) 'resumable))
                ((symbol-function 'emacs-agent-bookmark-handler)
                 (lambda (_) (error "temporary spawn failure"))))
        (emacs-agent-bookmark--auto-resume)
        (should (assoc (car managed) bookmark-alist))))))

(ert-deftest emacs-agent-bookmark-defers-when-session-has-live-owner ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let* ((managed (emacs-agent-bookmark-test--record directory))
           (bookmark-alist (list managed))
           resumed)
      (cl-letf (((symbol-function 'bookmark-maybe-load-default-file) #'ignore)
                ((symbol-function 'emacs-session-state-writable-p)
                 (lambda () nil))
                ((symbol-function 'emacs-agent-bookmark-handler)
                 (lambda (_) (setq resumed t))))
        (emacs-agent-bookmark--auto-resume)
        (should-not resumed)
        (should-not emacs-agent-bookmark--resume-done)
        (should (assoc (car managed) bookmark-alist))))))

(ert-deftest emacs-agent-bookmarks-never-populate-live-sidebar-registry ()
  (emacs-agent-bookmark-test--with-bookmarks
    (let ((bookmark-alist
           (list (emacs-agent-bookmark-test--record directory)))
          (emacs-agent-track--sessions (make-hash-table :test 'equal)))
      (should-not (emacs-agent-track-sessions))
      (should-not (emacs-agent-sidebar--groups))
      (should (equal (alist-get 'agents (emacs-agent--list)) [])))))

(ert-deftest emacs-agent-bookmark-mode-installs-and-removes-owned-hooks ()
  (let ((emacs-agent-bookmark-mode nil)
        (emacs-agent-bookmark--installed-p nil)
        (emacs-agent-bookmark--heartbeat-timer nil)
        (emacs-agent-bookmark--resume-done t)
        (emacs-agent-track-change-hook nil)
        (ghostel-command-start-functions nil)
        (ghostel-command-finish-functions nil)
        (ghostel-exit-functions nil)
        (emacs-session-before-checkpoint-hook nil)
        (emacs-startup-hook nil))
    (cl-letf (((symbol-function 'emacs-agent-track-setup) #'ignore)
              ((symbol-function 'emacs-agent-bookmark--reconcile) #'ignore))
      (with-temp-buffer
        (emacs-agent-bookmark--attach-buffer
         (current-buffer) "emacs-agent/codex/session" "codex" "session")
        (setq-local emacs-agent-bookmark--resume-pending-p t
                    emacs-agent-bookmark--resume-timer
                    (run-at-time 3600 nil #'ignore))
        (unwind-protect
            (progn
              (emacs-agent-bookmark-mode 1)
              (should emacs-agent-bookmark--installed-p)
              (should (timerp emacs-agent-bookmark--heartbeat-timer))
              (should (memq #'emacs-agent-bookmark--reconcile
                            emacs-agent-track-change-hook))
              (should (memq #'emacs-agent-bookmark--heartbeat
                            emacs-session-before-checkpoint-hook))
              (emacs-agent-bookmark-mode -1)
              (should-not emacs-agent-bookmark--installed-p)
              (should-not emacs-agent-bookmark--heartbeat-timer)
              (should-not (memq #'emacs-agent-bookmark--reconcile
                                emacs-agent-track-change-hook))
              (should-not (local-variable-p
                           'emacs-agent-bookmark--name))
              (should-not (local-variable-p
                           'emacs-agent-bookmark--resume-timer)))
          (when emacs-agent-bookmark-mode
            (emacs-agent-bookmark-mode -1)))))))

(ert-deftest emacs-agent-bookmark-mode-cleans-up-failed-install ()
  (let ((emacs-agent-bookmark-mode nil)
        (emacs-agent-bookmark--installed-p nil)
        (emacs-agent-bookmark--heartbeat-timer nil)
        (emacs-agent-track-change-hook nil)
        (ghostel-command-start-functions nil)
        (ghostel-command-finish-functions nil)
        (ghostel-exit-functions nil)
        (emacs-session-before-checkpoint-hook nil)
        (emacs-startup-hook nil))
    (cl-letf (((symbol-function 'emacs-agent-track-setup) #'ignore)
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) (error "simulated timer failure"))))
      (should-error (emacs-agent-bookmark-mode 1)))
    (should-not emacs-agent-bookmark-mode)
    (should-not emacs-agent-bookmark--installed-p)
    (should-not emacs-agent-bookmark--heartbeat-timer)
    (should-not (memq #'emacs-agent-bookmark--reconcile
                      emacs-agent-track-change-hook))
    (should-not (memq #'emacs-agent-bookmark--heartbeat
                      emacs-session-before-checkpoint-hook))))

(provide 'emacs-agent-bookmark-test)

;;; emacs-agent-bookmark-test.el ends here
