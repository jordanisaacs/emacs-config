;;; emacs-session-test.el --- Tests for daemon persistence -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-session)

(defmacro emacs-session-test--with-directory (&rest body)
  "Run BODY with an isolated owner-only session directory."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "emacs-session-test-" t))
          (emacs-session-directory (file-name-as-directory directory)))
     (set-file-modes directory #o700)
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun emacs-session-test--plain-validator (file)
  "Accept an owner-only FILE whose contents begin with `valid:'."
  (emacs-session-storage-assert-safe-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (unless (looking-at-p "valid:")
      (error "invalid test generation")))
  t)

(defun emacs-session-test--write-text (file text)
  "Write TEXT to FILE for a storage-layer test."
  (with-temp-file file (insert text)))

(ert-deftest emacs-session-storage-rotates-and-recovers-last-good ()
  (emacs-session-test--with-directory
    (let ((target (expand-file-name "state.el" directory)))
      (emacs-session-storage-write
       target (lambda (file) (emacs-session-test--write-text file "valid:one"))
       #'emacs-session-test--plain-validator)
      (emacs-session-storage-write
       target (lambda (file) (emacs-session-test--write-text file "valid:two"))
       #'emacs-session-test--plain-validator)
      (should (equal (with-temp-buffer
                       (insert-file-contents (concat target ".last-good"))
                       (buffer-string))
                     "valid:one"))
      (with-temp-file target (insert "truncated"))
      (set-file-modes target #o600)
      (should (equal (emacs-session-storage-recover
                      target #'emacs-session-test--plain-validator)
                     target))
      (should (equal (with-temp-buffer
                       (insert-file-contents target)
                       (buffer-string))
                     "valid:one"))
      (should (directory-files directory nil
                               "state\\.el\\.corrupt\\.")))))

(ert-deftest emacs-session-storage-rejects-permissive-state ()
  (emacs-session-test--with-directory
    (let ((file (expand-file-name "unsafe" directory)))
      (with-temp-file file (insert "valid:data"))
      (set-file-modes file #o644)
      (should-error (emacs-session-storage-assert-safe-file file)))))

(ert-deftest emacs-session-imports-legacy-bookmarks-and-savehist-once ()
  (emacs-session-test--with-directory
    (let* ((legacy-bookmarks (expand-file-name "legacy-bookmarks" directory))
           (legacy-history (expand-file-name "legacy-history" directory))
           (emacs-session-directory
            (file-name-as-directory (expand-file-name "managed" directory))))
      (with-temp-file legacy-bookmarks
        (insert ";;; Emacs Bookmark Format Version 1\n"
                "((\"legacy\" (filename . \"/tmp/legacy\")))\n"))
      (with-temp-file legacy-history
        (insert ";; Minibuffer history file\n"
                "(setq minibuffer-history '(\"legacy\"))\n"))
      ;; Legacy Emacs files may predate the new owner-only policy.  Import is
      ;; allowed from an owned regular file, but the managed copy is 0600.
      (set-file-modes legacy-bookmarks #o644)
      (set-file-modes legacy-history #o644)
      (emacs-session--import-legacy-state legacy-bookmarks legacy-history)
      (should (emacs-session-storage-validate-bookmarks
               (emacs-session--file "bookmarks.el")))
      (should (emacs-session-storage-validate-savehist
               (emacs-session--file "savehist.el")))
      (should (zerop (logand (file-modes
                              (emacs-session--file "bookmarks.el"))
                             #o077)))
      (should (zerop (logand (file-modes
                              (emacs-session--file "savehist.el"))
                             #o077)))
      (with-temp-file legacy-history
        (insert ";; Minibuffer history file\n"
                "(setq minibuffer-history '(\"replacement\"))\n"))
      (set-file-modes legacy-history #o600)
      (emacs-session--import-legacy-state legacy-bookmarks legacy-history)
      (should (string-match-p
               "legacy"
               (emacs-session-storage--file-contents
                (emacs-session--file "savehist.el")))))))

(ert-deftest emacs-session-bookmark-bootstrap-uses-upstream-writer ()
  (emacs-session-test--with-directory
    (let ((bookmark-alist nil)
          (bookmark-bookmarks-timestamp nil)
          (bookmark-alist-modification-count 0))
      (emacs-session--load-bookmarks)
      (let ((file (emacs-session--file "bookmarks.el")))
        (should (emacs-session-storage-validate-bookmarks file))
        (should (zerop (logand (file-modes file) #o077)))))))

(ert-deftest emacs-session-desktop-restore-waits-for-client-hook ()
  (emacs-session-test--with-directory
    (let ((emacs-session-mode t)
          (emacs-session--desktop-restored-p nil)
          (emacs-session--desktop-restore-failed-p nil)
          (emacs-session--pending-gui-frameset nil)
          (emacs-session-after-restore-hook nil)
          (reads 0)
          (mru-restores 0)
          (mode-enabled nil))
      ;; Merely configuring the state above performs no Desktop read.  The
      ;; first invocation represents `server-after-make-frame-hook'.
      (should (= reads 0))
      (cl-letf (((symbol-function 'frame-initial-p) (lambda (_) nil))
                ((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
                ((symbol-function 'emacs-session-frames-prepare) #'ignore)
                ((symbol-function 'desktop-read)
                 (lambda (&optional _) (cl-incf reads) t))
                ((symbol-function 'emacs-session-frames-prune) #'ignore)
                ((symbol-function 'emacs-session--sanitize-restored-buffers) #'ignore)
                ((symbol-function 'emacs-session-frames-restore-mru)
                 (lambda (_) (cl-incf mru-restores) t))
                ((symbol-function 'desktop-save-mode)
                 (lambda (value) (setq mode-enabled value))))
        (emacs-session--server-after-make-frame)
        (should (= reads 1))
        (should (= mru-restores 1))
        (should (= mode-enabled 1))
        ;; Further clients consume MRU slots, but never reload Desktop.
        (emacs-session--server-after-make-frame)
        (should (= reads 1))
        (should (= mru-restores 2))))))

(ert-deftest emacs-session-desktop-read-hands-startup-lease-to-upstream ()
  (emacs-session-test--with-directory
    (let ((desktop-base-lock-name "session.lease")
          (desktop-dirname emacs-session-directory)
          (reads 0))
      (desktop-claim-lock emacs-session-directory)
      (set-file-modes (emacs-session--file "session.lease") #o600)
      (cl-letf (((symbol-function 'desktop-read)
                 (lambda (&optional _)
                   (should-not
                    (file-exists-p
                     (emacs-session--file "session.lease")))
                   (cl-incf reads)
                   t)))
        (should (emacs-session--desktop-read-with-fallback)))
      (should (= reads 1)))))

(ert-deftest emacs-session-frame-mru-is-type-specific-and-consumed ()
  (let ((emacs-session--state (emacs-session-frames--empty-state))
        (emacs-session--restoring-p nil)
        (frame (selected-frame)))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-initial-p) (lambda (_) nil))
                  ((symbol-function 'display-graphic-p)
                   (lambda (&optional _) nil)))
          (set-frame-parameter frame 'emacs-session-captured nil)
          (emacs-session-frames-snapshot frame)
          (should (= (length (plist-get emacs-session--state :active-tty)) 1))
          (emacs-session-frames-capture frame)
          (should-not (plist-get emacs-session--state :active-tty))
          (should (= (length
                      (alist-get 'tty
                                 (plist-get emacs-session--state :frame-slots)))
                     1))
          (should (emacs-session-frames-restore-mru frame))
          (should-not
           (alist-get 'tty (plist-get emacs-session--state :frame-slots))))
      (set-frame-parameter frame 'emacs-session-captured nil)
      (set-frame-parameter frame 'emacs-session-frame-id nil)
      (set-frame-parameter frame 'desktop-dont-save nil))))

(ert-deftest emacs-session-frame-empty-state-is-fresh ()
  (let ((first (emacs-session-frames--empty-state))
        (second (emacs-session-frames--empty-state)))
    (setf (alist-get 'tty (plist-get first :frame-slots)) '((snapshot)))
    (should-not (eq first second))
    (should-not (alist-get 'tty (plist-get second :frame-slots)))))

(ert-deftest emacs-session-frame-preparation-preserves-user-exclusion ()
  (let* ((frame (selected-frame))
         (original (frame-parameter frame 'desktop-dont-save))
         (emacs-session-frames--saved-dont-save
          (make-hash-table :test #'eq)))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'desktop-dont-save 'user-owned)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _) t)))
            (emacs-session-frames-prepare frame))
          (should (eq (frame-parameter frame 'desktop-dont-save)
                      'user-owned))
          (set-frame-parameter frame 'desktop-dont-save 'changed-by-user)
          (emacs-session-frames-reset)
          (should (eq (frame-parameter frame 'desktop-dont-save)
                      'changed-by-user))
          (set-frame-parameter frame 'desktop-dont-save 'user-owned)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&optional _) nil)))
            (emacs-session-frames-prepare frame))
          (should (frame-parameter frame 'desktop-dont-save))
          (emacs-session-frames-reset)
          (should (eq (frame-parameter frame 'desktop-dont-save)
                      'user-owned)))
      (set-frame-parameter frame 'desktop-dont-save original)
      (set-frame-parameter frame 'emacs-session-frame-id nil)
      (set-frame-parameter frame 'emacs-session-captured nil))))

(ert-deftest emacs-session-frame-state-migrates-schema-one ()
  (let* ((slot `(:schema 1 :id "legacy" :type tty
                 :saved-at ,(float-time) :window-state (state)))
         (emacs-session--state
          `(:schema 1 :frame-slots ((graphic) (tty . (,slot))))))
    (emacs-session-frames-prune)
    (should (= (plist-get emacs-session--state :schema) 2))
    (should (equal (plist-get
                    (car (alist-get
                          'tty (plist-get emacs-session--state :frame-slots)))
                    :id)
                   "legacy"))
    (should-not (plist-get emacs-session--state :active-tty))))

(ert-deftest emacs-session-active-tty-becomes-mru-after-restart ()
  (let* ((slot `(:schema 1 :id "active" :type tty
                 :saved-at ,(float-time) :window-state (state)))
         (emacs-session--state (emacs-session-frames--empty-state)))
    (setq emacs-session--state
          (plist-put emacs-session--state :active-tty (list slot)))
    (emacs-session-frames-adopt-restored)
    (should-not (plist-get emacs-session--state :active-tty))
    (should (equal (plist-get
                    (car (alist-get
                          'tty (plist-get emacs-session--state :frame-slots)))
                    :id)
                   "active"))))

(ert-deftest emacs-session-dead-client-uses-last-live-snapshot ()
  (let* ((frame (make-symbol "dead-frame"))
         (slot `(:schema 1 :id "disconnected" :type tty
                 :saved-at ,(float-time) :window-state (state)))
         (emacs-session--state (emacs-session-frames--empty-state))
         (emacs-session--restoring-p nil)
         (emacs-session-frames--snapshot-cache
          (make-hash-table :test #'eq)))
    (setq emacs-session--state
          (plist-put emacs-session--state :active-tty (list slot)))
    (puthash frame slot emacs-session-frames--snapshot-cache)
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) nil)))
      (emacs-session-frames-capture frame))
    (should-not (plist-get emacs-session--state :active-tty))
    (should-not (gethash frame emacs-session-frames--snapshot-cache))
    (should (equal (plist-get
                    (car (alist-get
                          'tty (plist-get emacs-session--state :frame-slots)))
                    :id)
                   "disconnected"))))

(ert-deftest emacs-session-frame-capture-precedes-late-server-hook ()
  (emacs-session-test--with-directory
    (let ((delete-frame-functions nil)
          (emacs-session-mode nil)
          (emacs-session--state (emacs-session-frames--empty-state))
          (emacs-session--previous-desktop-buffer-filter nil))
      (unwind-protect
          (progn
            (emacs-session-mode 1)
            ;; `server-start' installs this at the default depth after init.
            (add-hook 'delete-frame-functions #'server-handle-delete-frame)
            (should
             (< (seq-position delete-frame-functions
                              #'emacs-session--frame-deleting)
                (seq-position delete-frame-functions
                              #'server-handle-delete-frame))))
        (when emacs-session-mode (emacs-session-mode -1))))))

(ert-deftest emacs-session-frame-pruning-removes-stale-records ()
  (let* ((old `(:schema 1 :id "old" :type tty
                :saved-at ,(- (float-time) 1000) :window-state (state)))
         (emacs-session-frame-slot-max-age 10)
         (emacs-session--state
          `(:schema 1 :frame-slots ((graphic) (tty . (,old))))))
    (emacs-session-frames-prune)
    (should (= (plist-get emacs-session--state :schema) 2))
    (should-not (alist-get 'tty
                           (plist-get emacs-session--state :frame-slots)))))

(ert-deftest emacs-session-desktop-cwd-hook-retains-only-safe-directories ()
  (emacs-session-test--with-directory
    (with-temp-buffer
      (setq default-directory emacs-session-directory)
      (emacs-session--prepare-buffer-directories)
      (should (local-variable-p 'emacs-session--saved-default-directory))
      (should (equal emacs-session--saved-default-directory
                     emacs-session-directory))
      (setq default-directory "/ssh:example.invalid:/tmp/")
      (emacs-session--prepare-buffer-directories)
      (should-not
       (local-variable-p 'emacs-session--saved-default-directory)))))

(ert-deftest emacs-session-desktop-validator-accepts-upstream-format-206 ()
  (emacs-session-test--with-directory
    (let ((file (expand-file-name "desktop.el" directory)))
      (with-temp-file file
        (insert ";; Desktop File for Emacs\n"
                ";; Desktop file format version 206\n"
                "(setq desktop-saved-frameset nil)\n"))
      (set-file-modes file #o600)
      (should (emacs-session-storage-validate-desktop file)))))

(ert-deftest emacs-session-desktop-first-save-cannot-be-checksum-skipped ()
  (emacs-session-test--with-directory
    (let ((desktop-file-checksum "unchanged")
          (desktop-file-modtime nil))
      (cl-letf (((symbol-function
                  'emacs-session--assert-no-live-foreign-owner)
                 #'ignore)
                ((symbol-function 'emacs-session--checkpoint-auxiliary-state)
                 #'ignore))
        (emacs-session--desktop-save
         (lambda (dirname _release _only-if-changed _version)
           (should-not desktop-file-checksum)
           (with-temp-file
               (expand-file-name desktop-base-file-name dirname)
             (insert ";; Desktop File for Emacs\n"
                     ";; Desktop file format version 208\n"
                     "(setq desktop-saved-frameset nil)\n"))
           (setq desktop-file-checksum "generated"))
         emacs-session-directory nil t nil))
      (should (equal desktop-file-checksum "generated"))
      (should
       (emacs-session-storage-validate-desktop
        (emacs-session--file "desktop.el"))))))

(ert-deftest emacs-session-desktop-failed-publication-restores-cache-state ()
  (emacs-session-test--with-directory
    (let ((desktop-file-checksum "previous")
          (desktop-file-modtime '(1 2 3 4)))
      (cl-letf (((symbol-function
                  'emacs-session--assert-no-live-foreign-owner)
                 #'ignore)
                ((symbol-function 'emacs-session--checkpoint-auxiliary-state)
                 #'ignore))
        (should-error
         (emacs-session--desktop-save
          (lambda (dirname _release _only-if-changed _version)
            (with-temp-file
                (expand-file-name desktop-base-file-name dirname)
              (insert "not a Desktop"))
            (setq desktop-file-checksum "invalid"
                  desktop-file-modtime '(9 9 9 9)))
          emacs-session-directory nil t nil)))
      (should (equal desktop-file-checksum "previous"))
      (should (equal desktop-file-modtime '(1 2 3 4)))
      (should-not (file-exists-p (emacs-session--file "desktop.el"))))))

(ert-deftest emacs-session-live-foreign-owner-fails-closed ()
  (emacs-session-test--with-directory
    (let ((desktop-dirname emacs-session-directory)
          (desktop-base-lock-name "session.lease")
          (reads 0))
      (cl-letf (((symbol-function 'desktop-owner) (lambda (&optional _) 4242))
                ((symbol-function 'emacs-session--pid-running-p)
                 (lambda (_) t))
                ((symbol-function 'desktop-read)
                 (lambda (&optional _) (cl-incf reads))))
        (should-not (emacs-session-state-writable-p))
        (should-error (emacs-session--desktop-read-with-fallback)
                      :type 'error)
        (should-error
         (emacs-session--assert-no-live-foreign-owner
          emacs-session-directory)
         :type 'error)
        (should (= reads 0))
        (should-not (file-exists-p
                     (emacs-session--file "session.lease")))))))

(ert-deftest emacs-session-clear-recovery-removes-last-good ()
  (emacs-session-test--with-directory
    (let* ((file (emacs-session--file "recovery-report.el"))
           (last-good (concat file ".last-good"))
           (emacs-session--recovery-data '(:schema 1 :buffers nil)))
      (dolist (candidate (list file last-good))
        (with-temp-file candidate
          (insert ";; Emacs Session Recovery Report; metadata only.\n"
                  "(:schema 1 :buffers nil)\n"))
        (set-file-modes candidate #o600))
      (emacs-session-clear-recovery-report)
      (should-not emacs-session--recovery-data)
      (should-not (file-exists-p file))
      (should-not (file-exists-p last-good)))))

(ert-deftest emacs-session-cancelled-shutdown-clears-shutdown-state ()
  (let ((emacs-session--shutting-down-p nil)
        (emacs-session--shutdown-hook-ran-p nil)
        (emacs-session--failures nil)
        (emacs-session-before-shutdown-hook
         (list (lambda () (error "test shutdown failure")))))
    (should-not (emacs-session--kill-query))
    (should-not (emacs-session-shutting-down-p))
    (should (equal (plist-get (car emacs-session--failures) :context)
                   "shutdown preparation failed"))))

(ert-deftest emacs-session-unrestored-daemon-shutdown-preserves-desktop ()
  (let ((emacs-session--desktop-restored-p nil)
        (auxiliary 0)
        (desktop 0))
    (cl-letf (((symbol-function 'daemonp) (lambda () t))
              ((symbol-function 'emacs-session--checkpoint-auxiliary-state)
               (lambda () (cl-incf auxiliary)))
              ((symbol-function 'emacs-session-checkpoint)
               (lambda (&optional _) (cl-incf desktop))))
      (emacs-session--shutdown-checkpoint)
      (should (= auxiliary 1))
      (should (= desktop 0))
      (setq emacs-session--desktop-restored-p t)
      (emacs-session--shutdown-checkpoint)
      (should (= desktop 1)))))

(ert-deftest emacs-session-install-failure-restores-global-settings ()
  (emacs-session-test--with-directory
    (let ((emacs-session-mode nil)
          (original-desktop-path (copy-tree desktop-path))
          (original-bookmark-file bookmark-default-file)
          (original-savehist-file savehist-file))
      (cl-letf (((symbol-function
                  'emacs-session--assert-no-live-foreign-owner)
                 (lambda (&optional _)
                   (error "simulated live owner"))))
        (should-error (emacs-session-mode 1)))
      (should-not emacs-session-mode)
      (should (equal desktop-path original-desktop-path))
      (should (equal bookmark-default-file original-bookmark-file))
      (should (equal savehist-file original-savehist-file))
      (should-not
       (advice-member-p #'emacs-session--desktop-save 'desktop-save)))))

(ert-deftest emacs-session-mode-lifecycle-is-idempotent ()
  (emacs-session-test--with-directory
    (let ((emacs-session-mode nil)
          (emacs-session--installed-p nil)
          (emacs-session--saved-settings nil)
          (emacs-session--saved-bookmark-state nil)
          (desktop-save-mode t))
      ;; Disabling an inactive package must not disable upstream Desktop.
      (emacs-session-mode -1)
      (should desktop-save-mode))
    (let ((emacs-session-mode nil)
          (emacs-session--installed-p nil)
          (emacs-session--saved-settings nil)
          (emacs-session--saved-bookmark-state nil))
      (unwind-protect
          (progn
            (emacs-session-mode 1)
            (should emacs-session--installed-p)
            (emacs-session-mode 1)
            (should
             (advice-member-p #'emacs-session--desktop-save 'desktop-save)))
        (emacs-session-mode -1))
      (should-not emacs-session--installed-p))))

(ert-deftest emacs-session-coordinated-checkpoint-produces-valid-generations ()
  (emacs-session-test--with-directory
    (let ((emacs-session-mode nil)
          (emacs-session--desktop-restored-p nil)
          (emacs-session--desktop-restore-failed-p nil)
          (emacs-session--state (emacs-session-frames--empty-state))
          (emacs-session--previous-desktop-buffer-filter nil)
          (bookmark-alist nil)
          (bookmark-bookmarks-timestamp nil)
          (bookmark-alist-modification-count 0))
      (unwind-protect
          (progn
            (emacs-session-mode 1)
            ;; The daemon owns recovery state even before its first client.
            (should (= (desktop-owner emacs-session-directory) (emacs-pid)))
            (emacs-session-checkpoint)
            (should (emacs-session-storage-validate-desktop
                     (emacs-session--file "desktop.el")))
            (should (emacs-session-storage-validate-bookmarks
                     (emacs-session--file "bookmarks.el")))
            (should (= (desktop-owner emacs-session-directory) (emacs-pid)))
            (emacs-session-checkpoint)
            (should (file-exists-p
                     (concat (emacs-session--file "desktop.el") ".last-good")))
            (emacs-session-checkpoint t)
            (should-not (file-exists-p (emacs-session--file "session.lease"))))
        (when emacs-session-mode (emacs-session-mode -1))))))

(provide 'emacs-session-test)

;;; emacs-session-test.el ends here
