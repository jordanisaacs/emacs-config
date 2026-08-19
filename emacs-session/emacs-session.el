;;; emacs-session.el --- Persistent daemon sessions -*- lexical-binding: t -*-

;;; Commentary:

;; `desktop-save-mode' remains the owner of buffers and full GUI framesets.
;; This package delays Desktop restoration until a daemon gets a real client,
;; makes Desktop/bookmark/Savehist writes atomic, and records recently closed
;; client window states without trying to recreate dead TTY terminals.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'desktop)
(require 'savehist)
(require 'seq)
(require 'server)
(require 'subr-x)
(require 'emacs-session-storage)
(require 'emacs-session-frames)

(defcustom emacs-session-directory
  (expand-file-name "session/" user-emacs-directory)
  "Owner-only directory containing managed daemon session state."
  :type 'directory
  :group 'emacs-session)

(defcustom emacs-session-desktop-auto-save-timeout 30
  "Idle seconds before Desktop checkpoints a changed window configuration."
  :type 'integer
  :group 'emacs-session)

(defcustom emacs-session-before-checkpoint-hook nil
  "Hook run immediately before a coordinated session checkpoint."
  :type 'hook
  :group 'emacs-session)

(defcustom emacs-session-before-shutdown-hook nil
  "Hook run once before a coordinated daemon shutdown."
  :type 'hook
  :group 'emacs-session)

(defcustom emacs-session-after-restore-hook nil
  "Hook run after Desktop and the first client frame have been restored."
  :type 'hook
  :group 'emacs-session)

(defvar emacs-session--desktop-restored-p nil)
(defvar emacs-session-mode)
(defvar emacs-session--desktop-restore-failed-p nil)
(defvar emacs-session--desktop-frames-restored-p nil)
(defvar emacs-session--pending-gui-frameset nil)
(defvar emacs-session--desktop-raw-save-p nil)
(defvar emacs-session--shutdown-hook-ran-p nil)
(defvar emacs-session--checkpoint-timer nil)
(defvar emacs-session--previous-desktop-buffer-filter nil)
(defvar emacs-session--saved-settings nil)
(defvar emacs-session--saved-bookmark-state nil)
(defvar emacs-session--installed-p nil)
(defvar emacs-session--shutting-down-p nil)
(defvar emacs-session--unclean-start-p nil)
(defvar emacs-session--recovery-data nil)
(defvar emacs-session--failures nil)

(defun emacs-session-shutting-down-p ()
  "Return non-nil once a coordinated daemon shutdown has begun."
  emacs-session--shutting-down-p)

(defvar-local emacs-session--saved-default-directory nil
  "Validated working directory persisted through Desktop.")

(defun emacs-session--file (name)
  "Return managed state file NAME."
  (expand-file-name name emacs-session-directory))

(defun emacs-session--same-directory-p (left right)
  "Return non-nil when directory names LEFT and RIGHT are equivalent."
  (string= (file-name-as-directory (expand-file-name left))
           (file-name-as-directory (expand-file-name right))))

(defun emacs-session--record-failure (context error-value)
  "Record ERROR-VALUE arising in CONTEXT and emit a concise message."
  (let ((message-text (error-message-string error-value)))
    (push `(:at ,(float-time) :context ,context :message ,message-text)
          emacs-session--failures)
    (message "emacs-session: %s: %s" context message-text)))

(defun emacs-session--lease-validator (file)
  "Validate Desktop-compatible PID lease FILE."
  (emacs-session-storage-assert-safe-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pid (read (current-buffer))))
      (unless (and (integerp pid) (> pid 0))
        (error "Invalid session lease: %s" file))
      (skip-chars-forward " \t\r\n")
      (unless (eobp) (error "Trailing data in session lease: %s" file))))
  t)

(defun emacs-session--desktop-owner (&optional directory)
  "Return the PID recorded in the managed lease for DIRECTORY."
  (let ((desktop-base-lock-name "session.lease"))
    (desktop-owner (or directory emacs-session-directory))))

(defun emacs-session--pid-running-p (pid)
  "Return non-nil when local PID still identifies a running process."
  (and (integerp pid) (> pid 0) (process-attributes pid)))

(defun emacs-session--assert-no-live-foreign-owner (&optional dirname)
  "Signal when DIRNAME's Desktop lease belongs to another live Emacs."
  (when-let* ((directory (or dirname desktop-dirname emacs-session-directory))
              (owner (emacs-session--desktop-owner directory))
              ((/= owner (emacs-pid)))
              ((emacs-session--pid-running-p owner)))
    (error "Session state is owned by live Emacs process %d" owner)))

(defun emacs-session--claim-lock ()
  "Claim the managed state with Desktop's public locking API."
  (emacs-session--assert-no-live-foreign-owner emacs-session-directory)
  (desktop-claim-lock emacs-session-directory)
  (set-file-modes (emacs-session--file "session.lease") #o600))

(defun emacs-session--release-owned-lock ()
  "Release the managed Desktop lock when this Emacs owns it."
  (when (eq (emacs-session--desktop-owner emacs-session-directory)
            (emacs-pid))
    (desktop-release-lock emacs-session-directory)))

;;;###autoload
(defun emacs-session-state-writable-p ()
  "Return non-nil when this daemon may publish managed session state."
  (condition-case nil
      (progn
        (emacs-session--assert-no-live-foreign-owner
         emacs-session-directory)
        t)
    (error nil)))

(defun emacs-session--recover-managed-files ()
  "Validate managed files and recover their last-good generations."
  (dolist (spec
           (list
            (list (emacs-session--file "desktop.el")
                  #'emacs-session-storage-validate-desktop nil)
            (list (emacs-session--file "bookmarks.el")
                  #'emacs-session-storage-validate-bookmarks
                  (list (ignore-errors
                          (file-newest-backup
                           (emacs-session--file "bookmarks.el")))))
            (list (emacs-session--file "savehist.el")
                  #'emacs-session-storage-validate-savehist nil)))
    (condition-case err
        (emacs-session-storage-recover
         (nth 0 spec) (nth 1 spec) (delq nil (nth 2 spec)))
      (error (emacs-session--record-failure
              (format "could not recover %s"
                      (file-name-nondirectory (car spec)))
              err)))))

(defun emacs-session--legacy-file-safe-p (file)
  "Return non-nil when legacy FILE is an owned, bounded regular file."
  (and (stringp file)
       (file-exists-p file)
       (not (file-symlink-p file))
       (let ((attributes (file-attributes file 'integer)))
         (and attributes
              (null (file-attribute-type attributes))
              (= (file-attribute-user-id attributes) (user-uid))
              (<= (file-attribute-size attributes)
                  emacs-session-storage-max-file-size)))))

(defun emacs-session--import-legacy-file (source target validator label)
  "Copy legacy SOURCE to absent managed TARGET after applying VALIDATOR.
LABEL identifies the state owner in recovery diagnostics."
  (when (and (emacs-session--legacy-file-safe-p source)
             (not (equal (expand-file-name source) (expand-file-name target)))
             (not (file-exists-p target)))
    (condition-case err
        (progn
          (emacs-session-storage-write
           target
           (lambda (temporary)
             (copy-file source temporary t t))
           validator)
          (message "emacs-session: imported legacy %s state" label))
      (error
       ;; Leave SOURCE untouched and continue with an empty managed owner.
       (emacs-session--record-failure
        (format "legacy %s import failed" label) err)))))

(defun emacs-session--import-legacy-state (legacy-bookmarks legacy-history)
  "Import LEGACY-BOOKMARKS and LEGACY-HISTORY into the managed directory."
  (emacs-session--import-legacy-file
   legacy-bookmarks (emacs-session--file "bookmarks.el")
   #'emacs-session-storage-validate-bookmarks "Bookmark")
  (emacs-session--import-legacy-file
   legacy-history (emacs-session--file "savehist.el")
   #'emacs-session-storage-validate-savehist "Savehist"))

(defun emacs-session--load-bookmarks ()
  "Load the validated managed bookmark universe."
  (let ((file (emacs-session--file "bookmarks.el")))
    (condition-case err
        (if (file-exists-p file)
            (bookmark-load file t t t)
          ;; Bootstrap the upstream Bookmark writer once so subsequent saves
          ;; preserve an owner-only mode without intercepting its write path.
          (bookmark-save nil file)
          (set-file-modes file #o600))
      (error (emacs-session--record-failure "bookmark restore failed" err)))))

(defconst emacs-session--managed-setting-variables
  '(desktop-path desktop-dirname desktop-base-file-name desktop-base-lock-name
    desktop-load-locked-desktop desktop-save desktop-auto-save-timeout
    desktop-restore-frames desktop-restore-in-current-display
    desktop-restore-forces-onscreen desktop-restore-reuses-frames
    desktop-restore-eager desktop-globals-to-save desktop-locals-to-save
    desktop-modes-not-to-save desktop-buffers-not-to-save-function
    bookmark-default-file bookmark-save-flag bookmark-watch-bookmark-file
    bookmark-version-control savehist-file savehist-file-modes
    kill-emacs-query-functions)
  "Global settings temporarily owned by `emacs-session-mode'.")

(defun emacs-session--save-settings ()
  "Remember global settings and the current Bookmark universe."
  (setq emacs-session--saved-settings
        (mapcar (lambda (symbol)
                  (cons symbol (copy-tree (symbol-value symbol))))
                emacs-session--managed-setting-variables)
        emacs-session--saved-bookmark-state
        (list :alist (copy-tree bookmark-alist)
              :timestamp (copy-tree bookmark-bookmarks-timestamp)
              :modification-count bookmark-alist-modification-count
              :desktop-save-mode desktop-save-mode)))

(defun emacs-session--restore-settings ()
  "Restore settings remembered by `emacs-session--save-settings'."
  (dolist (setting emacs-session--saved-settings)
    (set (car setting) (copy-tree (cdr setting))))
  (when emacs-session--saved-bookmark-state
    (setq bookmark-alist
          (copy-tree (plist-get emacs-session--saved-bookmark-state :alist))
          bookmark-bookmarks-timestamp
          (copy-tree
           (plist-get emacs-session--saved-bookmark-state :timestamp))
          bookmark-alist-modification-count
          (plist-get emacs-session--saved-bookmark-state
                     :modification-count)))
  (let ((restore-desktop-mode
         (plist-get emacs-session--saved-bookmark-state :desktop-save-mode)))
    (setq emacs-session--saved-settings nil
          emacs-session--saved-bookmark-state nil)
    (desktop-save-mode (if restore-desktop-mode 1 -1))))

(defun emacs-session--configure-state-owners ()
  "Point Desktop, Bookmark, and Savehist at the managed state directory."
  (setq emacs-session-directory
        (emacs-session-storage-ensure-directory emacs-session-directory)
        desktop-path (list emacs-session-directory)
        desktop-dirname emacs-session-directory
        desktop-base-file-name "desktop.el"
        desktop-base-lock-name "session.lease"
        desktop-load-locked-desktop 'check-pid
        desktop-save t
        desktop-auto-save-timeout emacs-session-desktop-auto-save-timeout
        desktop-restore-frames t
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen t
        desktop-restore-reuses-frames t
        desktop-restore-eager t
        bookmark-default-file (emacs-session--file "bookmarks.el")
        bookmark-save-flag 1
        bookmark-watch-bookmark-file 'silent
        bookmark-version-control t
        savehist-file (emacs-session--file "savehist.el")
        savehist-file-modes #o600)
  ;; Savehist is the sole owner of minibuffer, search, file-name, and kill
  ;; histories.  Desktop retains registers and its non-history defaults.
  (setq desktop-globals-to-save
        (seq-remove
         (lambda (variable)
           (memq (if (consp variable) (car variable) variable)
                 '(search-ring regexp-search-ring file-name-history kill-ring
                   kill-ring-yank-pointer command-history)))
         desktop-globals-to-save))
  (cl-pushnew 'emacs-session--state desktop-globals-to-save)
  (cl-pushnew 'emacs-session--saved-default-directory desktop-locals-to-save)
  (dolist (mode '(ghostel-mode term-mode vterm-mode comint-mode
                  compilation-mode shell-mode eshell-mode))
    (cl-pushnew mode desktop-modes-not-to-save))
  (emacs-session-frames-prune))

(defun emacs-session--safe-buffer-p (filename buffer-name mode rest)
  "Return non-nil when Desktop may persist BUFFER-NAME and FILENAME."
  (and
   (or (null emacs-session--previous-desktop-buffer-filter)
       (funcall emacs-session--previous-desktop-buffer-filter
                filename buffer-name mode rest))
   (buffer-live-p (get-buffer buffer-name))
   (with-current-buffer buffer-name
     (and (not (file-remote-p default-directory))
          (not (and filename (file-remote-p filename)))
          (not (get-buffer-process (current-buffer)))
          (not (derived-mode-p 'ghostel-mode 'term-mode 'vterm-mode
                               'comint-mode 'compilation-mode))
          (or (null filename) (file-exists-p filename))))))

(defun emacs-session--prepare-buffer-directories ()
  "Populate the package-owned Desktop local for safe buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if-let* ((directory
                 (emacs-session-frames--safe-directory default-directory)))
          (setq-local emacs-session--saved-default-directory directory)
        (kill-local-variable 'emacs-session--saved-default-directory)))))

(defun emacs-session--restore-buffer-directories ()
  "Apply safe package-owned working-directory hints after Desktop restore."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((directory
                   (and (local-variable-p
                         'emacs-session--saved-default-directory)
                        (emacs-session-frames--safe-directory
                         emacs-session--saved-default-directory))))
        (unless buffer-file-name
          (setq default-directory directory))))))

(defun emacs-session--sanitize-restored-buffers ()
  "Replace stale or remote restored working directories with safe locals."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (emacs-session-frames--safe-directory default-directory)
        (setq default-directory
              (or (and buffer-file-name
                       (emacs-session-frames--safe-directory
                        (file-name-directory buffer-file-name)))
                  (file-name-as-directory user-emacs-directory)))))))

(defun emacs-session--before-savehist-save ()
  "Preserve the current managed Savehist generation before upstream saves."
  (when (and emacs-session-mode
             (equal (expand-file-name savehist-file)
                    (expand-file-name (emacs-session--file "savehist.el")))
             (emacs-session-state-writable-p))
    (emacs-session-storage-preserve
     savehist-file #'emacs-session-storage-validate-savehist)))

(defun emacs-session--recovery-validator (file)
  "Validate a private recovery-report FILE."
  (let ((contents (emacs-session-storage--file-contents file)))
    (unless (string-match-p "Emacs Session Recovery Report" contents)
      (error "Invalid recovery report: %s" file)))
  (let ((forms (emacs-session-storage-read-forms file)))
    (unless (and (= (length forms) 1)
                 (listp (car forms))
                 (= (or (plist-get (car forms) :schema) -1) 1)
                 (listp (plist-get (car forms) :buffers)))
      (error "Malformed recovery report: %s" file)))
  t)

(defun emacs-session--modified-buffers ()
  "Return private metadata for modified buffers with recovery artifacts."
  (delq
   nil
   (mapcar
    (lambda (buffer)
      (with-current-buffer buffer
        (when (and (buffer-modified-p)
                   (not (string-prefix-p " " (buffer-name)))
                   (or buffer-file-name buffer-auto-save-file-name))
          `(:buffer ,(buffer-name)
            :file ,(and buffer-file-name (expand-file-name buffer-file-name))
            :auto-save ,buffer-auto-save-file-name
            :auto-save-exists ,(and buffer-auto-save-file-name
                                    (file-exists-p buffer-auto-save-file-name))))))
    (buffer-list))))

(defun emacs-session--load-recovery-data ()
  "Load the latest valid recovery report as inert data."
  (let ((file (emacs-session--file "recovery-report.el")))
    (when (emacs-session-storage-recover file
                                         #'emacs-session--recovery-validator)
      (setq emacs-session--recovery-data
            (car (emacs-session-storage-read-forms file))))))

(defun emacs-session--write-recovery-data ()
  "Persist a report when modified buffers currently need recovery."
  (let ((buffers (emacs-session--modified-buffers)))
    ;; An empty checkpoint must not erase the last useful recovery map.  It is
    ;; cleared explicitly after the user has inspected or recovered it.
    (when buffers
      (setq emacs-session--recovery-data
            `(:schema 1 :saved-at ,(float-time)
              :unclean-start ,emacs-session--unclean-start-p
              :buffers ,buffers))
      (emacs-session-storage-write
       (emacs-session--file "recovery-report.el")
       (lambda (temporary)
         (with-temp-file temporary
           (insert ";; Emacs Session Recovery Report; metadata only.\n")
           (let ((print-length nil) (print-level nil))
             (prin1 emacs-session--recovery-data (current-buffer)))
           (insert "\n")))
       #'emacs-session--recovery-validator))))

(defun emacs-session--checkpoint-auxiliary-state ()
  "Auto-save edits and persist Bookmark, Savehist, and recovery metadata."
  (emacs-session--assert-no-live-foreign-owner emacs-session-directory)
  (run-hooks 'emacs-session-before-checkpoint-hook)
  (emacs-session-frames-snapshot-live)
  (condition-case err
      (do-auto-save t)
    (error (emacs-session--record-failure "buffer auto-save failed" err)))
  (condition-case err
      (emacs-session--write-recovery-data)
    (error (emacs-session--record-failure "recovery report save failed" err)))
  (condition-case err
      (when (and (featurep 'bookmark)
                 (> bookmark-alist-modification-count 0))
        (bookmark-save))
    (error (emacs-session--record-failure "bookmark checkpoint failed" err)))
  (condition-case err
      (when savehist-mode (savehist-save t))
    (error (emacs-session--record-failure "Savehist checkpoint failed" err)))
  (emacs-session-frames-prune))

(defun emacs-session--desktop-save (original dirname &optional release
                                             only-if-changed version)
  "Coordinate ORIGINAL's Desktop save for DIRNAME and publish it atomically.
RELEASE, ONLY-IF-CHANGED, and VERSION retain their `desktop-save' meanings."
  (if (or emacs-session--desktop-raw-save-p
          (not (emacs-session--same-directory-p dirname
                                                emacs-session-directory)))
      (funcall original dirname release only-if-changed version)
    (let* ((target (emacs-session--file "desktop.el"))
           (temporary (emacs-session-storage--temp-file target "desktop"))
           (original-checksum desktop-file-checksum)
           (previous-modtime desktop-file-modtime)
           generated-checksum
           generated-checksum-p
           state-current-p)
      (unwind-protect
          (progn
            (emacs-session--assert-no-live-foreign-owner
             emacs-session-directory)
            (emacs-session--checkpoint-auxiliary-state)
            ;; Preserve upstream conflict and checksum behavior.  An existing
            ;; Desktop is copied with its mtime; a fresh one is presented as an
            ;; absent target, just as `desktop-save' normally expects.
            (if (file-exists-p target)
                (progn
                  (emacs-session-storage--copy-contents target temporary)
                  (set-file-modes temporary #o600)
                  (set-file-times
                   temporary
                   (file-attribute-modification-time
                    (file-attributes target))))
              (delete-file temporary))
            (let ((desktop-base-file-name
                   (file-name-nondirectory temporary))
                  (emacs-session--desktop-raw-save-p t)
                  (write-region-inhibit-fsync nil))
              (if (file-exists-p temporary)
                  (funcall original emacs-session-directory nil
                           only-if-changed version)
                ;; When the real target was absent, no copied temporary file
                ;; exists.  Clear the cached checksum so upstream cannot skip
                ;; the write and leave us nothing to publish.  Propagate the
                ;; checksum produced by a successful call back to its global.
                (let ((desktop-file-checksum nil))
                  (funcall original emacs-session-directory nil
                           only-if-changed version)
                  (setq generated-checksum desktop-file-checksum
                        generated-checksum-p t)))
              (when generated-checksum-p
                (setq desktop-file-checksum generated-checksum)))
            (unless (file-exists-p temporary)
              (error "Desktop save produced no state file"))
            (let ((lease (emacs-session--file "session.lease")))
              (when (file-exists-p lease)
                (set-file-modes lease #o600)))
            (if (and (file-exists-p target)
                     (emacs-session-storage-files-equal-p temporary target))
                (progn
                  (delete-file temporary)
                  (setq temporary nil))
              (emacs-session-storage-publish
               temporary target #'emacs-session-storage-validate-desktop)
              (setq temporary nil))
            (setq desktop-dirname emacs-session-directory
                  desktop-file-modtime
                  (time-convert
                   (file-attribute-modification-time (file-attributes target))
                   'list)
                  state-current-p t)
            (when release (desktop-release-lock emacs-session-directory))
            target)
        (unless state-current-p
          (setq desktop-file-checksum original-checksum
                desktop-file-modtime previous-modtime))
        (when (and temporary (file-exists-p temporary))
          (delete-file temporary))))))

(defun emacs-session--desktop-after-read ()
  "Capture Desktop results before upstream clears its transient frameset."
  (emacs-session--restore-buffer-directories)
  (when desktop-saved-frameset
    (if desktop-restore-frames
        ;; `desktop-after-read-hook' runs after upstream restored the frameset.
        (setq emacs-session--desktop-frames-restored-p t)
      (setq emacs-session--pending-gui-frameset
            (frameset-copy desktop-saved-frameset))))
  (let ((lease (emacs-session--file "session.lease")))
    (when (file-exists-p lease)
      (set-file-modes lease #o600))))

(defun emacs-session--promote-last-good-desktop ()
  "Quarantine the current Desktop and promote its valid last-good generation."
  (let* ((target (emacs-session--file "desktop.el"))
         (last-good (concat target ".last-good")))
    (when (emacs-session-storage-valid-p
           last-good #'emacs-session-storage-validate-desktop)
      (ignore-errors (desktop-release-lock emacs-session-directory))
      (when (file-exists-p target)
        (rename-file target (emacs-session-storage--quarantine-name target)))
      (emacs-session-storage--install-copy
       last-good target #'emacs-session-storage-validate-desktop)
      (setq desktop-file-modtime nil
            desktop-saved-frameset nil)
      (desktop-lazy-abort)
      t)))

(defun emacs-session--desktop-read-with-fallback ()
  "Read the managed Desktop, retrying once from last-good on runtime failure."
  ;; `desktop-read' returns nil both when no file exists and when it refuses a
  ;; live foreign lock.  Detect the latter before its ordinary no-file path can
  ;; create and publish a replacement Desktop.
  (emacs-session--assert-no-live-foreign-owner emacs-session-directory)
  ;; The headless daemon claims the lease during package installation so
  ;; bookmark recovery is never unowned.  `desktop-read' treats our own lease
  ;; as proof that the Desktop was already read, so release it at the narrow
  ;; handoff point and let Desktop claim it again after loading.
  (emacs-session--release-owned-lock)
  (condition-case first-error
      (desktop-read emacs-session-directory)
    (error
     (emacs-session--record-failure
      "current Desktop failed at runtime; trying last-good" first-error)
     (if (emacs-session--promote-last-good-desktop)
         (desktop-read emacs-session-directory)
       (signal (car first-error) (cdr first-error))))))

(defun emacs-session--restore-pending-gui-frameset ()
  "Restore and clear the GUI frameset deferred by an earlier TTY client."
  (when (and emacs-session--pending-gui-frameset (display-graphic-p))
    (when (emacs-session-frames-restore-frameset
           emacs-session--pending-gui-frameset)
      (setq emacs-session--pending-gui-frameset nil
            emacs-session--desktop-frames-restored-p t)
      t)))

(defun emacs-session--schedule-checkpoint ()
  "Coalesce frame lifecycle changes into one near-term checkpoint."
  (unless (or emacs-session--shutting-down-p
              (timerp emacs-session--checkpoint-timer))
    (setq emacs-session--checkpoint-timer
          (run-at-time
           0 nil
           (lambda ()
             (setq emacs-session--checkpoint-timer nil)
             (when (and emacs-session-mode
                        emacs-session--desktop-restored-p
                        (not emacs-session--desktop-restore-failed-p))
               (condition-case err
                   (emacs-session-checkpoint)
                 (error (emacs-session--record-failure
                         "scheduled checkpoint failed" err)))))))))

(defun emacs-session--frame-deleting (frame)
  "Capture a client FRAME before deletion or from its last live snapshot."
  (unless emacs-session--shutting-down-p
    (emacs-session-frames-capture frame)
    (emacs-session--schedule-checkpoint)))

(defun emacs-session--restore-on-frame (&optional frame)
  "Restore the session or a compatible MRU slot into client FRAME."
  (setq frame (or frame (selected-frame)))
  (when (and emacs-session-mode (frame-live-p frame)
             (or (not (frame-initial-p frame)) (not (daemonp)))
             (not emacs-session--shutting-down-p))
    (emacs-session-frames-prepare frame)
    (with-selected-frame frame
      (if emacs-session--desktop-restored-p
          (let ((changed
                 (or (and (display-graphic-p)
                          (emacs-session--restore-pending-gui-frameset))
                     (emacs-session-frames-restore-mru frame))))
            (when changed (emacs-session--schedule-checkpoint)))
        (let ((emacs-session--restoring-p t)
              (emacs-session--desktop-frames-restored-p nil)
              ;; A TTY first client restores buffers normally while the
              ;; after-read hook retains the GUI frameset for a later GUI.
              (desktop-restore-frames (display-graphic-p frame))
              read-result)
          (condition-case err
              (progn
                (setq read-result (emacs-session--desktop-read-with-fallback)
                      emacs-session--desktop-restored-p t)
                (emacs-session-frames-adopt-restored)
                (emacs-session--sanitize-restored-buffers)
                (unless (or emacs-session--desktop-frames-restored-p
                            (and (display-graphic-p)
                                 (emacs-session--restore-pending-gui-frameset)))
                  (emacs-session-frames-restore-mru frame))
                (desktop-save-mode 1)
                ;; A new state directory has no lease until the first save.
                (unless read-result (emacs-session-checkpoint))
                (run-hooks 'emacs-session-after-restore-hook))
            (error
             (setq emacs-session--desktop-restored-p t
                   emacs-session--desktop-restore-failed-p t)
             (desktop-save-mode 0)
             (emacs-session--record-failure "Desktop restore failed" err))))))
    (unless emacs-session--desktop-restore-failed-p
      (emacs-session-frames-snapshot frame))))

(defun emacs-session--window-state-changed (frame)
  "Refresh changed FRAME's in-memory crash snapshot."
  (when (and emacs-session-mode
             emacs-session--desktop-restored-p
             (not emacs-session--desktop-restore-failed-p)
             (not emacs-session--shutting-down-p))
    (emacs-session-frames-snapshot frame)))

(defun emacs-session--server-after-make-frame ()
  "Restore state after the server selects a newly attached client frame."
  (emacs-session--restore-on-frame (selected-frame)))

(defun emacs-session--startup ()
  "Restore a non-daemon Emacs after initialization."
  (unless (daemonp)
    (emacs-session--restore-on-frame (selected-frame))))

;;;###autoload
(defun emacs-session-checkpoint (&optional release)
  "Atomically checkpoint the full session.
Optional RELEASE non-nil releases the Desktop lease after publication."
  (interactive)
  (when (and (daemonp) emacs-session-mode
             (not emacs-session--desktop-restored-p))
    (error "Refusing to replace Desktop before the first client restores it"))
  (when emacs-session--desktop-restore-failed-p
    (error "Refusing to overwrite a Desktop that failed to restore"))
  (desktop-save emacs-session-directory release nil nil))

(defun emacs-session--run-before-shutdown-hook ()
  "Run the public shutdown hook at most once."
  (unless emacs-session--shutdown-hook-ran-p
    (setq emacs-session--shutdown-hook-ran-p t)
    (run-hooks 'emacs-session-before-shutdown-hook)))

(defun emacs-session--shutdown-checkpoint ()
  "Persist shutdown state without replacing an unrestored daemon Desktop."
  (if (and (daemonp) (not emacs-session--desktop-restored-p))
      ;; A daemon can be stopped before its first client.  Saving Desktop here
      ;; would replace the useful old session with startup-only buffers.
      (emacs-session--checkpoint-auxiliary-state)
    (emacs-session-checkpoint t)))

(defun emacs-session--kill-query ()
  "Last kill query: checkpoint only after earlier user queries accepted."
  (if emacs-session--shutting-down-p t
    (setq emacs-session--shutting-down-p t)
    (condition-case err
        (progn
          (emacs-session--run-before-shutdown-hook)
          (emacs-session--shutdown-checkpoint)
          t)
      (error
       (setq emacs-session--shutting-down-p nil
             emacs-session--shutdown-hook-ran-p nil)
       (emacs-session--record-failure "shutdown preparation failed" err)
       nil))))

(defun emacs-session--mark-shutting-down ()
  "Mark shutdown before process-owning kill hooks run."
  (setq emacs-session--shutting-down-p t))

;;;###autoload
(defun emacs-session-shutdown ()
  "Checkpoint the daemon, preserve resumable agents, and exit Emacs."
  (interactive)
  (setq emacs-session--shutting-down-p t)
  (condition-case err
      (progn
        (emacs-session--run-before-shutdown-hook)
        (emacs-session--shutdown-checkpoint))
    (error (emacs-session--record-failure "shutdown preparation failed" err)))
  ;; The explicit command is the noninteractive systemd path.  All user-facing
  ;; kill queries have either happened before it or were intentionally bypassed.
  (let ((kill-emacs-query-functions nil))
    (kill-emacs)))

;;;###autoload
(defun emacs-session-recovery-report ()
  "Display modified-buffer recovery metadata and persistence failures."
  (interactive)
  (let ((buffer (get-buffer-create "*Emacs Session Recovery*"))
        (records (plist-get emacs-session--recovery-data :buffers)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Emacs session recovery\n\n")
        (if records
            (dolist (record records)
              (insert (format "Buffer: %s\n" (plist-get record :buffer)))
              (when-let* ((file (plist-get record :file)))
                (insert (format "  File: %s\n" file)))
              (when-let* ((auto-save (plist-get record :auto-save)))
                (insert (format "  Auto-save: %s%s\n" auto-save
                                (if (plist-get record :auto-save-exists)
                                    " (present)" " (missing)"))))
              (insert "\n"))
          (insert "No retained modified-buffer recovery metadata.\n\n"))
        (when emacs-session--failures
          (insert "Persistence warnings:\n")
          (dolist (failure (reverse emacs-session--failures))
            (insert (format "  %s: %s\n"
                            (plist-get failure :context)
                            (plist-get failure :message)))))
        (special-mode)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun emacs-session-clear-recovery-report ()
  "Delete retained recovery metadata after explicit confirmation."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (yes-or-no-p "Clear retained Emacs recovery metadata? "))
    (setq emacs-session--recovery-data nil)
    (let* ((file (emacs-session--file "recovery-report.el"))
           (files (list file (concat file ".last-good")))
           deleted)
      (dolist (candidate files)
        (when (file-exists-p candidate)
          (delete-file candidate)
          (setq deleted t)))
      deleted)
    (message "Emacs session recovery metadata cleared")))

(defun emacs-session--install ()
  "Install persistence configuration and lifecycle hooks."
  (unless emacs-session--installed-p
    (let ((legacy-bookmark-file bookmark-default-file)
          (legacy-savehist-file savehist-file))
      (emacs-session--save-settings)
      (setq emacs-session--installed-p t)
      (condition-case err
        (progn
          (setq emacs-session--unclean-start-p nil
                emacs-session--desktop-restored-p nil
                emacs-session--desktop-restore-failed-p nil
                emacs-session--desktop-frames-restored-p nil
                emacs-session--pending-gui-frameset nil
                emacs-session--shutting-down-p nil
                emacs-session--shutdown-hook-ran-p nil)
          (desktop-save-mode 0)
          (emacs-session--configure-state-owners)
          ;; No managed file may be repaired, imported, or loaded for writing
          ;; while another live Emacs owns the lease.
          (emacs-session--assert-no-live-foreign-owner
           emacs-session-directory)
          (let* ((lease (emacs-session--file "session.lease"))
                 (invalid-lease
                  (and (or (file-exists-p lease) (file-symlink-p lease))
                       (not (emacs-session-storage-valid-p
                             lease #'emacs-session--lease-validator)))))
            (when invalid-lease
              (setq emacs-session--unclean-start-p t)
              (emacs-session-storage-recover
               lease #'emacs-session--lease-validator))
            (when-let* ((owner
                         (emacs-session--desktop-owner
                          emacs-session-directory)))
              (setq emacs-session--unclean-start-p
                    (or emacs-session--unclean-start-p
                        (not (emacs-session--pid-running-p owner))))))
          (emacs-session--claim-lock)
          (emacs-session--recover-managed-files)
          (emacs-session--import-legacy-state
           legacy-bookmark-file legacy-savehist-file)
          (emacs-session--load-recovery-data)
          (emacs-session--load-bookmarks)
          (setq emacs-session--previous-desktop-buffer-filter
                desktop-buffers-not-to-save-function
                desktop-buffers-not-to-save-function
                #'emacs-session--safe-buffer-p)
          (advice-add 'desktop-save :around #'emacs-session--desktop-save)
          (add-hook 'desktop-save-hook
                    #'emacs-session--prepare-buffer-directories)
          (add-hook 'desktop-after-read-hook
                    #'emacs-session--desktop-after-read)
          (add-hook 'savehist-save-hook
                    #'emacs-session--before-savehist-save)
          ;; Our query is deliberately late: a cancelled Ghostel/user query
          ;; must not put the daemon into shutdown state.  Desktop's own query
          ;; is replaced by this coordinated checkpoint.
          (remove-hook 'kill-emacs-query-functions #'desktop-kill)
          (add-hook 'kill-emacs-query-functions #'emacs-session--kill-query 90)
          (add-hook 'kill-emacs-hook #'emacs-session--mark-shutting-down -100)
          (add-hook 'after-make-frame-functions #'emacs-session-frames-prepare)
          ;; `server-handle-delete-frame' recursively tears down the client and
          ;; can leave later hooks with a dead frame, so capture first.
          (add-hook 'delete-frame-functions #'emacs-session--frame-deleting -90)
          (add-hook 'server-after-make-frame-hook
                    #'emacs-session--server-after-make-frame)
          (add-hook 'window-state-change-functions
                    #'emacs-session--window-state-changed)
          (add-hook 'emacs-startup-hook #'emacs-session--startup)
          (dolist (frame (frame-list))
            (emacs-session-frames-prepare frame)))
      (error
       (setq emacs-session-mode nil)
       (emacs-session--uninstall)
       (signal (car err) (cdr err)))))))

(defun emacs-session--uninstall ()
  "Remove hooks and advices installed by `emacs-session-mode'."
  (when emacs-session--installed-p
    (desktop-save-mode 0)
    (ignore-error file-error (emacs-session--release-owned-lock))
    (when (timerp emacs-session--checkpoint-timer)
      (cancel-timer emacs-session--checkpoint-timer)
      (setq emacs-session--checkpoint-timer nil))
    (advice-remove 'desktop-save #'emacs-session--desktop-save)
    (remove-hook 'desktop-save-hook #'emacs-session--prepare-buffer-directories)
    (remove-hook 'desktop-after-read-hook #'emacs-session--desktop-after-read)
    (remove-hook 'savehist-save-hook #'emacs-session--before-savehist-save)
    (remove-hook 'kill-emacs-query-functions #'emacs-session--kill-query)
    (remove-hook 'kill-emacs-hook #'emacs-session--mark-shutting-down)
    (remove-hook 'after-make-frame-functions #'emacs-session-frames-prepare)
    (remove-hook 'delete-frame-functions #'emacs-session--frame-deleting)
    (remove-hook 'server-after-make-frame-hook
                 #'emacs-session--server-after-make-frame)
    (remove-hook 'window-state-change-functions
                 #'emacs-session--window-state-changed)
    (remove-hook 'emacs-startup-hook #'emacs-session--startup)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (kill-local-variable 'emacs-session--saved-default-directory)))
    (emacs-session-frames-reset)
    (setq emacs-session--previous-desktop-buffer-filter nil
          emacs-session--shutting-down-p nil
          emacs-session--shutdown-hook-ran-p nil
          emacs-session--installed-p nil)
    (when emacs-session--saved-settings
      (emacs-session--restore-settings))))

;;;###autoload
(define-minor-mode emacs-session-mode
  "Persist and restore a daemon session around real client frames."
  :global t
  :group 'emacs-session
  (if emacs-session-mode
      (emacs-session--install)
    (emacs-session--uninstall)))

(provide 'emacs-session)

;;; emacs-session.el ends here
