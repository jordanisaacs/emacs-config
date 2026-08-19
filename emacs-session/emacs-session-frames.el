;;; emacs-session-frames.el --- Frame and client state -*- lexical-binding: t -*-

;;; Commentary:

;; Desktop owns full GUI framesets.  This module complements it with a small
;; stack of recently closed GUI and TTY client window states.  TTY terminals
;; themselves are never recreated; only a compatible new client's windows are
;; populated from a saved slot.

;;; Code:

(require 'cl-lib)
(require 'frameset)
(require 'seq)
(require 'subr-x)
(require 'emacs-session-storage)

(defcustom emacs-session-frame-slot-limit 8
  "Maximum recently closed frame slots retained per display type."
  :type 'integer
  :group 'emacs-session)

(defcustom emacs-session-frame-slot-max-age (* 30 24 60 60)
  "Maximum age in seconds of a recently closed frame slot."
  :type 'integer
  :group 'emacs-session)

(defconst emacs-session-frames-schema-version 2)

(defvar emacs-session--state nil
  "Desktop-persisted metadata owned by `emacs-session'.")

(defvar emacs-session--restoring-p nil
  "Non-nil while session code is restoring frames or windows.")

(defvar emacs-session-frames--snapshot-cache (make-hash-table :test #'eq)
  "Latest live snapshot keyed by frame object.
This cache lets a delayed terminal-disconnect hook recover state after the
frame object itself is no longer live.")

(defvar emacs-session-frames--saved-dont-save
  (make-hash-table :test #'eq :weakness 'key)
  "Original `desktop-dont-save' value for each frame prepared by the mode.")

(defun emacs-session-frames--empty-state ()
  "Return a fresh session metadata object."
  (list :schema emacs-session-frames-schema-version
        :frame-slots (list (list 'graphic) (list 'tty))
        :active-tty nil))

(defun emacs-session-frames--type (frame)
  "Return FRAME's persistence class."
  (if (display-graphic-p frame) 'graphic 'tty))

(defun emacs-session-frames--safe-directory (directory)
  "Return normalized local DIRECTORY when it is currently usable."
  (when (and (stringp directory)
             (not (file-remote-p directory))
             (file-directory-p directory))
    (file-name-as-directory (expand-file-name directory))))

(defun emacs-session-frames--new-id ()
  "Return an opaque daemon-independent frame identifier."
  (substring
   (secure-hash 'sha256
                (format "%s\0%s\0%s\0%s"
                        (system-name) (float-time)
                        (random most-positive-fixnum) (emacs-pid)))
   0 32))

(defun emacs-session-frames-prepare (frame)
  "Give FRAME a stable id and configure Desktop eligibility."
  (when (frame-live-p frame)
    (unless (frame-parameter frame 'emacs-session-frame-id)
      (set-frame-parameter frame 'emacs-session-frame-id
                           (emacs-session-frames--new-id)))
    ;; Desktop framesets are deliberately GUI-only.  Text clients use the
    ;; compatible MRU records below and an existing terminal is never deleted
    ;; in order to make room for a restored frame.
    (unless (display-graphic-p frame)
      (unless (gethash frame emacs-session-frames--saved-dont-save)
        ;; Wrap the value so an original nil remains distinguishable from a
        ;; missing hash-table entry.
        (puthash frame
                 (list (frame-parameter frame 'desktop-dont-save))
                 emacs-session-frames--saved-dont-save))
      (set-frame-parameter frame 'desktop-dont-save t)))
  frame)

(defun emacs-session-frames-reset ()
  "Restore frame parameters changed by `emacs-session-frames-prepare'."
  (maphash
   (lambda (frame saved)
     (when (frame-live-p frame)
       (set-frame-parameter frame 'desktop-dont-save (car saved))))
   emacs-session-frames--saved-dont-save)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'emacs-session-frame-id nil)
    (set-frame-parameter frame 'emacs-session-captured nil))
  (clrhash emacs-session-frames--saved-dont-save)
  (clrhash emacs-session-frames--snapshot-cache))

(defun emacs-session-frames--slot-valid-p (slot &optional now)
  "Return non-nil when SLOT has the current schema and is not stale at NOW."
  (let ((saved-at (plist-get slot :saved-at))
        (type (plist-get slot :type)))
    (and (listp slot)
         (= (or (plist-get slot :schema) -1) 1)
         (memq type '(graphic tty))
         (numberp saved-at)
         (<= 0 (- (or now (float-time)) saved-at)
             emacs-session-frame-slot-max-age)
         (listp (plist-get slot :window-state)))))

(defun emacs-session-frames-prune ()
  "Migrate session metadata and discard malformed or stale frame slots."
  (pcase (and (listp emacs-session--state)
              (plist-get emacs-session--state :schema))
    (2 nil)
    (1
     ;; Schema 1 had only closed-frame slots.  Copy it before adding the new
     ;; active-TTY field so a quoted Desktop value is never mutated in place.
     (setq emacs-session--state (copy-tree emacs-session--state)
           emacs-session--state
           (plist-put emacs-session--state :schema 2)
           emacs-session--state
           (plist-put emacs-session--state :active-tty nil)))
    (_
     (when emacs-session--state
       (message "emacs-session: reset unsupported frame-state schema"))
     (setq emacs-session--state (emacs-session-frames--empty-state))))
  (let* ((now (float-time))
         (slots (plist-get emacs-session--state :frame-slots))
         (graphic (seq-take
                   (seq-filter
                    (lambda (slot)
                      (and (eq (plist-get slot :type) 'graphic)
                           (emacs-session-frames--slot-valid-p slot now)))
                    (alist-get 'graphic slots))
                   emacs-session-frame-slot-limit))
         (tty (seq-take
               (seq-filter
                (lambda (slot)
                  (and (eq (plist-get slot :type) 'tty)
                       (emacs-session-frames--slot-valid-p slot now)))
                (alist-get 'tty slots))
                           emacs-session-frame-slot-limit))
         (active-tty
          (seq-take
           (seq-filter
            (lambda (slot)
              (and (eq (plist-get slot :type) 'tty)
                   (emacs-session-frames--slot-valid-p slot now)))
            (plist-get emacs-session--state :active-tty))
           emacs-session-frame-slot-limit)))
    (setq emacs-session--state
          (plist-put emacs-session--state :frame-slots
                     `((graphic . ,graphic) (tty . ,tty)))
          emacs-session--state
          (plist-put emacs-session--state :active-tty active-tty)))
  emacs-session--state)

(defun emacs-session-frames--push-slot (type slot)
  "Push SLOT onto the MRU stack for TYPE."
  (emacs-session-frames-prune)
  (let* ((slots (plist-get emacs-session--state :frame-slots))
         (others (alist-get type slots))
         (updated (seq-take
                   (cons slot
                         (seq-remove
                          (lambda (old)
                            (equal (plist-get old :id) (plist-get slot :id)))
                          others))
                   emacs-session-frame-slot-limit)))
    (setf (alist-get type slots) updated)
    (setq emacs-session--state
          (plist-put emacs-session--state :frame-slots slots))))

(defun emacs-session-frames--snapshot-record (frame)
  "Return a persistence record for live, non-initial FRAME."
  (when (and (frame-live-p frame)
             (not (frame-initial-p frame)))
    (emacs-session-frames-prepare frame)
    (with-selected-frame frame
      (let* ((window (selected-window))
             (buffer (window-buffer window))
             (directory (with-current-buffer buffer
                          (emacs-session-frames--safe-directory
                           default-directory)))
             (type (emacs-session-frames--type frame))
             (state (window-state-get (frame-root-window frame) t)))
        `(:schema 1
          :id ,(frame-parameter frame 'emacs-session-frame-id)
          :type ,type
          :saved-at ,(float-time)
          :directory ,directory
          :window-state ,state)))))

(defun emacs-session-frames--remove-active (id)
  "Remove the active TTY snapshot identified by ID."
  (emacs-session-frames-prune)
  (setq emacs-session--state
        (plist-put
         emacs-session--state :active-tty
         (seq-remove
          (lambda (slot) (equal id (plist-get slot :id)))
          (plist-get emacs-session--state :active-tty)))))

(defun emacs-session-frames-snapshot (frame &optional force)
  "Refresh the non-resumable live snapshot for FRAME.
TTY snapshots are included in Desktop checkpoints for daemon-crash recovery;
they become resumable slots only when loaded by a later daemon.  Optional
FORCE allows a coordinated checkpoint to snapshot a stable restore boundary."
  (when (and (or force (not emacs-session--restoring-p))
             (frame-live-p frame)
             (not (frame-initial-p frame)))
    (condition-case err
        (when-let* ((snapshot (emacs-session-frames--snapshot-record frame)))
          (set-frame-parameter frame 'emacs-session-captured nil)
          (puthash frame snapshot emacs-session-frames--snapshot-cache)
          (when (eq (plist-get snapshot :type) 'tty)
            (emacs-session-frames-prune)
            (let ((id (plist-get snapshot :id)))
              (setq emacs-session--state
                    (plist-put
                     emacs-session--state :active-tty
                     (seq-take
                      (cons snapshot
                            (seq-remove
                             (lambda (old)
                               (equal id (plist-get old :id)))
                             (plist-get emacs-session--state :active-tty)))
                      emacs-session-frame-slot-limit)))))
          snapshot)
      (error
       (message "emacs-session: could not snapshot frame: %s"
                (error-message-string err))
       nil))))

(defun emacs-session-frames-snapshot-live ()
  "Refresh checkpoint metadata for every live client frame."
  (let (active-tty)
    (dolist (frame (frame-list))
      (when-let* ((snapshot (emacs-session-frames-snapshot frame t)))
        (when (eq (plist-get snapshot :type) 'tty)
          (push snapshot active-tty))))
    ;; Drop records for TTY clients that are no longer live.  Their deletion
    ;; hook has either moved the cached record to an MRU slot or will do so
    ;; before the next checkpoint timer runs.
    (emacs-session-frames-prune)
    (setq emacs-session--state
          (plist-put emacs-session--state :active-tty
                     (seq-take (nreverse active-tty)
                               emacs-session-frame-slot-limit))))
  (let (dead)
    (maphash (lambda (frame _snapshot)
               (unless (frame-live-p frame) (push frame dead)))
             emacs-session-frames--snapshot-cache)
    (dolist (frame dead)
      (remhash frame emacs-session-frames--snapshot-cache)))
  emacs-session--state)

(defun emacs-session-frames-adopt-restored ()
  "Turn active TTY snapshots loaded from an older daemon into MRU slots."
  (emacs-session-frames-prune)
  (let ((active (copy-sequence
                 (plist-get emacs-session--state :active-tty))))
    (setq emacs-session--state
          (plist-put emacs-session--state :active-tty nil))
    ;; Preserve the saved order: the first active record remains the first
    ;; compatible layout offered to a newly attached TTY client.
    (dolist (slot (reverse active))
      (emacs-session-frames--push-slot 'tty slot)))
  emacs-session--state)

(defun emacs-session-frames-capture (frame)
  "Move FRAME's latest state into its display-type MRU stack."
  (unless emacs-session--restoring-p
    (let* ((live (frame-live-p frame))
           (already-captured
            (and live
                 (frame-parameter frame 'emacs-session-captured)))
           (snapshot
            (unless already-captured
              (if live
                  (or (condition-case err
                          (emacs-session-frames--snapshot-record frame)
                        (error
                         (message "emacs-session: could not capture frame: %s"
                                  (error-message-string err))
                         nil))
                      (gethash frame emacs-session-frames--snapshot-cache))
                (gethash frame emacs-session-frames--snapshot-cache)))))
      (when snapshot
        (when live
          (set-frame-parameter frame 'emacs-session-captured t))
        (remhash frame emacs-session-frames--snapshot-cache)
        (emacs-session-frames--remove-active (plist-get snapshot :id))
        (emacs-session-frames--push-slot
         (plist-get snapshot :type) snapshot))))
  emacs-session--state)

(defun emacs-session-frames--pop-slot (type)
  "Remove and return the most recent valid frame slot for TYPE."
  (emacs-session-frames-prune)
  (let* ((slots (plist-get emacs-session--state :frame-slots))
         (records (alist-get type slots))
         (slot (car records)))
    (when slot
      (setf (alist-get type slots) (cdr records))
      (setq emacs-session--state
            (plist-put emacs-session--state :frame-slots slots)))
    slot))

(defun emacs-session-frames-restore-mru (frame)
  "Restore one compatible recently closed window state into FRAME."
  (when (frame-live-p frame)
    (emacs-session-frames-prepare frame)
    (when-let* ((type (emacs-session-frames--type frame))
                (slot (emacs-session-frames--pop-slot type)))
      (let ((emacs-session--restoring-p t)
            (window-restore-killed-buffer-windows nil))
        (condition-case err
            (progn
              (with-selected-frame frame
                (window-state-put (plist-get slot :window-state)
                                  (frame-root-window frame) 'safe)
                (set-frame-parameter frame 'emacs-session-frame-id
                                     (plist-get slot :id))
                (when-let* ((directory
                             (emacs-session-frames--safe-directory
                              (plist-get slot :directory)))
                            (buffer (window-buffer (selected-window))))
                  ;; Visiting-file buffers already derive the right directory
                  ;; from their file.  The hint is for scratch/help/project
                  ;; buffers whose cwd would otherwise fall back globally.
                  (with-current-buffer buffer
                    (unless buffer-file-name
                      (setq default-directory directory)))))
              t)
          (error
           (message "emacs-session: ignored obsolete frame slot: %s"
                    (error-message-string err))
           nil))))))

(defun emacs-session-frames--reuse-p (frame)
  "Return non-nil when FRAME may be reused for a GUI frameset."
  (and (frame-live-p frame)
       (display-graphic-p frame)
       (not (frame-initial-p frame))))

(defun emacs-session-frames--cleanup (frame action)
  "Preserve text terminals and clean unused GUI FRAME for ACTION."
  (when (and (memq action '(:rejected :ignored))
             (frame-live-p frame)
             (display-graphic-p frame)
             (not (frame-initial-p frame)))
    (delete-frame frame)))

(defun emacs-session-frames-restore-frameset (frameset)
  "Restore GUI FRAMESET while preserving every existing TTY client."
  (when (and frameset (frameset-valid-p frameset) (display-graphic-p))
    (let ((emacs-session--restoring-p t))
      (condition-case err
          (progn
            (frameset-restore
             frameset
             :reuse-frames #'emacs-session-frames--reuse-p
             :cleanup-frames #'emacs-session-frames--cleanup
             :force-display t
             :force-onscreen t)
            (dolist (frame (frame-list))
              (emacs-session-frames-prepare frame))
            t)
        (error
         (message "emacs-session: GUI frameset restore failed: %s"
                  (error-message-string err))
         nil)))))

(provide 'emacs-session-frames)

;;; emacs-session-frames.el ends here
