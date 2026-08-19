;;; emacs-session-storage.el --- Safe session state files -*- lexical-binding: t -*-

;;; Commentary:

;; Shared persistence primitives for `emacs-session'.  All managed files live
;; in one owner-only directory and are published with a same-directory rename.
;; The previous valid generation is retained as FILE.last-good.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emacs-session nil
  "Persistent state for long-running Emacs daemons."
  :group 'convenience)

(defcustom emacs-session-storage-max-file-size (* 64 1024 1024)
  "Largest managed state file accepted during validation, in bytes."
  :type 'integer
  :group 'emacs-session)

(defun emacs-session-storage-ensure-directory (directory)
  "Create DIRECTORY if necessary and enforce owner-only access."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (when (file-symlink-p (directory-file-name directory))
      (error "Session state directory must not be a symlink: %s" directory))
    (unless (file-directory-p directory)
      (make-directory directory t))
    (let ((attributes (file-attributes directory 'integer)))
      (unless (= (file-attribute-user-id attributes) (user-uid))
        (error "Session state directory is not owned by this user: %s" directory)))
    (set-file-modes directory #o700)
    directory))

(defun emacs-session-storage-assert-safe-file (file)
  "Signal unless FILE is a small, owner-only regular file."
  (let ((attributes (file-attributes file 'integer)))
    (unless attributes
      (error "Session state file does not exist: %s" file))
    (when (file-symlink-p file)
      (error "Session state file must not be a symlink: %s" file))
    (unless (null (file-attribute-type attributes))
      (error "Session state file is not regular: %s" file))
    (unless (= (file-attribute-user-id attributes) (user-uid))
      (error "Session state file is not owned by this user: %s" file))
    (when (not (zerop (logand (file-modes file) #o077)))
      (error "Session state file has group/world permissions: %s" file))
    (when (> (file-attribute-size attributes)
             emacs-session-storage-max-file-size)
      (error "Session state file is too large: %s" file))
    t))

(defun emacs-session-storage--temp-file (target purpose)
  "Create an owner-only temporary file beside TARGET for PURPOSE."
  (make-temp-file
   (expand-file-name (format ".%s.%s."
                             (file-name-nondirectory target) purpose)
                     (file-name-directory target))))

(defun emacs-session-storage-valid-p (file validator)
  "Return non-nil when FILE passes VALIDATOR without signaling."
  (and (file-exists-p file)
       (condition-case nil
           (progn (funcall validator file) t)
         (error nil))))

(defun emacs-session-storage--copy-contents (source target)
  "Copy SOURCE literally to TARGET and synchronize it through Emacs."
  (with-temp-buffer
    (insert-file-contents-literally source)
    (let ((coding-system-for-write 'no-conversion)
          (write-region-inhibit-fsync nil))
      (write-region (point-min) (point-max) target nil 'silent))))

(defun emacs-session-storage--install-copy (source target validator)
  "Atomically copy valid SOURCE to TARGET using VALIDATOR."
  (let ((temporary (emacs-session-storage--temp-file target "recover")))
    (unwind-protect
        (progn
          (funcall validator source)
          (emacs-session-storage--copy-contents source temporary)
          (set-file-modes temporary #o600)
          (funcall validator temporary)
          (rename-file temporary target t)
          (setq temporary nil)
          (set-file-modes target #o600))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun emacs-session-storage-publish (temporary target validator)
  "Validate TEMPORARY and atomically publish it as TARGET.

When TARGET is valid according to VALIDATOR, retain it as TARGET.last-good
before replacement."
  (setq target (expand-file-name target))
  (emacs-session-storage-ensure-directory (file-name-directory target))
  (set-file-modes temporary #o600)
  (funcall validator temporary)
  (let ((last-good (concat target ".last-good")))
    (when (emacs-session-storage-valid-p target validator)
      (let ((backup-temp (emacs-session-storage--temp-file target "last-good")))
        (unwind-protect
            (progn
              (emacs-session-storage--copy-contents target backup-temp)
              (set-file-modes backup-temp #o600)
              (funcall validator backup-temp)
              (rename-file backup-temp last-good t)
              (setq backup-temp nil)
              (set-file-modes last-good #o600))
          (when (and backup-temp (file-exists-p backup-temp))
            (delete-file backup-temp)))))
    (rename-file temporary target t)
    (set-file-modes target #o600)
    target))

(defun emacs-session-storage-write (target writer validator)
  "Call WRITER with a temporary path, then publish it as TARGET.
VALIDATOR is called before either the new or last-good generation is used."
  (setq target (expand-file-name target))
  (emacs-session-storage-ensure-directory (file-name-directory target))
  (let ((temporary (emacs-session-storage--temp-file target "write")))
    (unwind-protect
        (progn
          (let ((write-region-inhibit-fsync nil))
            (funcall writer temporary))
          (unless (file-exists-p temporary)
            (error "Session state writer produced no file for %s" target))
          (prog1 (emacs-session-storage-publish temporary target validator)
            (setq temporary nil)))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun emacs-session-storage-replace (target writer validator)
  "Call WRITER and atomically replace TARGET without rotating last-good state."
  (setq target (expand-file-name target))
  (emacs-session-storage-ensure-directory (file-name-directory target))
  (let ((temporary (emacs-session-storage--temp-file target "replace")))
    (unwind-protect
        (progn
          (let ((write-region-inhibit-fsync nil))
            (funcall writer temporary))
          (set-file-modes temporary #o600)
          (funcall validator temporary)
          (rename-file temporary target t)
          (setq temporary nil)
          (set-file-modes target #o600)
          target)
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun emacs-session-storage-preserve (target validator)
  "Preserve valid TARGET as TARGET.last-good using VALIDATOR.
Return the backup name when a generation was preserved, nil otherwise."
  (when (emacs-session-storage-valid-p target validator)
    (let ((last-good (concat (expand-file-name target) ".last-good")))
      (emacs-session-storage-replace
       last-good
       (lambda (temporary)
         (emacs-session-storage--copy-contents target temporary))
       validator)
      last-good)))

(defun emacs-session-storage-files-equal-p (left right)
  "Return non-nil when regular files LEFT and RIGHT have equal contents."
  (and (file-regular-p left)
       (file-regular-p right)
       (= (file-attribute-size (file-attributes left))
          (file-attribute-size (file-attributes right)))
       (equal (emacs-session-storage--file-contents left)
              (emacs-session-storage--file-contents right))))

(defun emacs-session-storage--quarantine-name (file)
  "Return an unused quarantine name beside FILE."
  (let* ((stamp (format-time-string "%Y%m%dT%H%M%SZ" nil t))
         (base (format "%s.corrupt.%s" file stamp))
         (candidate base)
         (index 1))
    (while (file-exists-p candidate)
      (setq candidate (format "%s.%d" base index)
            index (1+ index)))
    candidate))

(defun emacs-session-storage-recover (target validator &optional backups)
  "Return a usable TARGET, recovering a valid prior generation.
Try TARGET.last-good followed by BACKUPS, checking each with VALIDATOR.
Invalid owned state is moved aside under a `.corrupt.TIMESTAMP' name."
  (setq target (expand-file-name target))
  (emacs-session-storage-ensure-directory (file-name-directory target))
  (cond
   ((emacs-session-storage-valid-p target validator) target)
   (t
    (when (or (file-exists-p target) (file-symlink-p target))
      ;; Never move a foreign regular file.  A symlink itself is safe to move
      ;; out of our already owner-only directory, but is never followed.
      (unless (file-symlink-p target)
        (let ((attributes (file-attributes target 'integer)))
          (unless (= (file-attribute-user-id attributes) (user-uid))
            (error "Refusing foreign session state file: %s" target))))
      (rename-file target (emacs-session-storage--quarantine-name target)))
    (when-let* ((candidate
                 (cl-find-if
                  (lambda (file)
                    (emacs-session-storage-valid-p file validator))
                  (cons (concat target ".last-good") backups))))
      (emacs-session-storage--install-copy candidate target validator)
      target))))

(defun emacs-session-storage--file-contents (file)
  "Return FILE contents after applying common security checks."
  (emacs-session-storage-assert-safe-file file)
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun emacs-session-storage-read-forms (file)
  "Read and return all Lisp forms from safe FILE without evaluating them."
  (emacs-session-storage-assert-safe-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form done)
      (while (not done)
        (condition-case nil
            (progn
              (setq form (read (current-buffer)))
              (push form forms))
          (end-of-file (setq done t))))
      (nreverse forms))))

(defun emacs-session-storage-validate-desktop (file)
  "Validate the structure of an Emacs Desktop FILE."
  (let ((contents (emacs-session-storage--file-contents file)))
    (unless (and (string-match-p "Desktop File for Emacs" contents)
                 (string-match-p "Desktop file format version [0-9]+" contents))
      (error "Invalid or unsupported Desktop file: %s" file)))
  (let ((forms (emacs-session-storage-read-forms file)))
    (unless forms (error "Empty Desktop file: %s" file)))
  t)

(defun emacs-session-storage-validate-savehist (file)
  "Validate a generated Savehist FILE without evaluating it."
  (let ((contents (emacs-session-storage--file-contents file)))
    (unless (string-match-p "Minibuffer history file" contents)
      (error "Invalid Savehist file: %s" file)))
  (let ((forms (emacs-session-storage-read-forms file)))
    (unless forms (error "Empty Savehist file: %s" file)))
  t)

(defun emacs-session-storage-validate-bookmarks (file)
  "Validate a generated Emacs bookmark FILE without loading it."
  (let ((contents (emacs-session-storage--file-contents file)))
    (unless (string-match-p "Emacs Bookmark Format Version 1" contents)
      (error "Invalid bookmark file: %s" file)))
  (let ((forms (emacs-session-storage-read-forms file)))
    (unless (= (length forms) 1)
      (error "Bookmark file must contain exactly one data form: %s" file))
    (unless (and (listp (car forms))
                 (cl-every (lambda (bookmark)
                             (and (consp bookmark)
                                  (stringp (car bookmark))
                                  (listp (cdr bookmark))))
                           (car forms)))
      (error "Malformed bookmark records in %s" file)))
  t)

(provide 'emacs-session-storage)

;;; emacs-session-storage.el ends here
