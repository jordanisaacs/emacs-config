;;; eglot-fwatcher.el --- Offload LSP file watching to fwatcher daemon -*- lexical-binding: t; -*-

;; Author: Jordan Isaacs
;; Package-Requires: ((emacs "29.1") (eglot "1.15"))
;; Keywords: languages, tools

;;; Commentary:

;; Replaces eglot's built-in `workspace/didChangeWatchedFiles' handling with an
;; external Rust daemon (`fwatcher').  The daemon owns inotify watches,
;; deduplicates across subscribers, and streams events over NDJSON stdio.  This
;; keeps emacs responsive when opening files in large monorepos that
;; dynamically register hundreds of thousands of file watches.
;;
;; Usage: (eglot-fwatcher-mode 1)

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'json)
(require 'jsonrpc)
(require 'project)

(declare-function jsonrpc-notify "jsonrpc" (connection method params))
(declare-function jsonrpc--process "jsonrpc" (connection))

(defgroup eglot-fwatcher nil
  "Offload LSP file watching to the `fwatcher' daemon."
  :group 'eglot
  :prefix "eglot-fwatcher-")

(defcustom eglot-fwatcher-executable "fwatcher"
  "Path to the `fwatcher' daemon binary.
Resolved with `executable-find' if not absolute."
  :type 'string)

(defvar eglot-fwatcher-mode)

(defvar eglot-fwatcher--process nil
  "The running `fwatcher' daemon process, or nil.")

(defvar eglot-fwatcher--request-id 0
  "Monotonic request id for the daemon protocol.")

(defvar eglot-fwatcher--subscriptions (make-hash-table :test 'equal)
  "Maps subscription_id (string) to (SERVER BASE-DIR REG-ID).")

(defvar eglot-fwatcher--residue ""
  "Incomplete line buffered from the daemon's stdout.")

(defun eglot-fwatcher--ensure-daemon ()
  "Spawn the `fwatcher' daemon if not already running."
  (unless (process-live-p eglot-fwatcher--process)
    (let ((exe (or (executable-find eglot-fwatcher-executable)
                   (user-error "eglot-fwatcher: %S not found on PATH"
                               eglot-fwatcher-executable))))
      (setq eglot-fwatcher--residue "")
      (clrhash eglot-fwatcher--subscriptions)
      (setq eglot-fwatcher--process
            (make-process
             :name "fwatcher"
             :buffer nil
             :command (list exe)
             :connection-type 'pipe
             :noquery t
             :filter #'eglot-fwatcher--filter
             :sentinel #'eglot-fwatcher--sentinel
             :stderr (get-buffer-create " *fwatcher stderr*"))))))

(defun eglot-fwatcher--sentinel (_proc event)
  "Log unexpected daemon exits using EVENT string."
  (message "fwatcher daemon: %s" (string-trim event)))

(defun eglot-fwatcher--filter (_proc chunk)
  "Accumulate CHUNK and dispatch each complete NDJSON line."
  (setq eglot-fwatcher--residue (concat eglot-fwatcher--residue chunk))
  (let ((pos 0)
        (text eglot-fwatcher--residue)
        done)
    (while (not done)
      (let ((nl (string-search "\n" text pos)))
        (if (not nl)
            (setq done t)
          (let ((line (substring text pos nl)))
            (setq pos (1+ nl))
            (condition-case err
                (eglot-fwatcher--handle-msg
                 (json-parse-string line
                                    :object-type 'plist
                                    :null-object nil
                                    :false-object nil))
              (error (message "fwatcher: bad line %S: %S" line err)))))))
    (setq eglot-fwatcher--residue (substring text pos))))

(defun eglot-fwatcher--handle-msg (msg)
  "Dispatch a parsed MSG plist from the daemon."
  (cond
   ((plist-get msg :event)
    (let* ((sid (plist-get msg :subscription_id))
           (path (plist-get msg :path))
           (kind (plist-get msg :kind))
           (entry (gethash sid eglot-fwatcher--subscriptions)))
      (when entry
        (let ((server (car entry))
              (type (pcase kind
                      ("create" 1)
                      ("change" 2)
                      ("delete" 3)
                      (_ 2))))
          (when (and server (process-live-p (jsonrpc--process server)))
            (jsonrpc-notify
             server :workspace/didChangeWatchedFiles
             (list :changes
                   (vector (list :uri (eglot-path-to-uri path)
                                 :type type)))))))))
   ((plist-member msg :ok)
    (unless (eq (plist-get msg :ok) t)
      (message "fwatcher: req %s failed: %s"
               (plist-get msg :id)
               (plist-get msg :err))))))

(defun eglot-fwatcher--next-id ()
  "Return the next daemon request id."
  (cl-incf eglot-fwatcher--request-id))

(defun eglot-fwatcher--send (obj)
  "Send OBJ as a single NDJSON line to the daemon."
  (eglot-fwatcher--ensure-daemon)
  (process-send-string
   eglot-fwatcher--process
   (concat (json-serialize obj) "\n")))

(defun eglot-fwatcher--do-register (server method id watchers)
  "Handle `workspace/didChangeWatchedFiles' via the daemon.
Sends one subscribe request per base-path group derived from WATCHERS
(registered by LSP registration ID against SERVER under METHOD)."
  (eglot-unregister-capability server method id)
  (let ((groups (make-hash-table :test 'equal))
        (root (project-root (eglot--project server))))
    (mapc
     (lambda (w)
       (let* ((glob-pat (plist-get w :globPattern))
              (pat-str (if (consp glob-pat)
                           (plist-get glob-pat :pattern)
                         glob-pat))
              (base-uri (when (consp glob-pat)
                          (plist-get glob-pat :baseUri)))
              (base-path
               (cond ((null base-uri) root)
                     ((stringp base-uri) (eglot-uri-to-path base-uri))
                     (t (eglot-uri-to-path (plist-get base-uri :uri)))))
              (kind (or (plist-get w :kind) 7))
              (key (expand-file-name (file-name-as-directory base-path))))
         (puthash key
                  (cons (list pat-str kind) (gethash key groups))
                  groups)))
     watchers)
    (maphash
     (lambda (base-path entries)
       (let* ((sid (format "%s:%s:%s"
                           (sxhash server) id (sxhash base-path)))
              (globs (vconcat (mapcar #'car entries)))
              (kinds (apply #'logior (mapcar #'cadr entries))))
         (puthash sid (list server base-path id)
                  eglot-fwatcher--subscriptions)
         (eglot-fwatcher--send
          (list :op "subscribe"
                :id (eglot-fwatcher--next-id)
                :subscription_id sid
                :root base-path
                :globs globs
                :kinds kinds))))
     groups)))

(defun eglot-fwatcher--do-unregister (server id)
  "Cancel daemon subscriptions for SERVER and LSP registration ID."
  (let (to-remove)
    (maphash
     (lambda (sid entry)
       (when (and (eq (car entry) server)
                  (equal (nth 2 entry) id))
         (push sid to-remove)))
     eglot-fwatcher--subscriptions)
    (dolist (sid to-remove)
      (remhash sid eglot-fwatcher--subscriptions)
      (when (process-live-p eglot-fwatcher--process)
        (eglot-fwatcher--send
         (list :op "unsubscribe"
               :id (eglot-fwatcher--next-id)
               :subscription_id sid))))))

(defun eglot-fwatcher--on-shutdown (server &rest _)
  "Sweep daemon subscriptions for SERVER on `eglot-shutdown'."
  (when eglot-fwatcher-mode
    (let (to-remove)
      (maphash
       (lambda (sid entry)
         (when (eq (car entry) server)
           (push sid to-remove)))
       eglot-fwatcher--subscriptions)
      (dolist (sid to-remove)
        (remhash sid eglot-fwatcher--subscriptions)
        (when (process-live-p eglot-fwatcher--process)
          (eglot-fwatcher--send
           (list :op "unsubscribe"
                 :id (eglot-fwatcher--next-id)
                 :subscription_id sid)))))))

(cl-defmethod eglot-register-capability :around
  (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
  "Offload to `fwatcher' when `eglot-fwatcher-mode' is enabled."
  (if eglot-fwatcher-mode
      (eglot-fwatcher--do-register server method id watchers)
    (cl-call-next-method)))

(cl-defmethod eglot-unregister-capability :around
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Offload to `fwatcher' when `eglot-fwatcher-mode' is enabled."
  (if eglot-fwatcher-mode
      (eglot-fwatcher--do-unregister server id)
    (cl-call-next-method)))

;;;###autoload
(define-minor-mode eglot-fwatcher-mode
  "Offload LSP workspace/didChangeWatchedFiles to the `fwatcher' daemon."
  :global t
  :group 'eglot-fwatcher
  (cond
   (eglot-fwatcher-mode
    (advice-add 'eglot-shutdown :before #'eglot-fwatcher--on-shutdown))
   (t
    (advice-remove 'eglot-shutdown #'eglot-fwatcher--on-shutdown)
    (when (process-live-p eglot-fwatcher--process)
      (eglot-fwatcher--send
       (list :op "shutdown" :id (eglot-fwatcher--next-id)))
      (clrhash eglot-fwatcher--subscriptions)))))

(provide 'eglot-fwatcher)

;;; eglot-fwatcher.el ends here
