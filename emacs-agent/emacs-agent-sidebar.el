;;; emacs-agent-sidebar.el --- Local Ghostel agent sidebar -*- lexical-binding: t -*-

;;; Commentary:

;; A magit-section sidebar over `emacs-agent-track's in-process registry.  It has
;; no dependency on `pm serve'; hiding or killing the sidebar does not stop the
;; underlying Ghostel monitor.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)
(require 'pm-agent)
(require 'emacs-agent)
(require 'emacs-agent-track)
(require 'pm-faces)
(require 'pm-table)

(declare-function pm-project-dispatch "pm-transient" (&optional arg))

(defgroup emacs-agent-sidebar nil
  "Local agent-session sidebar."
  :group 'emacs-agent)

(defcustom emacs-agent-sidebar-width 29
  "Default total sidebar width in columns."
  :type 'integer :group 'emacs-agent-sidebar)

(defconst emacs-agent-sidebar-buffer-name "*emacs-agents*")
(defconst emacs-agent-sidebar--no-project "(no project)")

(defface emacs-agent-sidebar-working '((t :inherit success :weight bold)) "Working agent.")
(defface emacs-agent-sidebar-blocked '((t :inherit error :weight bold)) "Blocked agent.")
(defface emacs-agent-sidebar-done '((t :inherit warning :weight bold)) "Unseen completion.")
(defface emacs-agent-sidebar-idle '((t :inherit shadow)) "Idle or unknown agent.")
(defface emacs-agent-sidebar-age '((t :inherit shadow)) "Last-active age.")
(defface emacs-agent-sidebar-detail '((t :inherit shadow :slant italic)) "Activity detail.")
(defface emacs-agent-sidebar-rule '((t :inherit shadow)) "Separator rule.")

(defconst emacs-agent-sidebar--status
  '(("blocked" "▲" emacs-agent-sidebar-blocked t)
    ("done" "◆" emacs-agent-sidebar-done t)
    ("working" "●" emacs-agent-sidebar-working nil)
    ("idle" "○" emacs-agent-sidebar-idle nil)
    ("unknown" "◌" emacs-agent-sidebar-idle nil)))
(defconst emacs-agent-sidebar--age-width 5)

(defvar emacs-agent-sidebar--render-timer nil)
(defvar emacs-agent-sidebar--last-width nil)

(defun emacs-agent-sidebar--ellipsize (string width)
  "Truncate STRING to WIDTH with an ellipsis."
  (if (> (length string) width)
      (concat (substring string 0 (max 0 (1- width))) "…")
    string))

(defun emacs-agent-sidebar--fit (string width)
  "Ellipsize STRING to WIDTH and right-pad it."
  (let ((cut (emacs-agent-sidebar--ellipsize string width)))
    (concat cut (make-string (max 0 (- width (length cut))) ?\s))))

(defun emacs-agent-sidebar--rjust (string width)
  "Right-align STRING in WIDTH columns."
  (if (>= (length string) width) string
    (concat (make-string (- width (length string)) ?\s) string)))

(defun emacs-agent-sidebar--age (milliseconds)
  "Render MILLISECONDS since epoch as a short age."
  (if (or (null milliseconds) (<= milliseconds 0)) "·"
    (let ((seconds (- (float-time) (/ milliseconds 1000.0))))
      (cond ((< seconds 60) "now")
            ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
            ((< seconds 86400) (format "%dh" (floor (/ seconds 3600))))
            (t (format "%dd" (floor (/ seconds 86400))))))))

(defun emacs-agent-sidebar--width ()
  "Return the sidebar's usable text width."
  (if-let ((window (get-buffer-window emacs-agent-sidebar-buffer-name t)))
      (max 24 (window-max-chars-per-line window))
    emacs-agent-sidebar-width))

(defun emacs-agent-sidebar--rule (width)
  "Return a WIDTH-column separator."
  (pm-propertize-face (concat (make-string width ?─) "\n") 'emacs-agent-sidebar-rule))

(defun emacs-agent-sidebar--session-less-p (left right)
  "Return non-nil when LEFT's stable buffer order precedes RIGHT."
  (let ((left-order (alist-get 'buffer_order left))
        (right-order (alist-get 'buffer_order right)))
    (cond ((and (numberp left-order) (numberp right-order)
                (/= left-order right-order))
           (< left-order right-order))
          ((numberp left-order) t)
          ((numberp right-order) nil)
          (t
           (string< (or (alist-get 'buffer_name left) (alist-get 'id left) "")
                    (or (alist-get 'buffer_name right) (alist-get 'id right) ""))))))

(defun emacs-agent-sidebar--detail (session status)
  "Return SESSION's secondary detail for STATUS."
  (cond ((equal status "blocked") "waiting on you")
        ((equal status "done") "completed — not viewed")
        ((alist-get 'activity session))
        ((alist-get 'rule_id session) (alist-get 'rule_id session))))

(defun emacs-agent-sidebar--insert-session (session width)
  "Insert SESSION using WIDTH columns."
  (let* ((status (or (alist-get 'status session) "unknown"))
         (spec (assoc status emacs-agent-sidebar--status))
         (glyph (or (nth 1 spec) "◌"))
         (face (or (nth 2 spec) 'emacs-agent-sidebar-idle))
         (emphasize (nth 3 spec))
         (title (or (alist-get 'title session) "(untitled)"))
         (age (emacs-agent-sidebar--age (alist-get 'last_activity_at session)))
         (room (max 6 (- width 2 emacs-agent-sidebar--age-width 1)))
         (detail (emacs-agent-sidebar--detail session status)))
    (magit-insert-section (emacs-agent-sidebar-session session)
      (magit-insert-heading
        (concat (pm-propertize-face glyph face) " "
                (pm-propertize-face (emacs-agent-sidebar--fit title room)
                                    (if emphasize face 'default))
                " " (pm-propertize-face (emacs-agent-sidebar--rjust age emacs-agent-sidebar--age-width)
                                         'emacs-agent-sidebar-age)))
      (when detail
        (insert "  " (pm-propertize-face
                       (concat "↳ " (emacs-agent-sidebar--ellipsize detail (- width 4)))
                       'emacs-agent-sidebar-detail)
                "\n")))))

(defun emacs-agent-sidebar--groups ()
  "Return registry sessions grouped by project."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (session (emacs-agent-track-sessions))
      (let ((project (or (alist-get 'project session) emacs-agent-sidebar--no-project)))
        (push session (gethash project table))))
    (sort
     (let (groups)
       (maphash (lambda (project sessions) (push (cons project sessions) groups)) table)
       groups)
     (lambda (left right) (string< (car left) (car right))))))

(defun emacs-agent-sidebar--insert-legend ()
  "Insert compact status legend."
  (insert (pm-propertize-face "▲ " 'emacs-agent-sidebar-blocked) "blocked  "
          (pm-propertize-face "◆ " 'emacs-agent-sidebar-done) "done\n"
          (pm-propertize-face "● " 'emacs-agent-sidebar-working) "working  "
          (pm-propertize-face "○ " 'emacs-agent-sidebar-idle) "idle\n"))

(defun emacs-agent-sidebar--render ()
  "Render the local registry into the sidebar buffer."
  (let* ((buffer (get-buffer-create emacs-agent-sidebar-buffer-name))
         (window (get-buffer-window buffer t))
         (sessions (emacs-agent-track-sessions)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'emacs-agent-sidebar-mode) (emacs-agent-sidebar-mode))
      (let ((width (emacs-agent-sidebar--width)) (inhibit-read-only t)
            (point-before (if (window-live-p window) (window-point window) (point)))
            (window-start-before (and (window-live-p window) (window-start window))))
        (setq emacs-agent-sidebar--last-width width)
        (pm-table-reset-section-state)
        (remove-overlays (point-min) (point-max))
        (erase-buffer)
        (magit-insert-section (emacs-agent-sidebar-root)
          (insert (pm-table-banner "emacs agents" (length sessions)))
          (insert "\n" (emacs-agent-sidebar--rule width))
          (if (null sessions)
              (insert "\n" (pm-propertize-face "no active sessions" 'emacs-agent-sidebar-idle) "\n")
            (dolist (group (emacs-agent-sidebar--groups))
              (insert "\n")
              (magit-insert-section (emacs-agent-sidebar-project (car group))
                (magit-insert-heading
                  (pm-propertize-face (car group) 'pm-group-heading))
                (dolist (session (sort (cdr group) #'emacs-agent-sidebar--session-less-p))
                  (emacs-agent-sidebar--insert-session session width)))))
          (insert "\n" (emacs-agent-sidebar--rule width))
          (emacs-agent-sidebar--insert-legend))
        (pm-table-cover-root-section)
        (pm-table-show-root-section)
        (goto-char (min point-before (point-max)))
        (when (window-live-p window)
          (set-window-point window (point))
          (when window-start-before
            (set-window-start window (min window-start-before (point-max)) t)))))))

(defun emacs-agent-sidebar--schedule-render ()
  "Coalesce registry changes into one visible sidebar render."
  (when (and (get-buffer-window emacs-agent-sidebar-buffer-name t)
             (not (timerp emacs-agent-sidebar--render-timer)))
    (setq emacs-agent-sidebar--render-timer
          (run-at-time 0.2 nil
                       (lambda ()
                         (setq emacs-agent-sidebar--render-timer nil)
                         (emacs-agent-sidebar--render))))))

(defun emacs-agent-sidebar-visit ()
  "Visit the session or project at point."
  (interactive)
  (let* ((section (magit-current-section))
         (type (and section (oref section type)))
         (value (and section (oref section value))))
    (cond
     ((eq type 'emacs-agent-sidebar-session)
      (let* ((key (alist-get 'buffer_id value))
             (buffer (emacs-agent-track-buffer-for-id key)))
        (unless buffer (user-error "Agent buffer is no longer live"))
        (emacs-agent-track-mark-seen key)
        (pop-to-buffer buffer)))
     ((eq type 'emacs-agent-sidebar-project)
      (when (equal value emacs-agent-sidebar--no-project) (user-error "Not a pm project"))
      (let ((default-directory (pm-agent--cwd value))) (pm-project-dispatch)))
     (t (user-error "Point is not on a session or project")))))

(defun emacs-agent-sidebar--on-window-size-change (_frame)
  "Re-render when the sidebar window width changes."
  (when-let* ((window (get-buffer-window emacs-agent-sidebar-buffer-name t)))
    (when (/= (max 24 (window-max-chars-per-line window))
              (or emacs-agent-sidebar--last-width -1))
      (emacs-agent-sidebar--schedule-render))))

(defvar emacs-agent-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'emacs-agent-sidebar-visit)
    (define-key map (kbd "g") #'emacs-agent-sidebar-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))
(set-keymap-parent emacs-agent-sidebar-mode-map magit-section-mode-map)

(define-derived-mode emacs-agent-sidebar-mode magit-section-mode "PM-Agents"
  "Local coding-agent sidebar."
  (setq-local mode-line-format nil cursor-type nil truncate-lines t
              display-line-numbers nil)
  (pm-section-setup-margin))

(defun emacs-agent-sidebar-refresh ()
  "Refresh process state and redraw."
  (interactive)
  (emacs-agent-track--process-tick)
  (emacs-agent-sidebar--render))

(defun emacs-agent-sidebar--open ()
  "Open the sidebar in a left side window."
  (get-buffer-create emacs-agent-sidebar-buffer-name)
  (display-buffer
   (get-buffer emacs-agent-sidebar-buffer-name)
   `((display-buffer-in-side-window) (side . left) (slot . 0)
     (window-width . ,emacs-agent-sidebar-width)
     (window-parameters . ((no-delete-other-windows . t)))))
  (add-hook 'window-size-change-functions #'emacs-agent-sidebar--on-window-size-change)
  (emacs-agent-sidebar--render))

;;;###autoload
(defun emacs-agent-sidebar ()
  "Toggle the local coding-agent sidebar."
  (interactive)
  (if-let ((window (get-buffer-window emacs-agent-sidebar-buffer-name t)))
      (delete-window window)
    (emacs-agent-sidebar--open)))

(defun emacs-agent-sidebar-quit ()
  "Remove the sidebar UI without stopping agent monitoring."
  (interactive)
  (when (timerp emacs-agent-sidebar--render-timer)
    (cancel-timer emacs-agent-sidebar--render-timer)
    (setq emacs-agent-sidebar--render-timer nil))
  (remove-hook 'window-size-change-functions #'emacs-agent-sidebar--on-window-size-change)
  (when-let* ((buffer (get-buffer emacs-agent-sidebar-buffer-name)))
    (when-let* ((window (get-buffer-window buffer t))) (delete-window window))
    (kill-buffer buffer)))

(add-hook 'emacs-agent-track-change-hook #'emacs-agent-sidebar--schedule-render)

(provide 'emacs-agent-sidebar)

;;; emacs-agent-sidebar.el ends here
