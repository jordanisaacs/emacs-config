;;; pm-sidebar.el --- Local Ghostel agent sidebar -*- lexical-binding: t -*-

;;; Commentary:

;; A magit-section sidebar over `pm-agent-track's in-process registry.  It has
;; no dependency on `pm serve'; hiding or killing the sidebar does not stop the
;; underlying Ghostel monitor.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'subr-x)
(require 'pm-agent)
(require 'pm-agent-track)
(require 'pm-faces)
(require 'pm-table)

(declare-function pm-project-dispatch "pm-transient" (&optional arg))

(defgroup pm-sidebar nil
  "Local agent-session sidebar."
  :group 'pm)

(defcustom pm-sidebar-width 29
  "Default total sidebar width in columns."
  :type 'integer :group 'pm-sidebar)

(defconst pm-sidebar-buffer-name "*pm-agents*")
(defconst pm-sidebar--no-project "(no project)")

(defface pm-sidebar-working '((t :inherit success :weight bold)) "Working agent.")
(defface pm-sidebar-blocked '((t :inherit error :weight bold)) "Blocked agent.")
(defface pm-sidebar-done '((t :inherit warning :weight bold)) "Unseen completion.")
(defface pm-sidebar-idle '((t :inherit shadow)) "Idle or unknown agent.")
(defface pm-sidebar-age '((t :inherit shadow)) "Last-active age.")
(defface pm-sidebar-detail '((t :inherit shadow :slant italic)) "Activity detail.")
(defface pm-sidebar-rule '((t :inherit shadow)) "Separator rule.")

(defconst pm-sidebar--status
  '(("blocked" "▲" pm-sidebar-blocked t)
    ("done" "◆" pm-sidebar-done t)
    ("working" "●" pm-sidebar-working nil)
    ("idle" "○" pm-sidebar-idle nil)
    ("unknown" "◌" pm-sidebar-idle nil)))
(defconst pm-sidebar--attention
  '(("blocked" . 0) ("done" . 1) ("working" . 2) ("idle" . 3) ("unknown" . 4)))
(defconst pm-sidebar--age-width 5)

(defvar pm-sidebar--render-timer nil)
(defvar pm-sidebar--last-width nil)

(defun pm-sidebar--ellipsize (string width)
  "Truncate STRING to WIDTH with an ellipsis."
  (if (> (length string) width)
      (concat (substring string 0 (max 0 (1- width))) "…")
    string))

(defun pm-sidebar--fit (string width)
  "Ellipsize STRING to WIDTH and right-pad it."
  (let ((cut (pm-sidebar--ellipsize string width)))
    (concat cut (make-string (max 0 (- width (length cut))) ?\s))))

(defun pm-sidebar--rjust (string width)
  "Right-align STRING in WIDTH columns."
  (if (>= (length string) width) string
    (concat (make-string (- width (length string)) ?\s) string)))

(defun pm-sidebar--age (milliseconds)
  "Render MILLISECONDS since epoch as a short age."
  (if (or (null milliseconds) (<= milliseconds 0)) "·"
    (let ((seconds (- (float-time) (/ milliseconds 1000.0))))
      (cond ((< seconds 60) "now")
            ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
            ((< seconds 86400) (format "%dh" (floor (/ seconds 3600))))
            (t (format "%dd" (floor (/ seconds 86400))))))))

(defun pm-sidebar--width ()
  "Return the sidebar's usable text width."
  (if-let ((window (get-buffer-window pm-sidebar-buffer-name t)))
      (max 24 (window-max-chars-per-line window))
    pm-sidebar-width))

(defun pm-sidebar--rule (width)
  "Return a WIDTH-column separator."
  (pm-propertize-face (concat (make-string width ?─) "\n") 'pm-sidebar-rule))

(defun pm-sidebar--attention-rank (session)
  "Return SESSION's attention sort rank."
  (or (cdr (assoc (alist-get 'status session) pm-sidebar--attention)) 99))

(defun pm-sidebar--session-less-p (left right)
  "Return non-nil when LEFT should appear before RIGHT."
  (let ((left-rank (pm-sidebar--attention-rank left))
        (right-rank (pm-sidebar--attention-rank right)))
    (if (/= left-rank right-rank) (< left-rank right-rank)
      (> (or (alist-get 'last_activity_at left) 0)
         (or (alist-get 'last_activity_at right) 0)))))

(defun pm-sidebar--detail (session status)
  "Return SESSION's secondary detail for STATUS."
  (cond ((equal status "blocked") "waiting on you")
        ((equal status "done") "completed — not viewed")
        ((alist-get 'activity session))
        ((alist-get 'rule_id session) (alist-get 'rule_id session))))

(defun pm-sidebar--insert-session (session width)
  "Insert SESSION using WIDTH columns."
  (let* ((status (or (alist-get 'status session) "unknown"))
         (spec (assoc status pm-sidebar--status))
         (glyph (or (nth 1 spec) "◌"))
         (face (or (nth 2 spec) 'pm-sidebar-idle))
         (emphasize (nth 3 spec))
         (title (or (alist-get 'title session) "(untitled)"))
         (age (pm-sidebar--age (alist-get 'last_activity_at session)))
         (room (max 6 (- width 2 pm-sidebar--age-width 1)))
         (detail (pm-sidebar--detail session status)))
    (magit-insert-section (pm-sidebar-session session)
      (magit-insert-heading
        (concat (pm-propertize-face glyph face) " "
                (pm-propertize-face (pm-sidebar--fit title room)
                                    (if emphasize face 'default))
                " " (pm-propertize-face (pm-sidebar--rjust age pm-sidebar--age-width)
                                         'pm-sidebar-age)))
      (when detail
        (insert "  " (pm-propertize-face
                       (concat "↳ " (pm-sidebar--ellipsize detail (- width 4)))
                       'pm-sidebar-detail)
                "\n")))))

(defun pm-sidebar--groups ()
  "Return registry sessions grouped by project."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (session (pm-agent-track-sessions))
      (let ((project (or (alist-get 'project session) pm-sidebar--no-project)))
        (push session (gethash project table))))
    (sort
     (let (groups)
       (maphash (lambda (project sessions) (push (cons project sessions) groups)) table)
       groups)
     (lambda (left right) (string< (car left) (car right))))))

(defun pm-sidebar--insert-legend ()
  "Insert compact status legend."
  (insert (pm-propertize-face "▲ " 'pm-sidebar-blocked) "blocked  "
          (pm-propertize-face "◆ " 'pm-sidebar-done) "done\n"
          (pm-propertize-face "● " 'pm-sidebar-working) "working  "
          (pm-propertize-face "○ " 'pm-sidebar-idle) "idle\n"))

(defun pm-sidebar--render ()
  "Render the local registry into the sidebar buffer."
  (let* ((buffer (get-buffer-create pm-sidebar-buffer-name))
         (window (get-buffer-window buffer t))
         (sessions (pm-agent-track-sessions)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'pm-sidebar-mode) (pm-sidebar-mode))
      (let ((width (pm-sidebar--width)) (inhibit-read-only t)
            (point-before (if (window-live-p window) (window-point window) (point)))
            (window-start-before (and (window-live-p window) (window-start window))))
        (setq pm-sidebar--last-width width)
        (pm-table-reset-section-state)
        (remove-overlays (point-min) (point-max))
        (erase-buffer)
        (magit-insert-section (pm-sidebar-root)
          (insert (pm-table-banner "pm agents" (length sessions)))
          (insert "\n" (pm-sidebar--rule width))
          (if (null sessions)
              (insert "\n" (pm-propertize-face "no active sessions" 'pm-sidebar-idle) "\n")
            (dolist (group (pm-sidebar--groups))
              (insert "\n")
              (magit-insert-section (pm-sidebar-project (car group))
                (magit-insert-heading
                  (pm-propertize-face (car group) 'pm-group-heading))
                (dolist (session (sort (cdr group) #'pm-sidebar--session-less-p))
                  (pm-sidebar--insert-session session width)))))
          (insert "\n" (pm-sidebar--rule width))
          (pm-sidebar--insert-legend))
        (pm-table-cover-root-section)
        (pm-table-show-root-section)
        (goto-char (min point-before (point-max)))
        (when (window-live-p window)
          (set-window-point window (point))
          (when window-start-before
            (set-window-start window (min window-start-before (point-max)) t)))))))

(defun pm-sidebar--schedule-render ()
  "Coalesce registry changes into one visible sidebar render."
  (when (and (get-buffer-window pm-sidebar-buffer-name t)
             (not (timerp pm-sidebar--render-timer)))
    (setq pm-sidebar--render-timer
          (run-at-time 0.2 nil
                       (lambda ()
                         (setq pm-sidebar--render-timer nil)
                         (pm-sidebar--render))))))

(defun pm-sidebar-visit ()
  "Visit the session or project at point."
  (interactive)
  (let* ((section (magit-current-section))
         (type (and section (oref section type)))
         (value (and section (oref section value))))
    (cond
     ((eq type 'pm-sidebar-session)
      (let* ((key (alist-get 'buffer_id value))
             (buffer (pm-agent-track-buffer-for-id key)))
        (unless buffer (user-error "Agent buffer is no longer live"))
        (pm-agent-track-mark-seen key)
        (pop-to-buffer buffer)))
     ((eq type 'pm-sidebar-project)
      (when (equal value pm-sidebar--no-project) (user-error "Not a pm project"))
      (let ((default-directory (pm-agent--cwd value))) (pm-project-dispatch)))
     (t (user-error "Point is not on a session or project")))))

(defun pm-sidebar--on-window-size-change (_frame)
  "Re-render when the sidebar window width changes."
  (when-let* ((window (get-buffer-window pm-sidebar-buffer-name t)))
    (when (/= (max 24 (window-max-chars-per-line window))
              (or pm-sidebar--last-width -1))
      (pm-sidebar--schedule-render))))

(defvar pm-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "RET") #'pm-sidebar-visit)
    (define-key map (kbd "g") #'pm-sidebar-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))
(set-keymap-parent pm-sidebar-mode-map magit-section-mode-map)

(define-derived-mode pm-sidebar-mode magit-section-mode "PM-Agents"
  "Local coding-agent sidebar."
  (setq-local mode-line-format nil cursor-type nil truncate-lines t
              display-line-numbers nil)
  (pm-section-setup-margin))

(defun pm-sidebar-refresh ()
  "Refresh process state and redraw."
  (interactive)
  (pm-agent-track--process-tick)
  (pm-sidebar--render))

(defun pm-sidebar--open ()
  "Open the sidebar in a left side window."
  (get-buffer-create pm-sidebar-buffer-name)
  (display-buffer
   (get-buffer pm-sidebar-buffer-name)
   `((display-buffer-in-side-window) (side . left) (slot . 0)
     (window-width . ,pm-sidebar-width)
     (window-parameters . ((no-delete-other-windows . t)))))
  (add-hook 'window-size-change-functions #'pm-sidebar--on-window-size-change)
  (pm-sidebar--render))

;;;###autoload
(defun pm-sidebar ()
  "Toggle the local coding-agent sidebar."
  (interactive)
  (if-let ((window (get-buffer-window pm-sidebar-buffer-name t)))
      (delete-window window)
    (pm-sidebar--open)))

(defun pm-sidebar-quit ()
  "Remove the sidebar UI without stopping agent monitoring."
  (interactive)
  (when (timerp pm-sidebar--render-timer)
    (cancel-timer pm-sidebar--render-timer)
    (setq pm-sidebar--render-timer nil))
  (remove-hook 'window-size-change-functions #'pm-sidebar--on-window-size-change)
  (when-let* ((buffer (get-buffer pm-sidebar-buffer-name)))
    (when-let* ((window (get-buffer-window buffer t))) (delete-window window))
    (kill-buffer buffer)))

(add-hook 'pm-agent-track-change-hook #'pm-sidebar--schedule-render)

(provide 'pm-sidebar)

;;; pm-sidebar.el ends here
