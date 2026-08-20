;;; winner-mode-test.el --- Tests for the Winner window toggle -*- lexical-binding: t; -*-

;;; Commentary:

;; Focused regression tests for the custom `C-x 1' toggle in init.org.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'winner)

(declare-function my/toggle-delete-other-windows nil ())

(defvar winner-mode-test-init-file
  (expand-file-name
   "../init.org"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the init.org file containing the Winner toggle.")

(defun winner-mode-test--load-toggle ()
  "Load the Winner toggle definition from `winner-mode-test-init-file'."
  (with-temp-buffer
    (insert-file-contents winner-mode-test-init-file)
    (goto-char (point-min))
    (unless (re-search-forward
             "^[[:space:]]*\\((defun my/toggle-delete-other-windows\\_>\\)"
             nil t)
      (error "Winner toggle not found in %s" winner-mode-test-init-file))
    (goto-char (match-beginning 1))
    (eval (read (current-buffer)))))

(winner-mode-test--load-toggle)

(defun winner-mode-test--window-signature ()
  "Return the ordered buffer and side-window shape of the selected frame."
  (mapcar
   (lambda (window)
     (list (buffer-name (window-buffer window))
           (window-parameter window 'window-side)
           (window-parameter window 'no-delete-other-windows)))
   (winner-sorted-window-list)))

(defun winner-mode-test--execute-toggle ()
  "Execute the Winner toggle and emulate its command-boundary hooks."
  (setq this-command #'my/toggle-delete-other-windows
        real-this-command this-command)
  (funcall-interactively #'my/toggle-delete-other-windows)
  (winner-change-fun)
  (winner-save-old-configurations)
  (setq last-command this-command)
  this-command)

(defun winner-mode-test--run-scenario (side-window-p)
  "Run the two-press toggle scenario.
When SIDE-WINDOW-P is non-nil, preserve a protected side window."
  (let ((left (generate-new-buffer " *winner-test-left*"))
        (right (generate-new-buffer " *winner-test-right*"))
        (side (generate-new-buffer " *winner-test-side*"))
        (winner-ring-alist nil)
        (winner-currents nil)
        (winner-last-command nil)
        (winner-last-frames nil)
        (winner-modified-list nil)
        (winner-undo-frame nil)
        (winner-pending-undo-ring nil)
        (winner-undo-counter nil)
        (winner-undone-data nil)
        (winner-point-alist nil)
        (last-command nil)
        (this-command 'winner-mode-test-setup)
        (real-this-command 'winner-mode-test-setup)
        result)
    (unwind-protect
        (progn
          (winner-mode 1)
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) left)
            (set-window-buffer (split-window-right) right)
            (when side-window-p
              (let ((window
                     (display-buffer-in-side-window
                      side '((side . left) (slot . 0)
                             (window-width . 20)))))
                (set-window-parameter window 'no-delete-other-windows t)
                (set-window-dedicated-p window 'side)))
            (select-window (get-buffer-window left))
            (winner-change-fun)
            (winner-save-old-configurations)
            (let ((before (winner-mode-test--window-signature)))
              (winner-mode-test--execute-toggle)
              (let ((after-first-count (length (window-list)))
                    (single-main-window
                     (eq (selected-window) (window-main-window))))
                (setq result
                      (list :before before
                            :before-count (length before)
                            :after-first-count after-first-count
                            :single-main-window single-main-window
                            :second-command
                            (winner-mode-test--execute-toggle)
                            :after-second
                            (winner-mode-test--window-signature)
                            :after-second-count (length (window-list))
                            :last-command last-command))))))
      (winner-mode -1)
      (dolist (buffer (list left right side))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    result))

(ert-deftest winner-mode-toggle-restores-plain-split ()
  "A repeated toggle restores an ordinary two-window layout."
  (let ((result (winner-mode-test--run-scenario nil)))
    (should (= (plist-get result :before-count) 2))
    (should (= (plist-get result :after-first-count) 1))
    (should (= (plist-get result :after-second-count) 2))
    (should (equal (plist-get result :before)
                   (plist-get result :after-second)))
    (should (eq (plist-get result :second-command) 'winner-undo))
    (should (eq (plist-get result :last-command) 'winner-undo))))

(ert-deftest winner-mode-toggle-ignores-protected-side-window ()
  "A protected side window does not prevent the repeated toggle."
  (let ((result (winner-mode-test--run-scenario t)))
    (should (= (plist-get result :before-count) 3))
    (should (= (plist-get result :after-first-count) 2))
    (should (plist-get result :single-main-window))
    (should (= (plist-get result :after-second-count) 3))
    (should (equal (plist-get result :before)
                   (plist-get result :after-second)))))

(provide 'winner-mode-test)

;;; winner-mode-test.el ends here
