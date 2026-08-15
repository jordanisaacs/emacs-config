;;; emacs-agent-rules.el --- Herdr-derived terminal rules -*- lexical-binding: t -*-

;;; Commentary:

;; Declarative screen-state rules derived from Herdr's Apache-2.0 manifests:
;; https://github.com/herdrdev/herdr/tree/master/src/detect/manifests
;; The versions below are bundled deliberately so tracking stays offline and
;; reproducible.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst emacs-agent-rules-manifests
  `(("claude" :version "2026.08.13.1" :rules
     ((:id "osc_title_working" :state "working" :priority 1100 :region "osc_title"
       :visible-working t :regex ("^[⠀-⣿◐-◓] "))
      (:id "btw_overlay_working" :state "working" :priority 975
       :region "bottom_non_empty_lines(5)" :visible-working t
       :line-regex ("^[[:space:]]*/btw\\(?:[[:space:]]\\|$\\)"
                    "(?i)esc to close[[:space:]]*$"))
      (:id "transcript_viewer" :state "unknown" :priority 1000
       :region "bottom_non_empty_lines(3)" :skip-state-update t
       :contains ("showing detailed transcript")
       :any ((:contains ("ctrl+o" "to toggle")) (:contains ("ctrl+e" "show all"))
             (:contains ("ctrl+e" "collapse")) (:contains ("↑↓ scroll"))
             (:contains ("? for shortcuts"))))
      (:id "live_blocked_form" :state "blocked" :priority 980
       :region "after_last_horizontal_rule" :visible-blocker t
       :contains ("esc to cancel")
       :any ((:contains ("enter to confirm"))
             (:contains ("enter to select")
              :any ((:contains ("tab/arrow keys to navigate"))
                    (:contains ("arrow keys to navigate"))
                    (:contains ("arrows to navigate"))
                    (:contains ("↑/↓ to navigate"))
                    (:contains ("↑↓ to navigate"))))))
      (:id "dynamic_workflow_prompt" :state "blocked" :priority 980
       :region "whole_recent" :visible-blocker t
       :contains ("run a dynamic workflow?" "esc to cancel"))
      (:id "live_prompt_box" :state "idle" :priority 950
       :region "prompt_box_body" :visible-idle t
       :line-regex ("^[[:space:]]*❯")
       :not ((:contains ("enter to select")) (:contains ("esc to cancel"))
             (:contains ("tab/arrow keys")) (:contains ("arrow keys to navigate"))
             (:contains ("↑/↓ to navigate"))))
      (:id "model_picker_menu" :state "unknown" :priority 900
       :region "whole_recent" :skip-state-update t
       :contains ("select model" "enter to set as default" "esc to cancel")
       :not ((:contains ("do you want to proceed?")) (:contains ("enter to select"))))
      (:id "bash_permission_prompt" :state "blocked" :priority 850
       :region "whole_recent" :visible-blocker t :contains ("do you want to proceed?")
       :any ((:contains ("bash command")) (:contains ("bash("))
             (:contains ("contains expansion")) (:contains ("tab to amend"))
             (:contains ("ctrl+e to explain")))
       :all ((:any ((:line-regex ("(?i)^[[:space:]]*❯?[[:space:]]*yes\\_>"))
                    (:line-regex ("(?i)^[[:space:]]*1\\.[[:space:]]*yes\\_>"))
                    (:line-regex ("(?i)^[[:space:]]*2\\.[[:space:]]*no\\_>"))))))
      (:id "generic_permission_prompt" :state "blocked" :priority 840
       :region "after_last_horizontal_rule" :visible-blocker t
       :contains ("do you want to proceed?" "esc to cancel")
       :all ((:any ((:line-regex ("(?i)^[[:space:]]*❯?[[:space:]]*1\\.[[:space:]]*yes\\_>"))
                    (:line-regex ("(?i)^[[:space:]]*2\\.[[:space:]]*yes\\_>"))
                    (:line-regex ("(?i)^[[:space:]]*2\\.[[:space:]]*no\\_>"))
                    (:line-regex ("(?i)^[[:space:]]*3\\.[[:space:]]*no\\_>"))))))
      (:id "legacy_no_prompt_blocker" :state "blocked" :priority 300
       :region "whole_recent"
       :any ((:contains ("do you want to") :any ((:contains ("yes")) (:contains ("❯"))))
             (:contains ("would you like to") :any ((:contains ("yes")) (:contains ("❯"))))
             (:contains ("waiting for permission"))
             (:contains ("do you want to allow this connection?"))
             (:contains ("tab to amend")) (:contains ("ctrl+e to explain"))
             (:contains ("do you want to proceed?" "esc to cancel"))
             (:contains ("review your answers"))
             (:contains ("skip interview and plan immediately")))
       :not ((:regex ("^[[:space:]]*❯[[:space:]]*$"))))
      (:id "osc_title_idle" :state "idle" :priority 250 :region "osc_title"
       :visible-idle t :regex ("^✳ "))
      (:id "osc_progress_idle" :state "idle" :priority 250 :region "osc_progress"
       :regex ("^4;0"))))
    ("codex" :version "2026.08.09.1" :rules
     ((:id "osc_title_blocked" :state "blocked" :priority 1100 :region "osc_title"
       :visible-blocker t :contains ("Action Required"))
      (:id "osc_title_working" :state "working" :priority 1050 :region "osc_title"
       :visible-working t
       :regex ("\\(?:^\\| \\)[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]\\(?: \\|$\\)"))
      (:id "transcript_viewer" :state "unknown" :priority 1000
       :region "after_last_prompt_marker" :skip-state-update t
       :contains ("↑/↓ to scroll" "pgup/pgdn to" "home/end to jump" "q to quit")
       :any ((:contains ("esc to edit prev")) (:contains ("esc/← to edit prev"))))
      (:id "trust_directory" :state "blocked" :priority 950
       :region "top_non_empty_lines(20)" :visible-blocker t
       :all ((:regex ("\\`> You are in [^\r\n]+\\(?:\r?\n\\|$\\)"))
             (:regex ("Do[[:space:]]+you[[:space:]]+trust[[:space:]]+the[[:space:]]+contents[[:space:]]+of[[:space:]]+this[[:space:]]+directory?"))))
      (:id "live_strong_blocker" :state "blocked" :priority 900
       :region "after_last_prompt_marker" :visible-blocker t
       :any ((:contains ("press enter to confirm or esc to cancel"))
             (:contains ("enter to submit answer")) (:contains ("enter to submit all"))
             (:contains ("allow command?"))))
      (:id "weak_blocker" :state "blocked" :priority 600 :region "whole_recent"
       :any ((:contains ("[y/n]")) (:contains ("yes (y)"))
             (:contains ("do you want to") :any ((:contains ("yes")) (:contains ("❯"))))
             (:contains ("would you like to") :any ((:contains ("yes")) (:contains ("❯"))))))
      (:id "screen_working_fallback" :state "working" :priority 500
       :region "bottom_non_empty_lines(3)" :visible-working t
       :line-regex ("^[•◦][[:space:]]+Working ([^)]*esc to interrupt)\\(?: · .*\\)?$")
       :not ((:contains ("■ Conversation interrupted"))))
      (:id "osc_title_idle" :state "idle" :priority 100 :region "osc_title"
       :visible-idle t :regex ("[^[:space:]]")
       :not ((:regex ("\\(?:^\\| \\)[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]\\(?: \\|$\\)"))
             (:contains ("Action Required"))))))
    ("cursor" :version "2026.08.03.1" :rules
     ((:id "write_file_approval" :state "blocked" :priority 320
       :region "bottom_non_empty_lines(8)" :visible-blocker t
       :contains ("write to this file?" "proceed (y)")
       :any ((:contains ("reject & propose changes")) (:contains ("esc or n or p"))
             (:contains ("add write("))))
      (:id "approval_prompt" :state "blocked" :priority 300 :region "whole_recent"
       :visible-blocker t
       :any ((:contains ("waiting for approval" "run this command?")
              :any ((:contains ("run (once) (y)")) (:contains ("skip (esc or n)"))))
             (:contains ("(y) (enter)"))
             (:line-regex ("(?i)^[[:space:]]*allow .*[(]y[)]"))
             (:contains ("keep (n)")) (:contains ("skip (esc or n)"))
             (:line-regex ("(?i)^[[:space:]]*\\(?:→[[:space:]]*\\)?run .*[(]y[)]"))))
      (:id "stop_hint_working" :state "working" :priority 100
       :region "bottom_non_empty_lines(6)" :visible-working t :contains ("ctrl+c to stop"))
      (:id "background_task_status_working" :state "working" :priority 95
       :region "bottom_non_empty_lines(5)" :visible-working t
       :line-regex ("(?i)\\_<[1-9][0-9]*[[:space:]]+background[[:space:]]+tasks?\\_>"))
      (:id "spinner_working" :state "working" :priority 90
       :region "bottom_non_empty_lines(8)" :visible-working t
       :line-regex ("^[[:space:]]*\\(?:⬡\\|⬢\\|[⠀-⣿]+\\)[[:space:]]+[[:alpha:]]+[[:word:]]*ing\\_>")))))
  "Pinned Herdr-derived manifests keyed by canonical agent name.")

(defun emacs-agent-rules--lines (text)
  "Split TEXT into lines while retaining empty lines."
  (split-string (string-replace "\r" "" (or text "")) "\n" nil))

(defun emacs-agent-rules--join (lines)
  "Join LINES as terminal text."
  (mapconcat #'identity lines "\n"))

(defun emacs-agent-rules--horizontal-rule-p (line)
  "Return non-nil when LINE is a Claude-style horizontal rule."
  (let ((trimmed (string-trim line)))
    (and (not (string-empty-p trimmed))
         (string-match "\\`─+" trimmed)
         (let ((count (- (match-end 0) (match-beginning 0)))
               (suffix (string-trim-left (substring trimmed (match-end 0)))))
           (or (string-empty-p suffix) (>= count 3))))))

(defun emacs-agent-rules--bottom-non-empty (text count)
  "Return TEXT from its COUNTth non-empty line from the bottom."
  (let* ((lines (emacs-agent-rules--lines text)) (seen 0) start)
    (cl-loop for line in (reverse lines) for reverse-index from 0
             when (not (string-empty-p (string-trim line))) do (cl-incf seen)
             when (= seen count)
             do (setq start (- (length lines) reverse-index 1)) and return nil)
    (if start (emacs-agent-rules--join (nthcdr start lines)) "")))

(defun emacs-agent-rules--top-non-empty (text count)
  "Return TEXT through its COUNTth non-empty line."
  (let ((lines (emacs-agent-rules--lines text)) (seen 0) end)
    (cl-loop for line in lines for index from 0
             when (not (string-empty-p (string-trim line))) do (cl-incf seen)
             when (= seen count) do (setq end (1+ index)) and return nil)
    (emacs-agent-rules--join (seq-take lines (or end (length lines))))))

(defun emacs-agent-rules--after-last-prompt (text)
  "Return the portion of TEXT after its last Codex prompt marker."
  (let ((lines (emacs-agent-rules--lines text)) index)
    (cl-loop for line in lines for i from 0
             when (or (string= line "›") (string-prefix-p "› " line)) do (setq index i))
    (if index (emacs-agent-rules--join (nthcdr (1+ index) lines)) text)))

(defun emacs-agent-rules--after-last-rule (text)
  "Return the portion of TEXT after its last horizontal rule."
  (let ((lines (emacs-agent-rules--lines text)) index)
    (cl-loop for line in lines for i from 0
             when (emacs-agent-rules--horizontal-rule-p line) do (setq index i))
    (if index (emacs-agent-rules--join (nthcdr (1+ index) lines)) text)))

(defun emacs-agent-rules--prompt-box-body (text)
  "Return the body of the last Claude prompt box in TEXT."
  (let ((lines (emacs-agent-rules--lines text)) (borders '()))
    (cl-loop for line in lines for i from 0
             when (emacs-agent-rules--horizontal-rule-p line) do (push i borders))
    (if (< (length borders) 2) ""
      (let* ((top (nth 1 borders)) (body (nthcdr (1+ top) lines)) end)
        (cl-loop for line in body for i from 0
                 when (emacs-agent-rules--horizontal-rule-p line)
                 do (setq end i) and return nil)
        (emacs-agent-rules--join (seq-take body (or end (length body))))))))

(defun emacs-agent-rules--region (spec screen osc-title osc-progress)
  "Select SPEC from SCREEN, OSC-TITLE, and OSC-PROGRESS."
  (cond
   ((string= spec "osc_title") (or osc-title ""))
   ((string= spec "osc_progress") (or osc-progress ""))
   ((string= spec "after_last_prompt_marker") (emacs-agent-rules--after-last-prompt screen))
   ((string= spec "prompt_box_body") (emacs-agent-rules--prompt-box-body screen))
   ((string= spec "after_last_horizontal_rule") (emacs-agent-rules--after-last-rule screen))
   ((string-match "\\`bottom_non_empty_lines(\\([0-9]+\\))\\'" spec)
    (emacs-agent-rules--bottom-non-empty screen (string-to-number (match-string 1 spec))))
   ((string-match "\\`top_non_empty_lines(\\([0-9]+\\))\\'" spec)
    (emacs-agent-rules--top-non-empty screen (string-to-number (match-string 1 spec))))
   ((string= spec "whole_recent") screen)
   (t "")))

(defun emacs-agent-rules--regexp-match-p (regexp text)
  "Return non-nil when REGEXP matches TEXT."
  (let* ((fold (string-prefix-p "(?i)" regexp))
         (case-fold-search fold)
         (pattern (if fold (substring regexp 4) regexp)))
    (string-match-p pattern text)))

(defun emacs-agent-rules--gate-match-p (gate text)
  "Return non-nil when declarative GATE matches TEXT."
  (let ((lower (downcase text)))
    (and (seq-every-p (lambda (needle) (string-search (downcase needle) lower))
                      (plist-get gate :contains))
         (seq-every-p (lambda (regexp) (emacs-agent-rules--regexp-match-p regexp text))
                      (plist-get gate :regex))
         (seq-every-p
          (lambda (regexp)
            (seq-some (lambda (line) (emacs-agent-rules--regexp-match-p regexp line))
                      (emacs-agent-rules--lines text)))
          (plist-get gate :line-regex))
         (seq-every-p (lambda (nested) (emacs-agent-rules--gate-match-p nested text))
                      (plist-get gate :all))
         (let ((any (plist-get gate :any)))
           (or (null any)
               (seq-some (lambda (nested) (emacs-agent-rules--gate-match-p nested text)) any)))
         (not (seq-some (lambda (nested) (emacs-agent-rules--gate-match-p nested text))
                        (plist-get gate :not))))))

(defun emacs-agent-rules-detect (agent screen &optional osc-title osc-progress)
  "Detect AGENT state from SCREEN and optional OSC evidence."
  (let* ((manifest (cdr (assoc agent emacs-agent-rules-manifests)))
         (rules (plist-get manifest :rules)) best)
    (dolist (rule rules)
      (let ((text (emacs-agent-rules--region (plist-get rule :region)
                                          (or screen "") osc-title osc-progress)))
        (when (and (emacs-agent-rules--gate-match-p rule text)
                   (or (null best)
                       (> (plist-get rule :priority) (plist-get best :priority))))
          (setq best rule))))
    (if best
        (list :state (plist-get best :state) :rule-id (plist-get best :id)
              :priority (plist-get best :priority)
              :visible-idle (plist-get best :visible-idle)
              :visible-blocker (plist-get best :visible-blocker)
              :visible-working (plist-get best :visible-working)
              :skip-state-update (plist-get best :skip-state-update))
      (list :state (if manifest "idle" "unknown") :rule-id nil :priority -1))))

(provide 'emacs-agent-rules)

;;; emacs-agent-rules.el ends here
