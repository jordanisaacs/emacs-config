;;; pm-agent-tracker-test.el --- Tests for local agent tracking -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'pm-agent-track)

(ert-deftest pm-agent-rules-detects-all-supported-agents ()
  (should (equal (plist-get (pm-agent-rules-detect
                             "claude" "" "⠋ Working" "") :state)
                 "working"))
  (should (equal (plist-get (pm-agent-rules-detect
                             "codex"
                             "›\nAllow command?\npress enter to confirm or esc to cancel")
                            :state)
                 "blocked"))
  (should (equal (plist-get (pm-agent-rules-detect
                             "cursor"
                             "Waiting for approval\nRun this command?\nRun (once) (y)")
                            :state)
                 "blocked")))

(ert-deftest pm-agent-rules-honors-skip-state-update ()
  (let ((detection
         (pm-agent-rules-detect
          "codex"
          "›\nShowing transcript\n↑/↓ to scroll pgup/pgdn to page home/end to jump q to quit\nesc to edit prev")))
    (should (plist-get detection :skip-state-update))
    (should (equal (plist-get detection :rule-id) "transcript_viewer"))))

(ert-deftest pm-agent-rules-selects-prompt-box-body ()
  (let ((screen "old\n────────\n❯ prompt\n────────\nfooter"))
    (should (equal (pm-agent-rules--prompt-box-body screen) "❯ prompt"))))

(ert-deftest pm-agent-track-classifies-wrapper-process-groups ()
  (should (equal
           (pm-agent-track--classify-processes
            '((:pid 10 :comm "dbexec" :cmdline "dbexec isaac codex --")
              (:pid 11 :comm "codex" :cmdline "/nix/store/hash/bin/codex")))
           "codex"))
  (should (equal (pm-agent-track--record-agent
                  '(:comm "node" :cmdline "/opt/claude-code/cli.js"))
                 "claude"))
  (should (equal (pm-agent-track--record-agent '(:comm "agent" :cmdline "agent"))
                 "cursor")))

(ert-deftest pm-agent-track-finds-foreground-process-group-leader ()
  (cl-letf (((symbol-function 'pm-agent-track--process-record)
             (lambda (pid)
               (pcase pid
                 (1 '(:pid 1 :pgrp 1 :tpgid 20))
                 (20 '(:pid 20 :pgrp 20 :tpgid 20 :comm "codex"))))))
    (should (equal (pm-agent-track--foreground-processes 1)
                   '((:pid 20 :pgrp 20 :tpgid 20 :comm "codex"))))))

(ert-deftest pm-agent-track-ignores-idle-shell-process-group ()
  (cl-letf (((symbol-function 'pm-agent-track--process-record)
             (lambda (_pid) '(:pid 1 :pgrp 1 :tpgid 1))))
    (should-not (pm-agent-track--foreground-processes 1))))

(ert-deftest pm-agent-track-initial-idle-is-seen-later-idle-is-done ()
  (with-temp-buffer
    (let ((pm-agent-track--sessions (make-hash-table :test 'equal))
          (pm-agent-track--identity '((session_id . "s1") (cwd . "/tmp"))))
      (cl-letf (((symbol-function 'pm-agent-track--selected-p) (lambda () nil))
                ((symbol-function 'pm-agent-track--schedule-title) #'ignore)
                ((symbol-function 'pm-agent-track--project) (lambda (_) "demo")))
        (pm-agent-track--publish "buffer" "claude" '(:state "idle") "claude")
        (should (equal (alist-get 'status (gethash "buffer" pm-agent-track--sessions))
                       "idle"))
        (pm-agent-track--publish "buffer" "claude" '(:state "working") "claude")
        (pm-agent-track--publish "buffer" "claude" '(:state "idle" :visible-idle t)
                                 "claude")
        (should (equal (alist-get 'status (gethash "buffer" pm-agent-track--sessions))
                       "done"))
        (pm-agent-track-mark-seen "buffer")
        (should (equal (alist-get 'status (gethash "buffer" pm-agent-track--sessions))
                       "idle"))))))

(ert-deftest pm-agent-track-confirms-plain-working-to-idle ()
  (with-temp-buffer
    (let ((pm-agent-track--sessions (make-hash-table :test 'equal))
          (pm-agent-track--identity '((session_id . "s1"))))
      (cl-letf (((symbol-function 'pm-agent-track--selected-p) (lambda () nil))
                ((symbol-function 'pm-agent-track--schedule-title) #'ignore)
                ((symbol-function 'pm-agent-track--schedule-idle) #'ignore)
                ((symbol-function 'pm-agent-track--project) (lambda (_) "demo")))
        (pm-agent-track--publish "buffer" "claude" '(:state "working") "claude")
        (pm-agent-track--accept "buffer" "claude" '(:state "idle") "claude" nil)
        (should (equal (alist-get 'status (gethash "buffer" pm-agent-track--sessions))
                       "working"))
        (dotimes (_ 3)
          (pm-agent-track--accept "buffer" "claude" '(:state "idle") "claude" t))
        (should (equal (alist-get 'status (gethash "buffer" pm-agent-track--sessions))
                       "done"))))))

(provide 'pm-agent-tracker-test)

;;; pm-agent-tracker-test.el ends here
