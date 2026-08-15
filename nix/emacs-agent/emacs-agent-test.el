;;; emacs-agent-test.el --- Tests for local agent tracking -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'emacs-agent-track)
(require 'emacs-agent)

(ert-deftest emacs-agent-rules-detects-all-supported-agents ()
  (should (equal (plist-get (emacs-agent-rules-detect
                             "claude" "" "⠋ Working" "") :state)
                 "working"))
  (should (equal (plist-get (emacs-agent-rules-detect
                             "codex"
                             "›\nAllow command?\npress enter to confirm or esc to cancel")
                            :state)
                 "blocked"))
  (should (equal (plist-get (emacs-agent-rules-detect
                             "cursor"
                             "Waiting for approval\nRun this command?\nRun (once) (y)")
                            :state)
                 "blocked")))

(ert-deftest emacs-agent-rules-honors-skip-state-update ()
  (let ((detection
         (emacs-agent-rules-detect
          "codex"
          "›\nShowing transcript\n↑/↓ to scroll pgup/pgdn to page home/end to jump q to quit\nesc to edit prev")))
    (should (plist-get detection :skip-state-update))
    (should (equal (plist-get detection :rule-id) "transcript_viewer"))))

(ert-deftest emacs-agent-rules-selects-prompt-box-body ()
  (let ((screen "old\n────────\n❯ prompt\n────────\nfooter"))
    (should (equal (emacs-agent-rules--prompt-box-body screen) "❯ prompt"))))

(ert-deftest emacs-agent-track-classifies-wrapper-process-groups ()
  (let ((records
         '((:pid 10 :comm "dbexec" :cmdline "dbexec isaac codex --")
           (:pid 11 :comm "codex" :cmdline "/nix/store/hash/bin/codex"))))
    (should (equal (emacs-agent-track--classify-processes records) "codex"))
    (should (equal (emacs-agent-track--foreground-label records) "codex")))
  (should (equal (emacs-agent-track--foreground-label
                  '((:pid 10 :comm "dbexec" :cmdline "dbexec")))
                 "dbexec"))
  (should (equal (emacs-agent-track--record-agent
                  '(:comm "node" :cmdline "/opt/claude-code/cli.js"))
                 "claude"))
  (should (equal (emacs-agent-track--record-agent '(:comm "agent" :cmdline "agent"))
                 "cursor")))

(ert-deftest emacs-agent-track-recovers-explicit-command-session-ids ()
  (let ((session "01a00308-6ad2-7632-a6d4-1233d6aa67a5"))
    (should
     (equal
      (emacs-agent-track--command-session-id
       "codex"
       `((:comm "dbexec"
          :cmdline ,(concat "isaac.dbexec codex -- resume " session))))
      session))
    (should
     (equal
      (emacs-agent-track--command-session-id
       "claude"
       `((:comm "claude" :cmdline ,(concat "claude --session-id " session))))
      session))
    (should-not
     (emacs-agent-track--command-session-id
      "codex" '((:comm "codex" :cmdline "codex --model gpt-5.6"))))))

(ert-deftest emacs-agent-track-finds-children-created-by-any-task ()
  (cl-letf (((symbol-function 'directory-files)
             (lambda (&rest _) '("/proc/7/task/7" "/proc/7/task/9")))
            ((symbol-function 'insert-file-contents)
             (lambda (file &rest _)
               (insert (pcase file
                         ("/proc/7/task/7/children" "8 10")
                         ("/proc/7/task/9/children" "10 11")))
               nil)))
    (should (equal (emacs-agent-track--process-children 7) '(8 10 11)))))

(ert-deftest emacs-agent-track-finds-agents-behind-wrapper-descendants ()
  (cl-letf (((symbol-function 'emacs-agent-track--process-record)
             (lambda (pid)
               (pcase pid
                 (1 '(:pid 1 :pgrp 1 :tpgid 20 :comm "zsh"))
                 (20 '(:pid 20 :pgrp 20 :tpgid 20 :comm "dbexec" :cmdline "dbexec"))
                 (21 '(:pid 21 :pgrp 20 :tpgid 20 :comm "python3" :cmdline "wrapper"))
                 (22 '(:pid 22 :pgrp 22 :tpgid 22 :comm "claude"
                       :cmdline "claude --session-id 00000000-0000-0000-0000-000000000022")))))
            ((symbol-function 'emacs-agent-track--process-children)
             (lambda (pid) (pcase pid (20 '(21)) (21 '(22)) (_ nil)))))
    (let ((records (emacs-agent-track--foreground-processes 1)))
      (should (equal (mapcar (lambda (record) (plist-get record :pid)) records)
                     '(20 21 22)))
      (should (equal (emacs-agent-track--classify-processes records) "claude")))))

(ert-deftest emacs-agent-track-recovers-fresh-codex-id-from-open-lock ()
  (let ((session "01a0042a-2b58-7e31-ba13-c4f51cbc5844"))
    (cl-letf (((symbol-function 'directory-files)
               (lambda (&rest _) '("/proc/42/fd/55")))
              ((symbol-function 'file-symlink-p)
               (lambda (_)
                 (concat "/home/user/.codex/thread-writer-locks/" session ".lock"))))
      (should
       (equal (emacs-agent-track--codex-open-session-id
               '((:pid 42 :comm "codex" :cmdline "codex")))
              session)))))

(ert-deftest emacs-agent-track-finds-foreground-process-group-leader ()
  (cl-letf (((symbol-function 'emacs-agent-track--process-record)
             (lambda (pid)
               (pcase pid
                 (1 '(:pid 1 :pgrp 1 :tpgid 20))
                 (20 '(:pid 20 :pgrp 20 :tpgid 20 :comm "codex"))))))
    (should (equal (emacs-agent-track--foreground-processes 1)
                   '((:pid 20 :pgrp 20 :tpgid 20 :comm "codex"))))))

(ert-deftest emacs-agent-track-ignores-idle-shell-process-group ()
  (cl-letf (((symbol-function 'emacs-agent-track--process-record)
             (lambda (_pid) '(:pid 1 :pgrp 1 :tpgid 1))))
    (should-not (emacs-agent-track--foreground-processes 1))))

(ert-deftest emacs-agent-track-initial-idle-is-seen-later-idle-is-done ()
  (with-temp-buffer
    (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
          (emacs-agent-track--identity '((session_id . "s1") (cwd . "/tmp"))))
      (cl-letf (((symbol-function 'emacs-agent-track--selected-p) (lambda () nil))
                ((symbol-function 'emacs-agent-track--schedule-title-refresh) #'ignore)
                ((symbol-function 'emacs-agent-track--project) (lambda (_) "demo")))
        (emacs-agent-track--publish "buffer" "claude" '(:state "idle") "claude")
        (should (equal (alist-get 'status (gethash "buffer" emacs-agent-track--sessions))
                       "idle"))
        (emacs-agent-track--publish "buffer" "claude" '(:state "working") "claude")
        (emacs-agent-track--publish "buffer" "claude" '(:state "idle" :visible-idle t)
                                 "claude")
        (should (equal (alist-get 'status (gethash "buffer" emacs-agent-track--sessions))
                       "done"))
        (emacs-agent-track-mark-seen "buffer")
        (should (equal (alist-get 'status (gethash "buffer" emacs-agent-track--sessions))
                       "idle"))))))

(ert-deftest emacs-agent-track-confirms-plain-working-to-idle ()
  (with-temp-buffer
    (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
          (emacs-agent-track--identity '((session_id . "s1"))))
      (cl-letf (((symbol-function 'emacs-agent-track--selected-p) (lambda () nil))
                ((symbol-function 'emacs-agent-track--schedule-title-refresh) #'ignore)
                ((symbol-function 'emacs-agent-track--schedule-idle) #'ignore)
                ((symbol-function 'emacs-agent-track--project) (lambda (_) "demo")))
        (emacs-agent-track--publish "buffer" "claude" '(:state "working") "claude")
        (emacs-agent-track--accept "buffer" "claude" '(:state "idle") "claude" nil)
        (should (equal (alist-get 'status (gethash "buffer" emacs-agent-track--sessions))
                       "working"))
        (dotimes (_ 3)
          (emacs-agent-track--accept "buffer" "claude" '(:state "idle") "claude" t))
        (should (equal (alist-get 'status (gethash "buffer" emacs-agent-track--sessions))
                       "done"))))))

(ert-deftest emacs-agent-buffer-id-is-stable-and-opaque ()
  (with-temp-buffer
    (let ((first (emacs-agent-track-ensure-buffer-id)))
      (should (= (length first) 24))
      (should (equal first (emacs-agent-track-ensure-buffer-id))))))

(ert-deftest emacs-agent-record-includes-run-name-and-revision ()
  (with-temp-buffer
    (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
          (emacs-agent-track--identity nil)
          (emacs-agent-track--run-id "run-one")
          (emacs-agent-id "buffer-one")
          (emacs-agent-name "worker"))
      (cl-letf (((symbol-function 'emacs-agent-track--selected-p) (lambda () t))
                ((symbol-function 'emacs-agent-track--project) (lambda (_) "demo")))
        (emacs-agent-track--publish "buffer-one" "codex" '(:state "idle") "codex")
        (let ((record (gethash "buffer-one" emacs-agent-track--sessions)))
          (should (equal (alist-get 'id record) "buffer-one"))
          (should (equal (alist-get 'run_id record) "run-one"))
          (should (equal (alist-get 'name record) "worker"))
          (should (equal (alist-get 'kind record) "codex"))
          (should (equal (alist-get 'title record) "worker"))
          (should (equal (alist-get 'title_source record) "launch-name"))
          (should (= (alist-get 'revision record) 1)))
        (emacs-agent-track--publish "buffer-one" "codex" '(:state "working") "codex")
        (should (= (alist-get 'revision
                              (gethash "buffer-one" emacs-agent-track--sessions))
                   2))))))

(ert-deftest emacs-agent-record-uses-resume-id-when-hook-identity-is-missing ()
  (with-temp-buffer
    (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
          (emacs-agent-track--identity nil)
          (emacs-agent-track--run-id "run-one")
          (emacs-agent-id "buffer-one")
          (session "01a00308-6ad2-7632-a6d4-1233d6aa67a5")
          scheduled)
      (cl-letf (((symbol-function 'emacs-agent-track--selected-p) (lambda () t))
                ((symbol-function 'emacs-agent-track--project) (lambda (_) "demo"))
                ((symbol-function 'emacs-agent-track--schedule-title-refresh)
                 (lambda (&optional _) (setq scheduled t))))
        (emacs-agent-track--publish
         "buffer-one" "codex" '(:state "idle") "dbexec"
         `((:comm "dbexec"
            :cmdline ,(concat "isaac.dbexec codex -- resume " session))))
        (let ((record (gethash "buffer-one" emacs-agent-track--sessions)))
          (should (equal (alist-get 'vendor_session_id record) session))
          (should scheduled))))))

(ert-deftest emacs-agent-native-title-updates-display-but-not-control-name ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
        (notifications 0))
    (puthash "buffer-one"
             '((id . "buffer-one") (run_id . "run-one")
               (name . "worker") (kind . "codex")
               (vendor_session_id . "session-one")
               (title . "worker") (title_source . "launch-name")
               (revision . 1))
             emacs-agent-track--sessions)
    (cl-letf (((symbol-function 'emacs-agent-track--notify)
               (lambda () (cl-incf notifications))))
      (emacs-agent-track--apply-native-title
       '((id . "buffer-one") (run_id . "run-one") (kind . "codex")
         (session_id . "session-one") (title . "Native title")
         (source . "codex-explicit")))
      (let ((record (gethash "buffer-one" emacs-agent-track--sessions)))
        (should (equal (alist-get 'title record) "Native title"))
        (should (equal (alist-get 'title_source record) "codex-explicit"))
        (should (equal (alist-get 'name record) "worker"))
        (should (= (alist-get 'revision record) 2))
        (should (= notifications 1)))
      ;; A stale response from the previous run must not rename its replacement.
      (emacs-agent-track--apply-native-title
       '((id . "buffer-one") (run_id . "old-run") (kind . "codex")
         (session_id . "session-one") (title . "Stale title")
         (source . "codex-explicit")))
      (should (equal (alist-get 'title
                                (gethash "buffer-one" emacs-agent-track--sessions))
                     "Native title"))
      (should (= notifications 1)))))

(ert-deftest emacs-agent-native-title-cursor-does-not-create-visible-revision ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal))
        (notifications 0))
    (puthash "buffer-one"
             '((id . "buffer-one") (run_id . "run-one") (kind . "claude")
               (vendor_session_id . "session-one")
               (title . "Native title") (title_source . "claude-ai")
               (revision . 3))
             emacs-agent-track--sessions)
    (cl-letf (((symbol-function 'emacs-agent-track--notify)
               (lambda () (cl-incf notifications))))
      (emacs-agent-track--apply-native-title
       '((id . "buffer-one") (run_id . "run-one") (kind . "claude")
         (session_id . "session-one") (title . "Native title")
         (source . "claude-ai")
         (cursor . ((path . "/tmp/session") (offset . 42)))))
      (let ((record (gethash "buffer-one" emacs-agent-track--sessions)))
        (should (= (alist-get 'revision record) 3))
        (should (= (alist-get 'offset (alist-get 'title_cursor record)) 42))
        (should (zerop notifications))))))

(ert-deftest emacs-agent-native-title-batch-serializes-as-json-array ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (puthash "buffer-one"
             '((id . "buffer-one") (run_id . "run-one") (kind . "codex")
               (vendor_session_id . "session-one") (cwd . "/tmp")
               (title . "Fallback") (title_source . "agent-project"))
             emacs-agent-track--sessions)
    (let* ((requests (emacs-agent-track--title-requests))
           (encoded (json-serialize (vconcat requests)))
           (decoded (json-parse-string encoded :object-type 'alist
                                       :array-type 'list)))
      (should (= (length decoded) 1))
      (should (equal (alist-get 'session_id (car decoded)) "session-one")))))

(ert-deftest emacs-agent-resolves-id-and-unique-name ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (puthash "id-1" '((id . "id-1") (name . "worker")) emacs-agent-track--sessions)
    (should (equal (alist-get 'id (emacs-agent--resolve "id-1")) "id-1"))
    (should (equal (alist-get 'id (emacs-agent--resolve "worker")) "id-1"))
    (should-error (emacs-agent--resolve "missing") :type 'emacs-agent-api-error)))

(ert-deftest emacs-agent-validates-names-and-codex-profile ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (emacs-agent--validate-name "worker_1")
    (should-error (emacs-agent--validate-name "Worker")
                  :type 'emacs-agent-api-error)
    (should (equal (last (emacs-agent--launch-argv "codex" "demo" nil) 3)
                   '("--" "--profile" "emacs-agent")))
    (should-not (member "emacs-agent"
                        (emacs-agent--launch-argv
                         "codex" "demo" '("--profile" "custom"))))))

(ert-deftest emacs-agent-key-aliases-map-to-ghostel-encoding ()
  (should (equal (emacs-agent--key-spec "enter") '("return")))
  (should (equal (emacs-agent--key-spec "ctrl+c") '("c" . "ctrl")))
  (should (equal (emacs-agent--key-spec "pageup") '("prior")))
  (should-error (emacs-agent--key-spec "hyperdrive")
                :type 'emacs-agent-api-error))

(ert-deftest emacs-agent-read-selects-visible-and-recent-sources ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq-local emacs-agent-id "read-id")
      (setq-local ghostel--term 'fake-terminal)
      (puthash "read-id"
               '((id . "read-id") (run_id . "run") (kind . "claude")
                 (status . "idle") (revision . 1))
               emacs-agent-track--sessions)
      (cl-letf (((symbol-function 'ghostel--copy-all-text)
                 (lambda (_) "one\ntwo\nthree"))
                ((symbol-function 'ghostel-active-screen-text)
                 (lambda () "visible")))
        (should (equal
                 (alist-get 'text
                            (emacs-agent--read
                             '((target . "read-id") (source . "recent") (lines . 2))))
                 "two\nthree"))
        (should (equal
                 (alist-get 'text
                            (emacs-agent--read
                             '((target . "read-id") (source . "visible") (lines . 80))))
                 "visible"))))))

(ert-deftest emacs-agent-prompt-and-send-keys-use-ghostel-api ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)) seen)
    (with-temp-buffer
      (setq-local emacs-agent-id "input-id")
      (puthash "input-id"
               '((id . "input-id") (run_id . "run") (kind . "claude")
                 (status . "idle") (revision . 1))
               emacs-agent-track--sessions)
      (cl-letf (((symbol-function 'ghostel-paste-string)
                 (lambda (text) (push (list 'paste text) seen)))
                ((symbol-function 'ghostel-send-key)
                 (lambda (key &optional mods) (push (list 'key key mods) seen))))
        (emacs-agent--prompt '((target . "input-id") (text . "hello")))
        (emacs-agent--send-keys
         '((target . "input-id") (keys . ("esc" "ctrl+c"))))
        (should (member '(paste "hello") seen))
        (should (member '(key "return" nil) seen))
        (should (member '(key "escape" nil) seen))
        (should (member '(key "c" "ctrl") seen))))))

(ert-deftest emacs-agent-stop-revalidates-the-process-group ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)) signal)
    (with-temp-buffer
      (setq-local emacs-agent-id "stop-id")
      (setq-local ghostel--pid 10)
      (setq-local emacs-agent-track--run-id "run")
      (setq-local emacs-agent-track--foreground-pgrp 20)
      (puthash "stop-id"
               '((id . "stop-id") (run_id . "run") (kind . "codex")
                 (status . "working") (revision . 1))
               emacs-agent-track--sessions)
      (cl-letf (((symbol-function 'emacs-agent-track--foreground-processes)
                 (lambda (_) '((:pid 20 :pgrp 20 :comm "codex" :cmdline "codex"))))
                ((symbol-function 'signal-process)
                 (lambda (pid value) (setq signal (list pid value)))))
        (emacs-agent--stop '((target . "stop-id")))
        (should (equal signal '(-20 SIGTERM)))))))

(ert-deftest emacs-agent-identity-requires-native-kind-evidence ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq-local emacs-agent-id "identity-id")
      (setq-local ghostel--pid 10)
      (let* ((payload '((id . "identity-id") (kind . "claude")
                        (session_id . "session")))
             (encoded (base64-encode-string (json-serialize payload) t)))
        (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'emacs-agent-track--foreground-processes)
                   (lambda (_) '((:pid 20 :pgrp 20 :comm "codex" :cmdline "codex"))))
                  ((symbol-function 'emacs-agent-track--scan-buffer) #'ignore))
          (emacs-agent-report-identity encoded)
          (should-not emacs-agent-track--identity))
        (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                  ((symbol-function 'emacs-agent-track--foreground-processes)
                   (lambda (_) '((:pid 20 :pgrp 20 :comm "claude" :cmdline "claude"))))
                  ((symbol-function 'emacs-agent-track--scan-buffer) #'ignore))
          (pm-agent-track-identity encoded)
          (should (equal (alist-get 'session_id emacs-agent-track--identity) "session")))))))

(defun emacs-agent-test--decode-envelope (encoded)
  (json-parse-string
   (decode-coding-string (base64-decode-string encoded) 'utf-8)
   :object-type 'alist :array-type 'list :null-object nil :false-object nil))

(defun emacs-agent-test--wait-until (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE while servicing processes."
  (let ((deadline (+ (float-time) (or timeout 3))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (emacs-agent-track--process-tick))
    (funcall predicate)))

(ert-deftest emacs-agent-real-ghostel-hidden-lifecycle ()
  "Exercise hidden-buffer detection and lifecycle with a fake Codex TUI."
  :tags '(integration)
  (skip-unless (require 'ghostel nil t))
  (let* ((directory (make-temp-file "emacs-agent-integration-" t))
         (script (expand-file-name "codex-fake-agent" directory))
         (default-directory (file-name-as-directory directory))
         (ghostel-buffer-name "*emacs-agent-integration*")
         buffer id)
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "printf '\033[2J\033[H\033]2;Ready\007'\n"
              "while IFS= read -r line; do\n"
              "  case \"$line\" in\n"
              "    block*) printf '\033[2J\033[H\033]2;Action Required\007Allow command?\\npress enter to confirm or esc to cancel\\n' ;;\n"
              "    *) printf '\033[2J\033[H\033]2;⠋ Working\007• Working (esc to interrupt)\\n'; sleep 0.6; printf '\033[2J\033[H\033]2;Ready\007' ;;\n"
              "  esac\n"
              "done\n"))
    (set-file-modes script #o755)
    (unwind-protect
        (progn
          (emacs-agent-track-setup)
          (setq buffer (ghostel t))
          (with-current-buffer buffer
            (setq-local emacs-agent-name "integration")
            (setq id (emacs-agent-track-ensure-buffer-id))
            (ghostel-paste-string (shell-quote-argument script))
            (ghostel-send-key "return"))
          (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
          (should (emacs-agent-test--wait-until
                   (lambda () (gethash id emacs-agent-track--sessions)) 4))
          (should-not (get-buffer-window buffer t))
          (should (equal (alist-get 'kind (gethash id emacs-agent-track--sessions))
                         "codex"))
          (emacs-agent--prompt `((target . ,id) (text . "work")))
          (should (emacs-agent-test--wait-until
                   (lambda ()
                     (equal (alist-get 'status
                                       (gethash id emacs-agent-track--sessions))
                            "working"))))
          (should (emacs-agent-test--wait-until
                   (lambda ()
                     (equal (alist-get 'status
                                       (gethash id emacs-agent-track--sessions))
                            "done"))))
          (emacs-agent--prompt `((target . ,id) (text . "block")))
          (should (emacs-agent-test--wait-until
                   (lambda ()
                     (equal (alist-get 'status
                                       (gethash id emacs-agent-track--sessions))
                            "blocked"))))
          (should (string-match-p
                   "Allow command"
                   (alist-get 'text
                              (emacs-agent--read
                               `((target . ,id) (source . "visible")
                                 (lines . 80))))))
          (emacs-agent--stop `((target . ,id)))
          (should (emacs-agent-test--wait-until
                   (lambda () (not (gethash id emacs-agent-track--sessions))))))
      (when (buffer-live-p buffer)
        (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest emacs-agent-api-base64-wraps-success-and-errors ()
  (let ((emacs-agent-track--sessions (make-hash-table :test 'equal)))
    (let* ((request (base64-encode-string
                     (json-serialize '((op . "list"))) t))
           (response (emacs-agent-test--decode-envelope
                      (emacs-agent-api-call-base64 request))))
      (should (eq (alist-get 'ok response) t))
      (should (equal (alist-get 'type (alist-get 'result response)) "agents")))
    (let* ((request (base64-encode-string
                     (json-serialize '((op . "bogus"))) t))
           (response (emacs-agent-test--decode-envelope
                      (emacs-agent-api-call-base64 request))))
      (should-not (alist-get 'ok response))
      (should (equal (alist-get 'code (alist-get 'error response))
                     "invalid_request")))))

(provide 'emacs-agent-test)

;;; emacs-agent-test.el ends here
