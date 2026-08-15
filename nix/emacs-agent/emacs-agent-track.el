;;; emacs-agent-track.el --- Local Ghostel agent registry -*- lexical-binding: t -*-

;;; Commentary:

;; Foreground process groups determine whether an agent is alive; Ghostel's
;; native active screen and OSC evidence determine state.  Agent hooks attach
;; identity only.  This keeps tracking local to Emacs and makes hidden Ghostel
;; buffers just as observable as displayed ones.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'emacs-agent-rules)
(require 'pm-project)

(declare-function ghostel-active-screen-text "ghostel" ())
(declare-function ghostel-terminal-title "ghostel" ())
(declare-function ghostel-terminal-progress "ghostel" ())
(defvar ghostel-terminal-update-generation)
(defvar ghostel-terminal-update-hook)
(defvar ghostel--pid)
(defvar server-name)

(defgroup emacs-agent-track nil
  "Local coding-agent state derived from Ghostel terminals."
  :group 'pm)

(defcustom emacs-agent-track-process-interval 0.3
  "Seconds between foreground-process scans."
  :type 'number :group 'emacs-agent-track)

(defcustom emacs-agent-track-update-delay 0.075
  "Seconds used to coalesce bursts of terminal updates."
  :type 'number :group 'emacs-agent-track)

(defcustom emacs-agent-track-descendant-limit 64
  "Maximum wrapper descendants inspected for one foreground process group."
  :type 'integer :group 'emacs-agent-track)

(defcustom emacs-agent-track-title-interval 1.0
  "Seconds between native title refreshes while identified agents are live."
  :type 'number :group 'emacs-agent-track)

(defcustom emacs-agent-track-native-title-program "@EMACS_AGENT_NATIVE_TITLE@"
  "One-shot helper that reads vendor-owned session titles."
  :type 'file :group 'emacs-agent-track)

(defvar emacs-agent-track-change-hook nil
  "Hook run after the local registry changes.")
(defvar emacs-agent-track--sessions (make-hash-table :test 'equal))
(defvar emacs-agent-track--process-timer nil)
(defvar emacs-agent-track--title-timer nil)
(defvar emacs-agent-track--title-process nil)
(defvar emacs-agent-track--title-dirty nil)
(defvar emacs-agent-track--setup-done nil)

(defvar-local emacs-agent-id nil
  "Opaque identity for the current managed Ghostel buffer.")
(defvar-local emacs-agent-name nil
  "Optional user-facing name reserved for the current agent buffer.")
(defvar-local emacs-agent-start-pending nil
  "Non-nil while a named buffer is waiting for its agent process.")
(defvar-local emacs-agent-track--identity nil)
(defvar-local emacs-agent-track--identity-pgrp nil)
(defvar-local emacs-agent-track--run-id nil)
(defvar-local emacs-agent-track--foreground-pgrp nil)
(defvar-local emacs-agent-track--update-timer nil)
(defvar-local emacs-agent-track--idle-timer nil)
(defvar-local emacs-agent-track--idle-start nil)
(defvar-local emacs-agent-track--idle-confirmations 0)
(defvar-local emacs-agent-track--last-generation -1)
(defvar-local emacs-agent-track--last-foreground nil)
(defvar-local emacs-agent-track--last-activity-at nil)

(defconst emacs-agent-track--idle-delay 0.1)
(defconst emacs-agent-track--idle-required 3)
(defconst emacs-agent-track--idle-cap 0.7)
(defconst emacs-agent-track--uuid-regexp
  "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}")

(defun emacs-agent-track--new-id ()
  "Return a new opaque local identifier."
  (substring
   (secure-hash 'sha256
                (format "%s\0%s\0%s\0%s" (emacs-pid) (float-time)
                        (random most-positive-fixnum) (current-buffer)))
   0 24))

(defun emacs-agent-track-ensure-buffer-id ()
  "Return the current buffer's stable Emacs agent identifier."
  (or emacs-agent-id (setq-local emacs-agent-id (emacs-agent-track--new-id))))

(defun emacs-agent-track-sessions ()
  "Return live local sessions as a list of alists."
  (let (sessions)
    (maphash (lambda (_key value) (push value sessions)) emacs-agent-track--sessions)
    sessions))

(defun emacs-agent-track-buffer-for-id (buffer-id)
  "Return the live buffer identified by BUFFER-ID."
  (and buffer-id
       (seq-find (lambda (buffer)
                   (equal (buffer-local-value 'emacs-agent-id buffer) buffer-id))
                 (buffer-list))))

(defun emacs-agent-track--notify ()
  "Notify registry consumers."
  (run-hooks 'emacs-agent-track-change-hook))

(defun emacs-agent-track--record-without-revision (record)
  "Return a copy of RECORD without its revision field."
  (assq-delete-all 'revision (copy-tree record)))

(defun emacs-agent-track--store (key record)
  "Store RECORD under KEY, advancing its revision when it changed."
  (let* ((existing (gethash key emacs-agent-track--sessions))
         (changed (not (equal (emacs-agent-track--record-without-revision existing)
                              record)))
         (revision (if changed
                       (1+ (or (alist-get 'revision existing) 0))
                     (or (alist-get 'revision existing) 1))))
    (setf (alist-get 'revision record) revision)
    (puthash key record emacs-agent-track--sessions)
    (when changed (emacs-agent-track--notify))
    record))

(defun emacs-agent-track--now-ms ()
  "Return wall-clock time in milliseconds."
  (floor (* 1000 (float-time))))

(defun emacs-agent-track--process-record (pid)
  "Return the process fields needed for PID, or nil if it exited."
  (when-let* ((attributes (process-attributes pid)))
    (list :pid pid
          :ppid (alist-get 'ppid attributes)
          :pgrp (alist-get 'pgrp attributes)
          :tpgid (alist-get 'tpgid attributes)
          :comm (or (alist-get 'comm attributes) "")
          :cmdline (or (alist-get 'args attributes) ""))))

(defun emacs-agent-track--process-children (pid)
  "Return the direct child process ids of PID."
  (condition-case nil
      (delete-dups
       (seq-mapcat
        (lambda (task)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents (expand-file-name "children" task))
                (mapcar #'string-to-number
                        (split-string (buffer-string) nil t)))
            (file-error nil)))
        (directory-files (format "/proc/%d/task" pid) t "\\`[0-9]+\\'")))
    (file-error nil)))

(defun emacs-agent-track--native-agent-process-p (record)
  "Return non-nil when RECORD is the vendor process, not a launch wrapper."
  (member (downcase (plist-get record :comm)) '("agent" "claude" "codex")))

(defun emacs-agent-track--foreground-processes (shell-pid)
  "Return SHELL-PID's foreground leader and bounded wrapper descendants.

The foreground group id is also its leader's pid.  Agent launch wrappers put
the agent name in that leader's command line in the common case.  Some dbexec
launches hide it in a short descendant chain, so follow only that process tree
and stop at native vendor executables.  This remains bounded and avoids the
global /proc sweep that can starve Emacs on process-heavy hosts."
  (when-let* ((shell (emacs-agent-track--process-record shell-pid)))
    (let ((tpgid (plist-get shell :tpgid)))
      (when (and (> tpgid 0) (/= tpgid (plist-get shell :pgrp)))
        (when-let* ((leader (emacs-agent-track--process-record tpgid)))
          (let ((records (list leader))
                (queue (emacs-agent-track--process-children tpgid))
                (seen (make-hash-table :test 'eql))
                (remaining emacs-agent-track-descendant-limit))
            (puthash tpgid t seen)
            (while (and queue (> remaining 0))
              (let ((pid (pop queue)))
                (unless (gethash pid seen)
                  (puthash pid t seen)
                  (cl-decf remaining)
                  (when-let* ((record (emacs-agent-track--process-record pid)))
                    (setq records (append records (list record)))
                    (unless (emacs-agent-track--native-agent-process-p record)
                      (setq queue
                            (append queue (emacs-agent-track--process-children pid))))))))
            records))))))

(defun emacs-agent-track--record-agent (record)
  "Classify one process RECORD as an agent, if possible."
  (let ((comm (downcase (plist-get record :comm)))
        (text (downcase (plist-get record :cmdline))))
    (cond
     ((or (string= comm "agent") (string-match-p "cursor-agent\\|cursor.*/agent" text))
      "cursor")
     ((or (string-match-p "claude" comm)
          (string-match-p "claude-code\\|@anthropic-ai/claude\\|[/ ]claude\\(?:[ /.-]\\|$\\)" text))
      "claude")
     ((or (string= comm "codex")
          (string-match-p "[/ ]codex\\(?:[ /.-]\\|$\\)\\|openai.*codex" text))
      "codex")
     (t nil))))

(defun emacs-agent-track--classify-processes (records)
  "Classify agent wrappers and descendants in RECORDS."
  (seq-some #'emacs-agent-track--record-agent records))

(defun emacs-agent-track--foreground-label (records)
  "Return a concise activity label for foreground RECORDS."
  (or (emacs-agent-track--classify-processes records)
      (when records
        (plist-get (car (sort (copy-sequence records)
                              (lambda (a b) (< (plist-get a :pid)
                                               (plist-get b :pid)))))
                   :comm))))

(defun emacs-agent-track--command-session-id (agent records)
  "Return AGENT's explicit session id from process RECORDS, if present.

Hooks remain authoritative for fresh sessions.  Resume commands already carry
the vendor id, so recovering it here also covers sessions launched through PM
or by hand without a vendor hook profile."
  (let* ((marker (pcase agent
                   ("codex" "\\(?:\\_<resume\\_>\\|--resume\\)")
                   ("claude" "\\(?:--resume\\|-r\\|--session-id\\)")
                   (_ "\\(?:--resume\\|-r\\|--session-id\\)")))
         (regexp (concat marker "[=[:space:]]+\\("
                         emacs-agent-track--uuid-regexp "\\)"))
         (case-fold-search t))
    (seq-some
     (lambda (record)
       (let ((command (plist-get record :cmdline)))
         (and (stringp command)
              (string-match regexp command)
              (downcase (match-string 1 command)))))
     records)))

(defun emacs-agent-track--codex-open-session-id (records)
  "Return the Codex session id exposed by open files in RECORDS."
  (let ((case-fold-search t)
        (regexp (concat "\\(?:/thread-writer-locks/\\|/rollout-[^/]*-\\)\\("
                        emacs-agent-track--uuid-regexp
                        "\\)\\(?:\\.lock\\|\\.jsonl\\)\\'")))
    (seq-some
     (lambda (record)
       (when (string= (downcase (plist-get record :comm)) "codex")
         (let ((directory (format "/proc/%d/fd" (plist-get record :pid))))
           (condition-case nil
               (seq-some
                (lambda (fd)
                  (when-let* ((target (file-symlink-p fd)))
                    (and (string-match regexp target)
                         (downcase (match-string 1 target)))))
                (directory-files directory t "\\`[0-9]+\\'"))
             (file-error nil)))))
     records)))

(defun emacs-agent-track--native-session-id (agent records)
  "Return AGENT's session id from native process evidence in RECORDS."
  (or (emacs-agent-track--command-session-id agent records)
      (and (equal agent "codex")
           (emacs-agent-track--codex-open-session-id records))))

(defun emacs-agent-track--project (identity)
  "Resolve a pm project name for IDENTITY or the current buffer."
  (let* ((cwd (or (alist-get 'cwd identity) default-directory))
         (container (and cwd (boundp 'pm-projects-dir)
                         (pm--container-of cwd))))
    (and container (file-name-nondirectory (directory-file-name container)))))

(defun emacs-agent-track--progress-text ()
  "Return current Ghostel OSC progress using Herdr's numeric representation."
  (when-let* ((progress (ghostel-terminal-progress)))
    (format "%d;%s"
            (pcase (car progress)
              ('remove 0) ('set 1) ('error 2) ('indeterminate 3) ('pause 4) (_ -1))
            (or (cdr progress) ""))))

(defun emacs-agent-track--selected-p ()
  "Return non-nil when the current buffer is selected."
  (eq (current-buffer) (window-buffer (selected-window))))

(defun emacs-agent-track--title-requests ()
  "Return one native-title request for every identified live session."
  (let (requests)
    (maphash
     (lambda (_key record)
       (let ((session-id (alist-get 'vendor_session_id record)))
         (when (and (stringp session-id) (not (string-empty-p session-id)))
           (push `((id . ,(alist-get 'id record))
                   (run_id . ,(alist-get 'run_id record))
                   (kind . ,(alist-get 'kind record))
                   (session_id . ,session-id)
                   (transcript_path . ,(alist-get 'transcript_path record))
                   (cwd . ,(alist-get 'cwd record))
                   (title . ,(alist-get 'title record))
                   (title_source . ,(alist-get 'title_source record))
                   (title_cursor . ,(alist-get 'title_cursor record)))
                 requests))))
     emacs-agent-track--sessions)
    requests))

(defun emacs-agent-track--apply-native-title (result)
  "Apply one native title RESULT if it still names the same live run."
  (when-let* ((key (alist-get 'id result))
              (record (gethash key emacs-agent-track--sessions)))
    (when (and (equal (alist-get 'run_id result) (alist-get 'run_id record))
               (equal (alist-get 'kind result) (alist-get 'kind record))
               (equal (alist-get 'session_id result)
                      (alist-get 'vendor_session_id record)))
      (let* ((title (alist-get 'title result))
             (source (alist-get 'source result))
             (cursor (alist-get 'cursor result))
             (changed (and (stringp title) (not (string-empty-p title))
                           (or (not (equal title (alist-get 'title record)))
                               (not (equal source (alist-get 'title_source record)))))))
        ;; Resolver cursors are internal bookkeeping and must not create API
        ;; revisions or sidebar redraws when the visible title is unchanged.
        (when cursor (setf (alist-get 'title_cursor record) cursor))
        (when changed
          (setf (alist-get 'title record) title
                (alist-get 'title_source record) source
                (alist-get 'title_updated_at record) (emacs-agent-track--now-ms)
                (alist-get 'revision record)
                (1+ (or (alist-get 'revision record) 0))))
        (puthash key record emacs-agent-track--sessions)
        (when changed (emacs-agent-track--notify))))))

(defun emacs-agent-track--apply-native-title-output (output)
  "Parse native-title helper OUTPUT and apply its current results."
  (condition-case err
      (let* ((payload (json-parse-string output :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil))
             (results (alist-get 'results payload)))
        (dolist (result results) (emacs-agent-track--apply-native-title result)))
    (error (message "emacs agent tracker: ignored malformed title output: %s"
                    (error-message-string err)))))

(defun emacs-agent-track--schedule-title-refresh (&optional delay)
  "Schedule a native title refresh after DELAY seconds."
  (cond
   ((process-live-p emacs-agent-track--title-process)
    (setq emacs-agent-track--title-dirty t))
   ((timerp emacs-agent-track--title-timer) nil)
   ((emacs-agent-track--title-requests)
    (setq emacs-agent-track--title-timer
          (run-at-time (or delay 0) nil #'emacs-agent-track--start-title-refresh)))))

(defun emacs-agent-track--title-filter (process output)
  "Accumulate native title OUTPUT from PROCESS."
  (process-put process 'emacs-agent-output
               (concat (or (process-get process 'emacs-agent-output) "") output)))

(defun emacs-agent-track--title-sentinel (process _event)
  "Finish a native title PROCESS and arrange the next bounded refresh."
  (when (memq (process-status process) '(exit signal))
    (when (and (zerop (process-exit-status process))
               (not (string-empty-p (or (process-get process 'emacs-agent-output) ""))))
      (emacs-agent-track--apply-native-title-output
       (process-get process 'emacs-agent-output)))
    (setq emacs-agent-track--title-process nil)
    (let ((immediate emacs-agent-track--title-dirty))
      (setq emacs-agent-track--title-dirty nil)
      (emacs-agent-track--schedule-title-refresh
       (if immediate 0 emacs-agent-track-title-interval)))))

(defun emacs-agent-track--start-title-refresh ()
  "Start one asynchronous, batched native title refresh."
  (setq emacs-agent-track--title-timer nil)
  (let ((requests (emacs-agent-track--title-requests)))
    (when (and requests (file-executable-p emacs-agent-track-native-title-program))
      (setq emacs-agent-track--title-process
            (make-process
             :name "emacs-agent-native-title"
             :command (list emacs-agent-track-native-title-program)
             :connection-type 'pipe :noquery t :buffer nil
             :filter #'emacs-agent-track--title-filter
             :sentinel #'emacs-agent-track--title-sentinel))
      (process-send-string emacs-agent-track--title-process
                           (concat (json-serialize (vconcat requests)) "\n"))
      (process-send-eof emacs-agent-track--title-process))))

(defun emacs-agent-track--clear-idle ()
  "Clear the current buffer's pending idle confirmation."
  (when (timerp emacs-agent-track--idle-timer) (cancel-timer emacs-agent-track--idle-timer))
  (setq emacs-agent-track--idle-timer nil emacs-agent-track--idle-start nil
        emacs-agent-track--idle-confirmations 0))

(defun emacs-agent-track--schedule-idle ()
  "Schedule the next 100ms idle confirmation."
  (unless (timerp emacs-agent-track--idle-timer)
    (let ((buffer (current-buffer)))
      (setq emacs-agent-track--idle-timer
            (run-at-time
             emacs-agent-track--idle-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq emacs-agent-track--idle-timer nil)
                   (emacs-agent-track--scan-buffer t t)))))))))

(defun emacs-agent-track--remove (key &optional clear-identity)
  "Remove registry KEY and optionally CLEAR-IDENTITY in the current buffer."
  (emacs-agent-track--clear-idle)
  (when clear-identity
    (setq emacs-agent-track--identity nil
          emacs-agent-track--identity-pgrp nil))
  (when (remhash key emacs-agent-track--sessions) (emacs-agent-track--notify))
  (when (and (zerop (hash-table-count emacs-agent-track--sessions))
             (timerp emacs-agent-track--title-timer))
    (cancel-timer emacs-agent-track--title-timer)
    (setq emacs-agent-track--title-timer nil)))

(defun emacs-agent-track--fallback-title (agent project name)
  "Return a temporary title for AGENT, PROJECT, and launch NAME."
  (or name (if project (format "%s — %s" agent project) agent)))

(defun emacs-agent-track--publish (key agent detection foreground &optional processes)
  "Publish DETECTION for AGENT and FOREGROUND under KEY.

PROCESSES is the native foreground process evidence used to recover an
agent's vendor session id when no hook identity is available."
  (setq emacs-agent-start-pending nil)
  (let* ((existing (gethash key emacs-agent-track--sessions))
         (identity emacs-agent-track--identity)
         (reported-session-id (alist-get 'session_id identity))
         (existing-session-id
          (and existing
               (equal (alist-get 'run_id existing) emacs-agent-track--run-id)
               (equal (alist-get 'agent existing) agent)
               (alist-get 'vendor_session_id existing)))
         (session-id (if (and (stringp reported-session-id)
                              (not (string-empty-p reported-session-id)))
                         reported-session-id
                       (or (and (stringp existing-session-id)
                                (not (string-empty-p existing-session-id))
                                existing-session-id)
                           (emacs-agent-track--native-session-id agent processes)
                           "")))
         (project (emacs-agent-track--project identity))
         (semantic (plist-get detection :state))
         (previous (alist-get 'semantic_status existing))
         (same-session (and existing
                            (equal (alist-get 'run_id existing) emacs-agent-track--run-id)
                            (equal (alist-get 'agent existing) agent)
                            (equal (alist-get 'vendor_session_id existing) session-id)))
         (status (cond
                  ((not (equal semantic "idle")) semantic)
                  ((and same-session (equal (alist-get 'status existing) "done")) "done")
                  ((or (null existing) (equal previous "idle") (emacs-agent-track--selected-p))
                   "idle")
                  (t "done")))
         (title (if same-session (alist-get 'title existing)
                  (emacs-agent-track--fallback-title agent project emacs-agent-name)))
         (title-source (if same-session (alist-get 'title_source existing)
                         (if emacs-agent-name "launch-name" "agent-project")))
         (record `((id . ,key) (buffer_id . ,key)
                   (run_id . ,emacs-agent-track--run-id)
                   (name . ,emacs-agent-name)
                   (kind . ,agent) (agent . ,agent)
                   (buffer_name . ,(buffer-name))
                   (vendor_session_id . ,session-id) (project . ,project)
                   (cwd . ,(expand-file-name default-directory))
                   (transcript_path . ,(alist-get 'transcript_path identity))
                   (title . ,title) (title_source . ,title-source)
                   (title_cursor . ,(and same-session (alist-get 'title_cursor existing)))
                   (title_updated_at . ,(and same-session
                                             (alist-get 'title_updated_at existing)))
                   (status . ,status)
                   (semantic_status . ,semantic)
                   (rule_id . ,(plist-get detection :rule-id))
                   (activity . ,foreground)
                   (last_activity_at . ,(or emacs-agent-track--last-activity-at
                                            (emacs-agent-track--now-ms))))))
    (emacs-agent-track--store key record)
    (when (and (not same-session) (not (string-empty-p session-id)))
      (emacs-agent-track--schedule-title-refresh 0))
    record))

(defun emacs-agent-track--accept (key agent detection foreground confirmation
                                      &optional processes)
  "Stabilize DETECTION for KEY before publishing it.

PROCESSES is forwarded as native identity evidence."
  (if (plist-get detection :skip-state-update)
      (unless (gethash key emacs-agent-track--sessions)
        (emacs-agent-track--publish key agent '(:state "idle" :rule-id nil)
                                    foreground processes))
    (let* ((existing (gethash key emacs-agent-track--sessions))
           (previous (alist-get 'semantic_status existing))
           (next (plist-get detection :state))
           (plain-idle (and (equal previous "working") (equal next "idle")
                            (not (plist-get detection :visible-idle)))))
      (cond
       ((not plain-idle)
        (emacs-agent-track--clear-idle)
        (emacs-agent-track--publish key agent detection foreground processes))
       ((null emacs-agent-track--idle-start)
        (setq emacs-agent-track--idle-start (float-time)
              emacs-agent-track--idle-confirmations 0)
        (emacs-agent-track--schedule-idle))
       ((not confirmation) nil)
       ((or (>= (- (float-time) emacs-agent-track--idle-start) emacs-agent-track--idle-cap)
            (>= (cl-incf emacs-agent-track--idle-confirmations) emacs-agent-track--idle-required))
        (emacs-agent-track--clear-idle)
        (emacs-agent-track--publish key agent detection foreground processes))
       (t (emacs-agent-track--schedule-idle))))))

(defun emacs-agent-track--scan-buffer (&optional force confirmation)
  "Scan the current Ghostel buffer's foreground process."
  (when (and (derived-mode-p 'ghostel-mode) (not (file-remote-p default-directory)))
    (let* ((key (bound-and-true-p emacs-agent-id))
           (pid (bound-and-true-p ghostel--pid))
           (foreground (and pid (emacs-agent-track--foreground-processes pid))))
      (when key
        (if (null foreground)
            (progn (setq emacs-agent-track--last-foreground nil
                         emacs-agent-track--foreground-pgrp nil
                         emacs-agent-track--run-id nil)
                   (emacs-agent-track--remove key t))
          (let* ((agent (emacs-agent-track--classify-processes foreground))
                 (generation (or (bound-and-true-p ghostel-terminal-update-generation) 0))
                 (foreground-id (sort (mapcar (lambda (record) (plist-get record :pid))
                                              foreground) #'<))
                 (pgrp (plist-get (car foreground) :pgrp))
                 (changed (not (equal foreground-id emacs-agent-track--last-foreground))))
            (when changed
              (setq emacs-agent-track--run-id (emacs-agent-track--new-id)
                    emacs-agent-track--foreground-pgrp pgrp
                    emacs-agent-track--last-activity-at (emacs-agent-track--now-ms))
              (unless (equal emacs-agent-track--identity-pgrp pgrp)
                (setq emacs-agent-track--identity nil
                      emacs-agent-track--identity-pgrp nil)))
            (setq emacs-agent-track--last-foreground foreground-id)
            (if (null agent)
                (progn
                  (setq emacs-agent-track--run-id nil
                        emacs-agent-track--foreground-pgrp nil)
                  (emacs-agent-track--remove key t))
              (when (or force changed (/= generation emacs-agent-track--last-generation)
                        (null (gethash key emacs-agent-track--sessions)))
                (setq emacs-agent-track--last-generation generation)
                (emacs-agent-track--accept
                 key agent
                 (emacs-agent-rules-detect agent (ghostel-active-screen-text)
                                        (ghostel-terminal-title)
                                        (emacs-agent-track--progress-text))
                 (emacs-agent-track--foreground-label foreground) confirmation
                 foreground)))))))))

(defun emacs-agent-track--process-tick ()
  "Refresh every Ghostel buffer's foreground process group."
  (condition-case err
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'ghostel-mode)
            (emacs-agent-track--scan-buffer))))
    (error (message "emacs agent tracker: process scan failed: %s"
                    (error-message-string err)))))

(defun emacs-agent-track--terminal-updated ()
  "Coalesce a Ghostel terminal update into a state scan."
  (setq emacs-agent-track--last-activity-at (emacs-agent-track--now-ms))
  (unless (timerp emacs-agent-track--update-timer)
    (let ((buffer (current-buffer)))
      (setq emacs-agent-track--update-timer
            (run-at-time
             emacs-agent-track-update-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq emacs-agent-track--update-timer nil)
                   (emacs-agent-track--scan-buffer t)))))))))

(defun emacs-agent-track--buffer-killed ()
  "Remove current Ghostel buffer from the registry."
  (when-let* ((key (bound-and-true-p emacs-agent-id)))
    (when (timerp emacs-agent-track--update-timer) (cancel-timer emacs-agent-track--update-timer))
    (emacs-agent-track--remove key t)))

(defun emacs-agent-track--ghostel-buffer-setup ()
  "Install buffer-local tracking cleanup."
  (emacs-agent-track-ensure-buffer-id)
  (add-hook 'kill-buffer-hook #'emacs-agent-track--buffer-killed nil t))

(defun emacs-agent-track-mark-seen (buffer-id)
  "Change BUFFER-ID's done state back to idle."
  (when-let* ((record (gethash buffer-id emacs-agent-track--sessions)))
    (when (equal (alist-get 'status record) "done")
      (setf (alist-get 'status record) "idle"
            (alist-get 'revision record)
            (1+ (or (alist-get 'revision record) 0)))
      (puthash buffer-id record emacs-agent-track--sessions)
      (emacs-agent-track--notify))))

(defun emacs-agent-track--selected-buffer-changed ()
  "Mark a selected Ghostel completion as seen."
  (when-let* ((buffer (window-buffer (selected-window)))
              (key (buffer-local-value 'emacs-agent-id buffer)))
    (emacs-agent-track-mark-seen key)))

(defun emacs-agent-track--server-name ()
  "Return this Emacs server's socket name."
  (if (and (boundp 'server-name) (stringp server-name)) server-name "server"))

(defun emacs-agent-track--seed-environment ()
  "Seed Emacs-owned identity variables into a Ghostel child."
  (setenv "EMACS_AGENT_SERVER" (emacs-agent-track--server-name))
  (setenv "EMACS_AGENT_ID" (emacs-agent-track-ensure-buffer-id)))

;;;###autoload
(defun emacs-agent-report-identity (encoded-json)
  "Attach base64 ENCODED-JSON identity to its live Ghostel buffer."
  (condition-case err
      (let* ((json (decode-coding-string (base64-decode-string encoded-json) 'utf-8))
             (identity (json-parse-string json :object-type 'alist :array-type 'list
                                          :null-object nil :false-object nil))
             (id (or (alist-get 'id identity) (alist-get 'buffer_id identity)))
             (kind (or (alist-get 'kind identity) (alist-get 'agent identity)))
             (buffer (emacs-agent-track-buffer-for-id id)))
        (when (and buffer (member kind '("claude" "codex" "cursor")))
          (with-current-buffer buffer
            ;; Identity without native screen evidence is deliberately ignored.
            (when (derived-mode-p 'ghostel-mode)
              (let* ((pid (bound-and-true-p ghostel--pid))
                     (foreground (and pid (emacs-agent-track--foreground-processes pid)))
                     (native-kind (emacs-agent-track--classify-processes foreground))
                     (pgrp (and foreground (plist-get (car foreground) :pgrp))))
                (when (equal kind native-kind)
                  (setf (alist-get 'agent identity) kind
                        (alist-get 'kind identity) kind
                        (alist-get 'buffer_id identity) id
                        (alist-get 'id identity) id)
                  (setq emacs-agent-track--identity identity
                        emacs-agent-track--identity-pgrp pgrp)
                  (emacs-agent-track--scan-buffer t)))))))
    (error (message "emacs agent tracker: ignored malformed identity: %s"
                    (error-message-string err)))))

;; Temporary compatibility for reporters installed by older PM releases.
(defalias 'pm-agent-track-identity #'emacs-agent-report-identity)

;;;###autoload
(defun emacs-agent-track-setup ()
  "Start local Ghostel agent tracking."
  (unless emacs-agent-track--setup-done
    (setq emacs-agent-track--setup-done t)
    (add-hook 'ghostel-mode-hook #'emacs-agent-track--ghostel-buffer-setup)
    (add-hook 'ghostel-pre-spawn-hook #'emacs-agent-track--seed-environment)
    (add-hook 'ghostel-terminal-update-hook #'emacs-agent-track--terminal-updated)
    (add-hook 'buffer-list-update-hook #'emacs-agent-track--selected-buffer-changed)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'ghostel-mode) (emacs-agent-track--ghostel-buffer-setup))))
    (setq emacs-agent-track--process-timer
          (run-at-time 0 emacs-agent-track-process-interval #'emacs-agent-track--process-tick))))

(provide 'emacs-agent-track)

;;; emacs-agent-track.el ends here
