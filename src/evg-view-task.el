;;; -*- lexical-binding: t; -*-

(provide 'evg-view-task)

(require 'cl-lib)

(require 'evg-ui)
(require 'evg-util)

(defvar-local evg-build-variant nil)
(defvar-local evg-current-task nil)
(defvar-local evg-view-task-patch-title nil)

(cl-defstruct evg-task
  id
  display-name
  build-variant-display-name
  start-time
  finish-time
  status
  execution
  all-log
  task-log
  agent-log
  system-log
  event-log
  tests)

(defun evg-task-is-failed (task)
  (evg-status-failed-p (evg-task-status task)))

(defun evg-task-is-in-progress (task)
  (string= (evg-task-status task) evg-status-started))

(defun evg-task-is-undispatched (task)
  (string= (evg-task-status task) evg-status-undispatched))

(cl-defstruct evg-task-test
  file-name
  status
  log-url
  start-time
  finish-time)

(defun evg-task-test-parse (test-data)
  (make-evg-task-test
   :file-name (evg--gethash test-data "testFile")
   :status (evg--gethash test-data "status")
   :start-time (evg--gethash test-data "startTime")
   :finish-time (evg--gethash test-data "endTime")
   :log-url (evg--gethash test-data "logs" "urlRaw")))

(defun evg-task-test-insert (test)
  (insert
   (propertize
    (format "%7s %s"
            (evg-status-text (evg-task-test-status test))
            (evg-task-test-file-name test))
    'evg-task-test test))
  (newline))

(defun evg-task-parse-graphql (data)
  (make-evg-task
   :id (evg--gethash data "id")
   :display-name (evg--gethash data "displayName")
   :build-variant-display-name (evg--gethash data "buildVariantDisplayName")
   :start-time (evg--gethash data "startTime")
   :finish-time (evg--gethash data "finishTime")
   :status (evg--gethash data "status")
   :execution (evg--gethash data "execution")
   :all-log (evg--gethash data "logs" "allLogLink")
   :task-log (evg--gethash data "logs" "taskLogLink")
   :agent-log (evg--gethash data "logs" "agentLogLink")
   :system-log (evg--gethash data "logs" "systemLogLink")
   :event-log (evg--gethash data "logs" "eventLogLink")
   :tests (seq-map 'evg-task-test-parse (evg--gethash data "tests" "testResults"))))

(defun evg-get-task (task-id)
  (evg-task-parse-graphql
   (evg--gethash
    (evg-api-graphql-request
     (format
      "{
         task(taskId: %S) {
           id,
           displayName,
           buildVariantDisplayName,
           startTime,
           finishTime,
           status,
           execution,
           logs {
             agentLogLink,
             allLogLink,
             eventLogLink,
             systemLogLink,
             taskLogLink,
           }
           tests {
             testResults {
               testFile,
               status,
               startTime,
               endTime,
               logs {
                 urlRaw,
               }
             }
           }
         }
       }"
      task-id))
    "task")))

(defface evg-view-task-title
  '((t
     :bold t
     :italic t
     :underline t
     :height 1.5))
  "view task title face"
  :group 'evg)

(defun evg-view-task-header-line (property value)
  (format
   "%-16s%s"
   (propertize
     (concat property ":")
     'face 'bold)
   (with-temp-buffer
     (setq fill-column (- (window-width) 26))
     (setq fill-prefix (make-string 16 ? ))
     (insert value)
     (fill-paragraph)
     (buffer-string))))

(defun evg-view-task-refresh ()
  (interactive)
  (evg-view-task
   (evg-task-id evg-current-task)
   evg-view-task-patch-title
   evg-previous-buffer))

(defun evg-view-current-task-logs ()
  "Switch to a buffer displaying the current task's logs"
  (message "Fetching task logs...")
  (evg-get-string-async
   (format "%s&text=true" (evg-task-task-log evg-current-task))
   (let ((task-buffer (current-buffer)))
    (lambda (logs)
      (with-current-buffer task-buffer
        (evg-view-logs
         (format "%s / %s" evg-view-task-patch-title (evg-current-task-full-name))
         logs))))))

(defun evg-current-task-abort ()
  "Abort the current task. This does not refresh the buffer because evg takes a little while
   to update the status to aborted anyways."
  (evg-api-post
   (format "tasks/%s/abort" (evg-task-id evg-current-task))
   (lambda (_)
     (message "Task aborted"))))

(defun evg-current-task-restart ()
  "Restarts the current task and refreshes the view-task buffer."
  (evg-api-post
   (format "tasks/%s/restart" (evg-task-id evg-current-task))
   (lambda (_)
     (message "Task restarted")
     (evg-view-task-refresh))))

(defun evg-view-test-at-point ()
  (interactive)
  (if-let ((test (get-text-property (point) 'evg-task-test)))
      (if (or (not (evg-task-test-log-url test)) (string= "" (evg-task-test-log-url test)))
          (message "no logs to view")
        (progn
          (message "Fetching test logs...")
          (evg-get-string-async
           (let ((log-url (evg-task-test-log-url test)))
             (if (string-prefix-p "http" log-url)
                 log-url
               (format "https://evergreen.mongodb.com/%s&text=true" log-url)))
           (let ((task-buffer (current-buffer)))
             (lambda (logs)
               (with-current-buffer task-buffer
                 (evg-view-logs
                  (format "%s / %s / %s"
                          evg-view-task-patch-title
                          (evg-current-task-full-name)
                          (evg-task-test-file-name test))
                  logs)))))))))

(defun evg-current-task-full-name ()
  (format "%s / %s"  evg-build-variant (evg-task-display-name evg-current-task)))

(defun evg-insert-task-header (task)
  (evg-ui-insert-header
   (list
    (cons "Task Name" (evg-task-display-name task))
    (cons "Build Variant" (evg-task-build-variant-display-name task))
    (cons "Execution" (format "%d" (1+ (evg-task-execution task))))
    (cons "Status" (evg-status-text (evg-task-status task)))
    (cons "Started at" (evg-date-string (evg-task-start-time task)))))
  (newline))

(defun evg-view-task (task-id &optional patch-title previous-buffer)
  (message "fetching task data")
  (let ((task (evg-get-task task-id)))
    (message "fetching data done")
    (let ((full-display-name (format "%s / %s" (evg-task-build-variant-display-name task) (evg-task-display-name task)))
          (prefix (if patch-title (concat patch-title " / ") "")))
      (switch-to-buffer (get-buffer-create (format "evg-view-task: %s%s" prefix full-display-name)))
      (evg-view-task-mode)
      (setq display-line-numbers nil)
      (read-only-mode -1)
      (erase-buffer)
      (setq-local evg-current-task task)
      (setq-local evg-previous-buffer previous-buffer)
      (setq-local evg-view-task-patch-title patch-title)

      (evg-insert-task-header task)

      (insert-button "View Task Logs" 'action (lambda (_) (evg-view-current-task-logs)))
      (newline)
      (when (evg-task-is-failed task)
        (insert-button "View Failure Details" 'action (lambda (_) (evg-view-failure-details (format "%s / %s" evg-view-task-patch-title (evg-current-task-full-name)) task))))
      (when (evg-task-is-in-progress task)
        (newline)
        (insert-button "Abort Task" 'action (lambda (_) (evg-current-task-abort))))
      (when (not (or (evg-task-is-undispatched task) (evg-task-is-in-progress task)))
        (newline)
        (insert-button "Restart Task" 'action (lambda (_) (evg-current-task-restart))))
      (newline 2)
      (let ((failed-tests
             (seq-filter
              (lambda (test) (string= "fail" (evg-task-test-status test)))
              (evg-task-tests task)))
            (passed-tests
             (seq-filter
              (lambda (test) (string= "pass" (evg-task-test-status test)))
              (evg-task-tests task))))
        (if (or failed-tests passed-tests)
            (progn
              (insert
               (format "Test Results (%s, %s):"
                       (propertize (format "%d passed" (length passed-tests)) 'face '('success . nil))
                       (propertize (format "%d failed" (length failed-tests)) 'face '('error . nil))))
              (newline)
              (seq-do 'evg-task-test-insert failed-tests)
              (seq-do 'evg-task-test-insert passed-tests))
          (insert (propertize "No test results to display." 'face 'italic)))))
    (read-only-mode)
    (goto-char (point-min))))

(defvar evg-view-task-mode-map nil "Keymap for evg-view-task buffers")

(progn
  (setq evg-view-task-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-view-task-mode-map
             (kbd "<RET>") 'evg-view-test-at-point
             "r" 'evg-view-task-refresh
             evg-back-key 'evg-back)))
  (define-key evg-view-task-mode-map (kbd "<RET>") 'evg-view-test-at-point)
  (define-key evg-view-task-mode-map (kbd "r") 'evg-view-task-refresh)
  (define-key evg-view-task-mode-map evg-back-key 'evg-back))

(define-derived-mode
  evg-view-task-mode
  fundamental-mode
  "Evergreen Task"
  "Major mode for evg-view-task buffer")

(define-derived-mode
  evg-view-logs-mode
  compilation-mode
  "Evergreen Logs"
  "Major mode for viewing evergreen logs")

(defvar evg-view-logs-mode-map nil "Keymap for evg-view-logs buffers")

(progn
  (setq evg-view-logs-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-view-logs-mode-map
             evg-back-key 'evg-back)))
  (define-key evg-view-logs-mode-map evg-back-key 'evg-back))

(defun evg-view-logs (buffer-name logs)
  (let ((back-buffer (current-buffer)))
    (switch-to-buffer (get-buffer-create (format "evg-view-logs: %s" buffer-name)))
    (message "Viewing logs for %s" buffer-name)
    (fundamental-mode)
    (read-only-mode -1)
    (insert logs)
    (evg-view-logs-mode)
    (setq-local evg-previous-buffer back-buffer)
    (setq-local header-line-format buffer-name)
    (goto-char (point-min))))

(cl-defstruct evg-failure-details
  note
  known-issues
  suspected-issues)

(defun evg-failure-details-parse (data)
  (make-evg-failure-details
   :note (evg--gethash data "note" "message")
   :known-issues (seq-map
                  'evg-issue-parse
                  (evg--gethash data "issues"))
   :suspected-issues (seq-map
                      'evg-issue-parse
                      (evg--gethash data "suspectedIssues"))))

(cl-defstruct evg-issue
  "A jira issue related to a task/test failure."
  key
  url
  summary
  confidence
  status
  resolution)

(defun evg-issue-parse (data)
  (let ((jira-fields (evg--gethash data "jiraTicket" "fields")))
    (make-evg-issue
     :key (evg--gethash data "issueKey")
     :url (evg--gethash data "url")
     :confidence (evg--gethash data "confidenceScore")
     :summary (evg--gethash jira-fields "summary")
     :status (evg--gethash jira-fields "status" "name")
     :resolution (evg--gethash jira-fields "resolutionName"))))

(define-derived-mode
  evg-failure-details-mode
  fundamental-mode
  "Evergreen Task Failure Details"
  "Major mode for viewing evergreen task failure details")

(defvar evg-failure-details-mode-map nil "Keymap for evg-failure-details buffers")

(progn
  (setq evg-failure-details-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-failure-details-mode-map
             evg-back-key 'evg-back
             (kbd "M-j") 'evg-failure-details-goto-next-link
             (kbd "M-k") 'evg-failure-details-goto-previous-link
             (kbd "<RET>") 'evg-failure-details-visit-link-at-point)))
  (define-key evg-failure-details-mode-map evg-back-key 'evg-back)
  (define-key evg-failure-details-mode-map (kbd "<RET>") 'evg-failure-details-visit-link-at-point)
  (define-key evg-failure-details-mode-map (kbd "M-n") 'evg-failure-details-goto-next-link)
  (define-key evg-failure-details-mode-map (kbd "M-p") 'evg-failure-details-goto-previous-link))

(defconst evg--issue-query-body
  "issueKey,
   url,
   confidenceScore,
   jiraTicket {
     fields {
       resolutionName,
       summary,
       status {
         name
       }
     }
   }")

(defun evg--failure-details-query (task-id)
  (format
   "{
      task(taskId: %S) {
        annotation {
          issues { %s }
          suspectedIssues { %s }
          note {
            message
          }
        }
      }
    }"
   task-id
   evg--issue-query-body
   evg--issue-query-body))

(defun evg-issue-status-text (issue)
  "Propertize the given status string appropriately according to the value of the status (e.g. green for \"Fixed\")."
  (let* ((text (or (evg-issue-resolution issue) (evg-issue-status issue)))
         (face (cond
                ((string-match-p "Fixed" text) 'success)
                ((string-match-p "\\(In Progress\\|Waiting\\)" text) 'warning)
                ((string-match-p "Open" text) 'error)
                (t 'shadow))))
    (propertize text 'face face)))

(defun evg-failure-details--get-issue-url-at-point ()
  (get-char-property (point) 'evg-issue-url))

(defun evg-failure-details--has-address-at-point ()
  (get-char-property (point) 'goto-address))

(defun evg-failure-details--has-link-at-point ()
  (or (not (eq (evg-failure-details--get-issue-url-at-point) nil))
      (evg-failure-details--has-address-at-point)))

(defun evg-failure-details-goto-link (travel-fn)
  ;; First, travel past any link that point is already on.
  ;; Then try to advance to the next one.
  (let ((initial-point (point)))
    (when (evg--advance-until travel-fn (lambda () (not (evg-failure-details--has-link-at-point))))
      (when (not (evg--advance-until travel-fn 'evg-failure-details--has-link-at-point))
        (message "No more links")
        (goto-char initial-point)))))

(defun evg-failure-details-goto-next-link ()
  "Move point to the next link in the failure-details buffer, if any."
  (interactive)
  (evg-failure-details-goto-link 'evg--try-forward-char))

(defun evg-failure-details-goto-previous-link ()
  "Move point to the previous link in the failure-details buffer, if any."
  (interactive)
  (evg-failure-details-goto-link 'evg--try-backward-char))

(defun evg-failure-details-visit-link-at-point ()
  (interactive)
  (cond
   ((when-let ((url (evg-failure-details--get-issue-url-at-point)))
      (browse-url url)))
   ((evg-failure-details--has-address-at-point) (goto-address-at-point))))

(defun evg-view-failure-details (buffer-name task)
  (let* ((back-buffer (current-buffer))
         (data (evg-api-graphql-request (evg--failure-details-query (evg-task-id task))))
         (failure-details (evg-failure-details-parse (evg--gethash data "task" "annotation"))))
    (switch-to-buffer (get-buffer-create (format "evg-failure-details: %s" buffer-name)))
    (fundamental-mode)
    (read-only-mode -1)
    (erase-buffer)

    (evg-insert-task-header task)

    (defun evg-issue-insert-link (issue)
      (insert "- ")
      (insert-button
       (evg-issue-key issue)
       'evg-issue-url
       (evg-issue-url issue)
       'action
       (lambda (button)
         (browse-url (button-get button 'evg-issue-url))))
      (newline))

    (defun evg-insert-issue-property (key value)
      (insert "    " (propertize (format "%s:" key) 'face 'italic) " " value)
      (newline))

    (insert (propertize "Known Issues" 'face 'bold))
    (newline 2)
    (seq-do
     (lambda (issue)
       (evg-issue-insert-link issue)
       (evg-insert-issue-property "Summary" (evg-issue-summary issue))
       (evg-insert-issue-property "Status" (evg-issue-status-text issue)))
     (evg-failure-details-known-issues failure-details))
    (when (eq (evg-failure-details-known-issues failure-details) nil)
      (insert (propertize "No known issues related to this failure." 'face 'italic 'rear-nonsticky '(face))))
    (newline)

    (insert (propertize "Suspected Issues" 'face 'bold))
    (newline 2)
    (seq-do
     (lambda (issue)
       (evg-issue-insert-link issue)
       (evg-insert-issue-property "Summary" (evg-issue-summary issue))
       (evg-insert-issue-property "Status" (evg-issue-status-text issue))
       (evg-insert-issue-property "Confidence in suggestion" (format "%.1f%%" (* (evg-issue-confidence issue) 100.0))))
     (evg-failure-details-suspected-issues failure-details))
    (when (eq (evg-failure-details-suspected-issues failure-details) nil)
      (insert (propertize "No suspected issues related to this failure." 'face 'italic 'rear-nonsticky '(face))))
    (newline 2)

    (insert (propertize "Note" 'face 'bold))
    (newline 2)
    (insert (evg-failure-details-note failure-details))

    (evg-failure-details-mode)
    (read-only-mode)
    (goto-address-mode)
    (setq-local evg-previous-buffer back-buffer)
    (goto-char (point-min))))
