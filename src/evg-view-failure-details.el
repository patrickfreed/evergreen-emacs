;;; -*- lexical-binding: t; -*-

(provide 'evg-view-failure-details)

(require 'cl-lib)

(require 'evg-ui)
(require 'evg-util)

(defvar-local evg-current-failure-details nil)

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

(defun evg-view-failure-details-refresh ()
  (interactive)
  (evg-view-failure-details (buffer-name) evg-current-task evg-previous-buffer))

(define-derived-mode
  evg-view-failure-details-mode
  fundamental-mode
  "Evergreen Task Failure Details"
  "Major mode for viewing evergreen task failure details")

(defvar evg-view-failure-details-mode-map nil "Keymap for evg-view-failure-details buffers")

(progn
  (setq evg-view-failure-details-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-view-failure-details-mode-map
             evg-back-key 'evg-back
             (kbd "M-j") 'evg-view-failure-details-goto-next-link
             (kbd "M-k") 'evg-view-failure-details-goto-previous-link
             (kbd "a k") 'evg-view-failure-details-add-known-issue
             (kbd "d k") 'evg-view-failure-details-remove-known-issue
             (kbd "a s") 'evg-view-failure-details-add-suspected-issue
             (kbd "d s") 'evg-view-failure-details-remove-suspected-issue
             "r" 'evg-view-failure-details-refresh
             (kbd "<RET>") 'evg-view-failure-details-visit-link-at-point)))
  (define-key evg-view-failure-details-mode-map evg-back-key 'evg-back)
  (define-key evg-view-failure-details-mode-map (kbd "<RET>") 'evg-view-failure-details-visit-link-at-point)
  (define-key evg-view-failure-details-mode-map (kbd "r") 'evg-view-failure-details-refresh)
  (define-key evg-view-failure-details-mode-map (kbd "M-n") 'evg-view-failure-details-goto-next-link)
  (define-key evg-view-failure-details-mode-map (kbd "M-p") 'evg-view-failure-details-goto-previous-link)
  (define-key evg-view-failure-details-mode-map (kbd "a k") 'evg-view-failure-details-add-known-issue)
  (define-key evg-view-failure-details-mode-map (kbd "d k") 'evg-view-failure-details-remove-known-issue)
  (define-key evg-view-failure-details-mode-map (kbd "a s") 'evg-view-failure-details-add-suspected-issue))
  (define-key evg-view-failure-details-mode-map (kbd "d s") 'evg-view-failure-details-remove-suspected-issue)

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

(defun evg-view-failure-details--get-issue-url-at-point ()
  (get-char-property (point) 'evg-issue-url))

(defun evg-view-failure-details--has-address-at-point ()
  (get-char-property (point) 'goto-address))

(defun evg-view-failure-details--has-link-at-point ()
  (or (not (eq (evg-view-failure-details--get-issue-url-at-point) nil))
      (evg-view-failure-details--has-address-at-point)))

(defun evg-view-failure-details-goto-link (travel-fn)
  ;; First, travel past any link that point is already on.
  ;; Then try to advance to the next one.
  (let ((initial-point (point)))
    (when (evg--advance-until travel-fn (lambda () (not (evg-view-failure-details--has-link-at-point))))
      (when (not (evg--advance-until travel-fn 'evg-view-failure-details--has-link-at-point))
        (message "No more links")
        (goto-char initial-point)))))

(defun evg-view-failure-details-goto-next-link ()
  "Move point to the next link in the failure-details buffer, if any."
  (interactive)
  (evg-view-failure-details-goto-link 'evg--try-forward-char))

(defun evg-view-failure-details-goto-previous-link ()
  "Move point to the previous link in the failure-details buffer, if any."
  (interactive)
  (evg-view-failure-details-goto-link 'evg--try-backward-char))

(defun evg-view-failure-details-visit-link-at-point ()
  (interactive)
  (cond
   ((when-let ((url (evg-view-failure-details--get-issue-url-at-point)))
      (browse-url url)))
   ((evg-view-failure-details--has-address-at-point) (goto-address-at-point))))

(defun evg-view-failure-details-add-known-issue (ticket)
  (interactive (list (read-string "Known issue Jira ticket: ")))
  (evg--add-annotation-issue ticket t 100)
  (message "Added known issue: %s" ticket))

(defun evg-view-failure-details-add-suspected-issue (ticket confidence)
  (interactive (list (read-string "Suspected issue Jira ticket: ") (read-number "Confidence score (0-100): ")))
  (evg--add-annotation-issue ticket :json-false confidence)
  (message "Added suspected issue: %s" ticket))

(defun evg--add-annotation-issue (ticket is-known confidence)
  (unless evg-current-task
    (error "Can only add known or suspected issue from a buffer associated with a task"))
  (evg-api-graphql-request
   "mutation AddAnnotationIssue($taskId: String!, $execution: Int!, $apiIssue: IssueLinkInput!, $isIssue: Boolean!) {
       addAnnotationIssue(taskId: $taskId, execution: $execution, apiIssue: $apiIssue, isIssue: $isIssue)
    }"
   (list (cons "taskId" (evg-task-id evg-current-task))
         (cons "execution" (evg-task-execution evg-current-task))
         (cons "apiIssue" (list (cons "confidenceScore" (/ confidence 100.0))
                                (cons "issueKey" ticket)
                                (cons "url" (concat "https://jira.mongodb.org/browse/" ticket))))
         (cons "isIssue" is-known))
   "AddAnnotationIssue")
  (when (derived-mode-p 'evg-view-failure-details-mode)
    (evg-failure-details-refresh)))

(defun evg-view-failure-details-remove-suspected-issue (ticket)
  (interactive (list
                (completing-read "Suspected issue to remove: "
                                 (seq-map
                                  'evg-issue-key
                                  (evg-failure-details-suspected-issues evg-current-failure-details)))))
  (evg--remove-annotation-issue ticket :json-false)
  (message "Removed suspected issue %s" ticket))

(defun evg-view-failure-details-remove-known-issue (ticket)
  (interactive (list
                (completing-read "Known issue to remove: "
                                 (seq-map
                                  'evg-issue-key
                                  (evg-failure-details-known-issues evg-current-failure-details)))))
  (evg--remove-annotation-issue ticket t)
  (message "Removed known issue %s" ticket))

(defun evg--remove-annotation-issue (ticket is-known)
  (unless evg-current-task
    (error "Can only remove known or suspected issue from a buffer associated with a task"))
  (evg-api-graphql-request
   "mutation RemoveAnnotationIssue($taskId: String!, $execution: Int!, $apiIssue: IssueLinkInput!, $isIssue: Boolean!) {
       removeAnnotationIssue(taskId: $taskId, execution: $execution, apiIssue: $apiIssue, isIssue: $isIssue)
    }"
   (list (cons "taskId" (evg-task-id evg-current-task))
         (cons "execution" (evg-task-execution evg-current-task))
         (cons "apiIssue" (list (cons "issueKey" ticket)
                                (cons "url" (concat "https://jira.mongodb.org/browse/" ticket))))
         (cons "isIssue" is-known))
   "RemoveAnnotationIssue")
  (evg-view-failure-details-refresh))

(defun evg-view-failure-details (buffer-name task &optional prev-buffer)
  (message "Fetching failure details...")
  (let* ((back-buffer (or prev-buffer (current-buffer)))
         (data (evg-api-graphql-request (evg--failure-details-query (evg-task-id task))))
         (failure-details (evg-failure-details-parse (evg--gethash data "task" "annotation"))))
    (message "Fetching failure details...done")
    (switch-to-buffer (get-buffer-create buffer-name))
    (fundamental-mode)
    (read-only-mode -1)
    (erase-buffer)

    (evg-insert-task-header task)

    (defun evg-issue-insert-link (issue)
      (insert "  - ")
      (insert-button
       (evg-issue-key issue)
       'evg-issue-url
       (evg-issue-url issue)
       'action
       (lambda (button)
         (browse-url (button-get button 'evg-issue-url))))
      (newline))

    (defun evg-insert-issue-property (key value)
      (insert "      " (propertize (format "%s:" key) 'face 'italic) " " value)
      (newline))

    (insert (propertize "Known Issues" 'face 'bold 'rear-nonsticky t))
    (newline 2)
    (seq-do
     (lambda (issue)
       (evg-issue-insert-link issue)
       (evg-insert-issue-property "Summary" (evg-issue-summary issue))
       (evg-insert-issue-property "Status" (evg-issue-status-text issue)))
     (evg-failure-details-known-issues failure-details))
    (when (eq (evg-failure-details-known-issues failure-details) nil)
      (insert (propertize "No known issues related to this failure." 'face 'italic 'rear-nonsticky '(face)))
      (newline))

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
      (insert (propertize "No suspected issues related to this failure." 'face 'italic 'rear-nonsticky '(face)))
      (newline))
    (newline)

    (insert (propertize "Note" 'face 'bold))
    (newline 2)
    (insert (evg-failure-details-note failure-details))

    (evg-view-failure-details-mode)
    (read-only-mode)
    (goto-address-mode)
    (setq-local evg-previous-buffer back-buffer)
    (setq-local evg-current-task task)
    (setq-local evg-current-failure-details failure-details)
    (goto-char (point-min))))
