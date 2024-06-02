;;; -*- lexical-binding: t; -*-

(provide 'evg-view-patch)

(require 'evg-view-task)
(require 'evg-ui)
(require 'evg-util)

(require 'cl-lib)

(defconst evg-patch-status-created "created")
(defconst evg-patch-status-started "started")
(defconst evg-patch-status-failed "failed")
(defconst evg-patch-status-success "succeeded")

(defvar-local evg-view-patch-patch nil)
(defvar-local evg-view-patch-tasks nil)
(defvar-local evg-view-patch-task-format 'text)
(defvar-local evg-view-patch-text-format-show-all-tasks nil
  "Whether to show all tasks when displaying in text mode or to filter out
tasks with certain statuses (e.g. tasks that succeeded).")

(cl-defstruct evg-patch
  id
  project-id
  description
  number
  revision
  status
  author
  activated
  create-time
  start-time
  finish-time)

(defun evg-patch-parse-graphql-response (patch)
  "Parse a patch() GrapQL query response into an evg-patch."
  (let ((times (gethash "time" patch)))
    (make-evg-patch
     :id (gethash "id" patch)
     :project-id (gethash "projectIdentifier" patch)
     :description (gethash "description" patch)
     :number (gethash "patchNumber" patch)
     :revision (gethash "githash" patch)
     :status (gethash "status" patch)
     :author (gethash "authorDisplayName" patch)
     :activated (gethash "activated" patch)
     :create-time (gethash "createTime" patch)
     :start-time (gethash "started" times)
     :finish-time (gethash "finished" times))))

(defun evg-patch-parse-version-graphql-response (version)
  "Parse a version() GraphQL query resposne into an evg-patch."
  (make-evg-patch
     :id (gethash "id" version)
     :project-id (gethash "projectIdentifier" version)
     :description (gethash "message" version)
     :number (when-let ((patch-data (gethash "patch" version)))
               (gethash "patchNumber" patch-data))
     :revision (gethash "revision" version)
     :status (gethash "status" version)
     :author (gethash "author" version)
     :create-time (gethash "createTime" version)
     :start-time (gethash "startTime" version)
     :finish-time (gethash "finishTime" version)))

(defun evg-patch-is-mainline-commit (patch)
  (not (evg-patch-number patch)))

(defun evg-patch-title (patch)
  (if-let ((patch-number (evg-patch-number patch)))
      (format "Patch %d - %S" (evg-patch-number patch) (evg-patch-description patch))
    (format "Revision %s - %S" (substring (evg-patch-revision patch) 0 7) (evg-patch-description patch))))

(defun evg-patch-abort (patch)
  "Abort the provided patch. This does not refresh the buffer."
  (evg-api-post
   (format "versions/%s/abort" (evg-patch-id patch))
   (lambda (_) (message "Aborted patch"))))

(defun evg-patch-restart (patch)
  "Restart the provided patch. This does refreshes the buffer."
  (evg-api-post
   (format "versions/%s/restart" (evg-patch-id patch))
   (lambda (_) (message "Restarted patch"))))

(defun evg-get-current-patch-tasks ()
  "Fetches full list of task results broken down by variant."
  (message "Fetching patch data...")
  (let ((buildvariants-data
         (evg-api-graphql-request
          (format
           "{
              version(versionId: %S) {
                buildVariants(options: {}) {
                  variant
                  displayName
                  tasks {
                    id
                    displayName
                    status
                  }
                }
              }
            }"
           (evg-patch-id evg-view-patch-patch)))))
    (message "Fetching patch data...done")
    (seq-map
     (lambda (variant-data)
       (let ((variant-display-name (gethash "displayName" variant-data)))
         (cons variant-display-name
               (seq-map
                (lambda (data) (evg-task-info-parse data variant-display-name))
                (gethash "tasks" variant-data)))))
     (gethash "buildVariants" (gethash "version" buildvariants-data)))))

(defun evg-current-patch-is-in-progress ()
  (or
   (string= evg-patch-status-started (evg-patch-status evg-view-patch-patch))
   (seq-some
    (lambda (variant-tasks)
      (seq-some (lambda (task) (string= evg-status-started (evg-task-info-status task))) (cdr variant-tasks)))
    evg-view-patch-tasks)))

(cl-defstruct evg-task-info id display-name status variant-display-name)

(defun evg-task-info-parse (data variant)
  (make-evg-task-info
   :id (gethash "id" data)
   :display-name (gethash "displayName" data)
   :status (gethash "status" data)
   :variant-display-name variant
  ))

(defun evg-task-at-point ()
  (or
   (get-text-property (point) 'evg-task-info)
   (get-text-property (point) 'evg-element-data)))

(defun evg-view-task-at-point ()
  (interactive)
  (if-let ((task (evg-task-at-point))
           (build-variant (evg-task-info-variant-display-name task)))
      (evg-view-task (evg-task-info-id task) (evg-patch-title evg-view-patch-patch) (current-buffer))))

(defun evg-switch-task-format ()
  (interactive)
  (evg-view-patch evg-view-patch-patch
                        (if (eq evg-view-patch-task-format 'text) 'grid 'text)
                        evg-view-patch-tasks))

(defun evg-view-patch-text-format-toggle-show-all-tasks ()
  (interactive)
  (when (eq evg-view-patch-task-format 'text)
    (setq-local evg-view-patch-text-format-show-all-tasks (not evg-view-patch-text-format-show-all-tasks))
    (evg-view-patch evg-view-patch-patch 'text evg-view-patch-tasks)))

(defun evg-insert-variant-tasks (tasks task-format)
  (if (eq task-format 'text)
      (let* ((not-passed-tasks (seq-filter (lambda (task)
                                             (let ((task-status (evg-task-info-status task)))
                                               (not (or (evg-status-passed-p task-status) (evg-status-unscheduled-p task-status)))))
                                           tasks))
             (filter-tasks (lambda (status-fn) (seq-filter (lambda (task) (funcall status-fn (evg-task-info-status task))) not-passed-tasks)))
             (failed-tasks (funcall filter-tasks 'evg-status-failed-p))
             (system-failed-tasks (funcall filter-tasks 'evg-status-system-failed-p))
             (known-issue-tasks (funcall filter-tasks 'evg-status-known-issue-p))
             (started-tasks (funcall filter-tasks 'evg-status-started-p))
             (other-tasks (cl-set-difference not-passed-tasks (append failed-tasks system-failed-tasks known-issue-tasks)))
             (ordered-tasks (append failed-tasks system-failed-tasks known-issue-tasks started-tasks other-tasks)))
        (seq-do
         (lambda (task)
           (insert
            (with-temp-buffer
              (insert (format "  %-14s %s"
                              (evg-status-text (evg-task-info-status task))
                              (evg-task-info-display-name task)))
              (put-text-property (point-min) (point-max) 'evg-task-info task)
              (put-text-property (point-min) (point-max) 'rear-nonsticky t)
              (buffer-string)))
           (newline))
         (if evg-view-patch-text-format-show-all-tasks
             (append ordered-tasks (cl-set-difference tasks ordered-tasks))
           ordered-tasks)))
    (insert
     (evg-grid-create
      ""
      (seq-map
       (lambda (task)
         (make-evg-grid-element
          :description (evg-task-info-display-name task)
          :status (evg-task-info-status task)
          :data task))
       tasks)))))

(defun evg-goto-next-task-failure ()
  "Move the point to the next task failure in the patch."
  (interactive)
  (evg-goto-failure (cond
                           ((eq evg-view-patch-task-format 'grid) 'evg--try-forward-char)
                           ((eq evg-view-patch-task-format 'text) (lambda () (= (forward-line) 0))))))

(defun evg-goto-previous-task-failure ()
  "Move the point to the previous task failure in the patch."
  (interactive)
  (evg-goto-failure (cond
                           ((eq evg-view-patch-task-format 'grid) 'evg--try-backward-char)
                           ((eq evg-view-patch-task-format 'text) (lambda () (= (forward-line -1) 0))))))

(defun evg-goto-failure (travel-fn)
  (when (not (evg--advance-until
              travel-fn
              (lambda ()
                (when-let* ((task (evg-task-at-point)) (status (evg-task-info-status task)))
                  (or (evg-status-failed-p status) (evg-status-system-failed-p status) (evg-status-known-issue-p status))))))
    (message "No more failures")))

(defun evg-view-patch-refresh ()
  (interactive)
  (message "Refreshing...")
  (let* ((id (evg-patch-id evg-view-patch-patch))
         (cb (lambda (patch)
               (message "Refreshing...done")
               (evg-view-patch patch)))
         (patch (if (evg-patch-number evg-view-patch-patch)
                    (evg-get-patch id cb)
                  (evg-get-version id cb))))))

(defun evg-view-patch (patch &optional task-format tasks prev-buffer)
  "Switch to a buffer displaying the results of the provided patch. Optionally specify the format to display the task
results (either 'text or 'grid) and a previous buffer that can be returned to."
  (switch-to-buffer
   (get-buffer-create
    (format "evg-view-patch: %s: %S"
            (evg-patch-title patch)
            (truncate-string-to-width (evg-patch-description patch) 50 nil nil t))))
  (read-only-mode -1)
  (unless (derived-mode-p 'evg-view-patch-mode)
    (evg-view-patch-mode))
  (setq display-line-numbers nil)
  (erase-buffer)
  (when prev-buffer (setq-local evg-previous-buffer prev-buffer))
  (setq-local evg-view-patch-patch patch)
  (setq-local evg-view-patch-tasks (or tasks (evg-get-current-patch-tasks)))
  (when task-format
    (setq-local evg-view-patch-task-format task-format))
  (setq-local global-hl-line-mode nil)
  (setq-local cursor-type 'hollow)

  (with-eval-after-load 'evil
    (setq-local evil-normal-state-cursor 'hollow))

  (cursor-intangible-mode)
  (cursor-sensor-mode)

  ;; header
  (evg-ui-insert-header
   (list
    (cons "Description" (car (split-string (evg-patch-description patch) "[\r\n]")))
    (when (not (evg-patch-is-mainline-commit patch))
      (cons "Patch ID" (evg-patch-id patch)))
    (if (evg-patch-number evg-view-patch-patch)
        (cons "Patch Number" (format "%d" (evg-patch-number evg-view-patch-patch)))
      (cons "Revision" (evg-patch-revision evg-view-patch-patch)))
    (cons "Author" (evg-patch-author evg-view-patch-patch))
    (cons "Status" (evg-status-text (evg-patch-status evg-view-patch-patch)))
    (cons "Created at" (evg-patch-create-time evg-view-patch-patch))))
  (newline)

  ;; restart/abort buttons
  (when (not (evg-patch-is-mainline-commit evg-view-patch-patch))
    (insert-button "Reconfigure tasks/variants"
                   'action (lambda (_) (evg-configure-patch evg-view-patch-patch evg-view-patch-tasks)))
    (newline))
  (let ((is-in-progress (evg-current-patch-is-in-progress)))
    (if is-in-progress
        (progn
          (insert-button "Abort patch" 'action (lambda (_) (evg-patch-abort evg-view-patch-patch)))
          (newline))
      (when (not (string= (evg-patch-status evg-view-patch-patch) evg-patch-status-created))
        (insert-button "Restart patch" 'action (lambda (_) (evg-patch-restart evg-view-patch-patch)))
        (newline))))
  (newline)

  ;; task results
  (seq-do
   (lambda (variant-tasks)
     (insert
      (with-temp-buffer
        (insert (format "%s" (car variant-tasks)))
        (add-text-properties (point-min) (point-max) (list 'face 'bold))
        (let* ((tasks (cdr variant-tasks))
               (count-tasks (lambda (status-fn) (seq-count (lambda (task) (funcall status-fn (evg-task-info-status task))) tasks)))
               (n-undispatched (funcall count-tasks 'evg-status-undispatched-p))
               (n-unscheduled (funcall count-tasks 'evg-status-unscheduled-p))
               (n-passed (funcall count-tasks 'evg-status-passed-p))
               (n-failed (funcall count-tasks 'evg-status-failed-p))
               (n-system-failed (funcall count-tasks 'evg-status-system-failed-p))
               (n-known-issue (funcall count-tasks 'evg-status-known-issue-p))
               (n-started (funcall count-tasks 'evg-status-started-p))
               (aggregate-stats (list)))
          (when (> n-passed 0)
            (setq aggregate-stats (cons (propertize (format "%s passed" n-passed) 'face 'success) aggregate-stats)))
          (when (> n-failed 0)
            (setq aggregate-stats (cons (propertize (format "%s failed" n-failed) 'face 'error) aggregate-stats)))
          (when (> n-system-failed 0)
            (setq aggregate-stats (cons (propertize (format "%s system failed" n-system-failed) 'face 'evg-status-text-system-failed) aggregate-stats)))
          (when (> n-known-issue 0)
            (setq aggregate-stats (cons (propertize (format "%s known issue" n-known-issue) 'face 'shadow) aggregate-stats)))
          (when (> n-started 0)
            (setq aggregate-stats (cons (propertize (format "%s started" n-started) 'face 'warning) aggregate-stats)))
          (when (> n-undispatched 0)
            (setq aggregate-stats (cons (propertize (format "%s undispatched" n-undispatched) 'face 'shadow) aggregate-stats)))
          (when (> n-unscheduled 0)
            (setq aggregate-stats (cons (propertize (format "%s unscheduled" n-unscheduled) 'face 'shadow) aggregate-stats)))
          (insert " (" (string-join (reverse aggregate-stats) ", ") ")"))
        (buffer-string)))
     (newline)
     (evg-insert-variant-tasks (cdr variant-tasks) task-format)
     (newline))
   evg-view-patch-tasks)
  (read-only-mode)
  (goto-char (point-min)))

(defvar evg-view-patch-mode-map nil "Keymap for evg-view-patch buffers")

(progn
  (setq evg-view-patch-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-view-patch-mode-map
             (kbd "<RET>") 'evg-view-task-at-point
             "r" 'evg-view-patch-refresh
             "d" 'evg-switch-task-format
             (kbd "M-j") 'evg-goto-next-task-failure
             (kbd "M-k") 'evg-goto-previous-task-failure
             evg-back-key 'evg-back)))
  (define-key evg-view-patch-mode-map (kbd "<RET>") 'evg-view-task-at-point)
  (define-key evg-view-patch-mode-map (kbd "r") 'evg-view-patch-refresh)
  (define-key evg-view-patch-mode-map (kbd "d") 'evg-switch-task-format)
  (define-key evg-view-patch-mode-map (kbd "M-n") 'evg-goto-next-task-failure)
  (define-key evg-view-patch-mode-map (kbd "M-p") 'evg-goto-previous-task-failure)
  (define-key evg-view-task-mode-map evg-back-key 'evg-back))

(define-derived-mode
  evg-view-patch-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-view-patch buffer")
