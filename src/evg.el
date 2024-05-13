;;; -*- lexical-binding: t; -*-

(defgroup evg nil "Customization group for evergreen related options." :group 'extensions)

(require 'evg-configure)
(require 'evg-view-patch)
(require 'evg-view-task)
(require 'evg-grid)
(require 'evg-api)
(require 'evg-ui)

(require 'json)
(require 'cl-lib)
(require 'seq)

(defvar-local evg-project-name nil)
(defvar-local evg-status-my-patches nil)
(defvar-local evg-status-recent-patches nil)
(defvar-local evg-status-waterfall-versions nil)

(defun evg-submit-patch (project-name description)
  "Submit a patch to the given project with the given description. Returns the patch's ID."
  (let* ((command (format "evergreen patch -p \"%s\" --yes --description \"%s\"" project-name description))
         (output (shell-command-to-string command)))
    (if (string-match "ID : \\([a-f0-9]+\\)" output)
        (match-string 1 output)
      (message "failed: %s" output))))

(defun evg-patch ()
  "Interactively submit a patch to the current project, prompting for a description"
  (interactive)
  (let ((description (read-string "Description: ")))
    (if-let (id (evg-submit-patch evg-project-name description))
        (progn
          (message "Submitted patch id: %s" id)
          (evg-get-patch id 'evg-configure-patch))
      (message "Patch failed"))))

(defun evg-status-setup ()
  "Prepare the buffer to display the status for a project."
  (read-only-mode -1)
  (setq display-line-numbers nil)
  (erase-buffer)
  (evg-ui-insert-header
   (list
    (cons "Project" evg-project-name)))
  (newline))

(defun evg-debug-print (thing)
  (interactive)
  (with-current-buffer (get-buffer-create "*debug*")
    (erase-buffer)
    (insert (json-encode thing))
    (json-pretty-print-buffer)
    (json-mode)))

(defun evg-status-get-waterfall-versions-callback (status-buffer)
  (cl-function
   (lambda (data)
     (with-current-buffer status-buffer
       (setq-local
        evg-status-waterfall-versions
        (seq-filter
         (lambda (x) x)
         (seq-map
          (lambda (version-data)
            (if-let ((version (gethash "version" version-data)))
                (evg-patch-parse-version-graphql-response version)))
          (gethash "versions" (gethash "mainlineCommits" data)))))
       (evg-status-display)))))

(defun evg-status-get-my-patches-callback (status-buffer)
  (cl-function
   (lambda (data)
     (with-current-buffer status-buffer
       (setq-local
        evg-status-my-patches
        (or
         (seq-take
          (seq-filter
           (lambda (patch) (string= (evg-patch-project-id patch) evg-project-name))
           (seq-map
            'evg-patch-parse-graphql-response
            (gethash "patches" (gethash "patches" (gethash "user" data)))))
          10)
         'no-patches))
       (evg-status-display)))))

(defun evg-status-get-recent-patches-callback (status-buffer)
  (cl-function
   (lambda (data)
     (with-current-buffer status-buffer
       (setq-local
        evg-status-recent-patches
        (seq-map
         'evg-patch-parse-graphql-response
         (gethash "patches" (gethash "patches" (gethash "project" data)))))
       (evg-status-display)))))

(defun evg-status-display ()
  (defun insert-patch (patch)
    (insert
     (propertize
      (concat
       (format "%9s" (evg-status-text (evg-patch-status patch)))
       " "
       (let ((author (evg-patch-author patch)))
         (concat
          (truncate-string-to-width
           (let ((description (car (split-string (evg-patch-description patch) "[\r\n]"))))
             (if (> (length description) 0)
                 description
               "no description"))
           (- (window-width) 20 (length author))
           nil nil t)
          " "
          (propertize (concat "by: " author) 'face '('italic 'shadow)))))
      'evg-patch-property patch))
    (insert " ")
    (newline))

  (when (and evg-status-recent-patches evg-status-waterfall-versions evg-status-my-patches)
    (message "Fetching status done")
    (evg-status-setup)
    (insert (propertize "Recent Commits:" 'face '('bold)))
    (newline)
    (seq-do 'insert-patch evg-status-waterfall-versions)
    (newline)
    (when (not (eq evg-status-my-patches 'no-patches))
      (insert (propertize "My Patches:" 'face '('bold)))
      (newline)
      (seq-do 'insert-patch evg-status-my-patches)
      (newline))
    (insert (propertize "Recent Patches:" 'face '('bold)))
    (newline)
    (seq-do 'insert-patch evg-status-recent-patches)
    (read-only-mode)
    (goto-char 0)))

;;;###autoload
(defun evg-status (project-name)
  "Open the evergreen status page for the given project"
  (interactive (list (evg-read-project-name)))
  (evg-api-init)
  (switch-to-buffer (get-buffer-create (format "evg-status: %s" project-name)))
  (evg-mode)
  (setq-local evg-project-name project-name)
  (evg-status-setup)

  (insert "Fetching status...")
  (read-only-mode)
  (evg-list-patches-async project-name)
  (goto-char (point-min)))

(defun evg-inspect-patch-at-point ()
  "Open a buffer viewing the results of the version under the point."
  (interactive)
  (when-let ((patch (get-text-property (point) 'evg-patch-property)))
    (if (and
         (not (evg-patch-is-mainline-commit patch))
         (string= (evg-patch-status patch) evg-patch-status-created)
         (eq (evg-patch-activated patch) :json-false))
        (evg-configure-patch patch)
      (evg-view-patch patch))))

(defun evg-status-refresh ()
  "Refetch the status of the recent patches for this project and update the status buffer accordingly."
  (interactive)
  (evg-status evg-project-name))

(defvar evg-mode-map nil "Keymap for evg-status page")

(progn
  (setq evg-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-mode-map
             (kbd "<RET>") 'evg-inspect-patch-at-point
             "p" 'evg-patch
             "r" 'evg-status-refresh)))
  (define-key evg-mode-map (kbd "<RET>") 'evg-inspect-patch-at-point)
  (define-key evg-mode-map (kbd "p") 'evg-patch)
  (define-key evg-mode-map (kbd "r") 'evg-status-refresh))

(define-derived-mode
  evg-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-status page")

(defconst evg-patch-grapql-query-body
  "
  id
  description
  status
  activated
  githash
  patchNumber
  authorDisplayName
  projectIdentifier
  time {
    submittedAt
    started
    finished
  }")
  
(defun evg-list-patches-async (project-name)
  "Fetch list of recent patches associated with the given project."
  (message "fetching %s evergreen status..." project-name)
  (evg-api-graphql-request-async
   (format
    "{
       project(projectIdentifier: %S) {
         patches(patchesInput: { limit: 10 }) {
           patches {
           %s
           }
         }
       }
     }"
    project-name evg-patch-grapql-query-body)
   (evg-status-get-recent-patches-callback (current-buffer)))
  (evg-api-graphql-request-async
   (format
    "{
       mainlineCommits(options: { projectIdentifier: %S, limit: 10 }) {
         versions {
           version {
             id
             projectIdentifier
             status
             message
             revision
             startTime
             finishTime
             createTime
             author
           }
         } 
       }
     }"
    project-name)
   (evg-status-get-waterfall-versions-callback (current-buffer)))
  ;; Use a large limit here since this includes all patches for a given user, not just those
  ;; associated with this project. The extra patches will be filtered out in the callback and
  ;; limited to 10 like the other sections.
  (evg-api-graphql-request-async
   (format
    "{
       user(userId: %S) {
         patches(patchesInput: { limit: 50 }) {
           patches {
           %s
           }
         } 
       }
     }"
    evg-user evg-patch-grapql-query-body)
   (evg-status-get-my-patches-callback (current-buffer))))

(defun evg-get-patch (patch-id handler)
  "Fetch the details of a single patch and pass the evg-patch object to the given handler."
  (evg-api-graphql-request-async
   (format
    "{
       patch(id: %S) {
       %s
       }
     }"
    patch-id evg-patch-grapql-query-body)
   (lambda (data)
     (let ((version (gethash "patch" data)))
       (funcall handler (evg-patch-parse-graphql-response version))))))

(defun evg-get-version (version-id handler)
  "Fetch the details of a single mainline commit and pass the evg-patch object to the given handler."
  (evg-api-graphql-request-async
   (format
    "{
       version(id: %S) {
         id
         message
         status
         revision
         author
         createTime
         startTime
         finishTime
       }
     }"
    version-id)
   (lambda (data)
     (let ((version (gethash "version" data)))
       (funcall handler (evg-patch-parse-version-graphql-response version))))))
