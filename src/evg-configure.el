;;; -*- lexical-binding: t; -*-

(provide 'evg-configure)

(require 'cl-lib)

(require 'evg-view-task)
(require 'evg-view-patch)
(require 'evg-ui)
(require 'evg-api)

(defvar-local evg-configure-target-patch nil)
(defvar-local evg-configure-variants nil)

(cl-defstruct evg-configure-task name selected location parent)

(defun evg-configure-task-is-visible (task)
  (not (evg-configure-variant-collapsed (evg-configure-task-parent task))))

(defun evg-configure-task-face (task)
  (if (evg-configure-task-selected task)
      'evg-configure-task-selected
    'evg-configure-variant-unselected))

(defun evg-configure-task-insert (task)
  "Insert the given task into the buffer and mark its location."
  (insert
   (with-temp-buffer
     (insert "  ")
     (evg-configure-insert-checkbox (evg-configure-task-selected task))
     (insert " ")
     (insert (evg-configure-task-name task))
     (add-text-properties (point-min) (point-max) (list 'evg-configure-task task))
     (add-face-text-property (point-min) (point-max) (evg-configure-task-face task))
     (buffer-string)))
  (setf (evg-configure-task-location task) (evg-configure-make-marker)))

(defun evg-configure-task-set-selected (task selected)
  "Toggles whether the provided task is selcted or not. This must be called when the point is on
   the same line as the task."
  (let ((is-selected (evg-configure-task-selected task)))
    (when (not (eq selected is-selected))
      (setf (evg-configure-task-selected task) selected)
      (when-let ((location (evg-configure-task-location task)))
        (save-excursion
          (read-only-mode -1)
          (goto-char (marker-position location))
          (kill-line)
          (evg-configure-task-insert task)
          (read-only-mode)))
      (evg-configure-variant-update-nselected (evg-configure-task-parent task))))
  (when (evg-configure-task-location task) (forward-line)))

(cl-defstruct evg-configure-variant display-name name tasks collapsed location)

(defun evg-configure-variant-nselected-tasks (variant)
  (length (evg-configure-variant-selected-tasks variant)))

(defun evg-configure-variant-selected-tasks (variant)
  (seq-filter
   'evg-configure-task-selected
   (evg-configure-variant-tasks variant)))

(defun evg-configure-variant-face (variant)
  "Get the face for a given variant line depending on the number of selected tasks for that variant."
  (let ((nselected (evg-configure-variant-nselected-tasks variant)))
    (if (> nselected 0)
        (if (= nselected (length (evg-configure-variant-tasks variant))) 'success 'warning)
      '('evg-configure-variant-unselected . 'bold))))

(defun evg-configure-variant-insert (variant)
  "Insert a variant line into the configure buffer."
  (insert
   (propertize
    (concat
     (if (evg-configure-variant-collapsed variant) "⮞" "⮟")
     " "
     (evg-configure-variant-display-name variant)
     (format
      " (%d/%d)"
      (evg-configure-variant-nselected-tasks variant)
      (length (evg-configure-variant-tasks variant))))
    'evg-configure-variant variant
    'face (evg-configure-variant-face variant)
    'rear-nonsticky t))
  (setf (evg-configure-variant-location variant) (evg-configure-make-marker)))

(defun evg-configure-variant-set-selected (variant selected)
  (seq-do
   (lambda (task)
     (evg-configure-task-set-selected task selected))
   (evg-configure-variant-tasks variant))
  (forward-line))

(defun evg-configure-variant-parse (data scheduled-tasks)
  "Parse a configure-variant from the given data, using the provided alist of display-name to evg-task-info
   to determine pre-selected tasks."
  (let*
      ((variant-scheduled-tasks (or (cdr (assoc-string (gethash "displayName" data) scheduled-tasks)) '()))
       (variant
        (make-evg-configure-variant
         :display-name (gethash "displayName" data)
         :name (gethash "name" data)
         :tasks (seq-map (lambda (task-name)
                           (make-evg-configure-task
                            :name task-name
                            :selected (seq-some
                                       (lambda (task)
                                         (string= task-name (evg-task-info-display-name task)))
                                       variant-scheduled-tasks)
                            :location nil
                            :parent nil))
                         (gethash "tasks" data))
         :collapsed t
         :location nil)))
    (seq-do
     (lambda (task)
       (setf (evg-configure-task-parent task) variant))
     (evg-configure-variant-tasks variant))
    variant))

(defun evg-configure-variant-update-nselected (variant)
  "Update the number of selected variants displayed on the variant line."
  (save-excursion 
    (read-only-mode -1)
    (goto-char (marker-position (evg-configure-variant-location variant)))
    (kill-line)
    (evg-configure-variant-insert variant)
    (read-only-mode)))

(defun evg-configure-make-marker ()
  "Make a marker that is at the beginning of the current line and updates properly in response to insertion."
  (let ((marker (make-marker)))
    (set-marker marker (line-beginning-position))
    (set-marker-insertion-type marker t)
    marker))

(defun evg-configure-insert-checkbox (is-selected)
  (insert
   (propertize
    (concat "[" (if is-selected "✔" " ") "]")
    'face 'org-checkbox)))

(defun evg-configure-patch (patch &optional scheduled-tasks)
  "Switch to a configuration buffer for the given evg-patch struct using the provided alist of display-name
   to evg-task-info to determine pre-scheduled tasks"
  (let ((prev-buffer (current-buffer)))
    (switch-to-buffer (get-buffer-create (format "evg-configure: %S" (evg-patch-description patch))))
    (read-only-mode -1)
    (evg-configure-mode)
    (erase-buffer)

    (setq-local evg-configure-target-patch patch)
    (setq-local evg-previous-buffer prev-buffer)

    (evg-ui-insert-header
     (list
      (cons "Description" (evg-patch-description patch))
      (cons "Patch Number" (number-to-string (evg-patch-number patch)))
      (cons "Patch ID" (evg-patch-id patch))
      (cons "Status" (evg-status-text (evg-patch-status patch)))
      (cons "Created at" (evg-date-string (evg-patch-create-time patch))))
     "Configure Patch")

    (newline)
    (setq-local evg-configure-variants
                (seq-map
                 (lambda (variant-data)
                   (let ((variant (evg-configure-variant-parse variant-data scheduled-tasks)))
                     (evg-configure-variant-insert variant)
                     (newline)
                     variant))
                 (evg-get-patch-variants (evg-patch-id patch))))
    (read-only-mode)
    (goto-char (point-min))))

(defun evg-configure-current-variant ()
  (get-text-property (point) 'evg-configure-variant))

(defun evg-configure-current-task ()
  (get-text-property (point) 'evg-configure-task))

(defun evg-configure-toggle-current-variant ()
  "Toggles the section at point"
  (interactive)
  (when-let ((variant (evg-configure-current-variant)))
    (read-only-mode -1)
    (save-excursion
      (setf (evg-configure-variant-collapsed variant) (not (evg-configure-variant-collapsed variant)))
      (if (evg-configure-variant-collapsed variant)
          (progn
            (forward-line)
            (seq-do
             (lambda (task)
               (setf (evg-configure-task-location task) nil)
               (kill-whole-line))
             (evg-configure-variant-tasks variant)))
        (forward-line)
        (seq-do
         (lambda (task)
           (evg-configure-task-insert task)
           (newline))
         (evg-configure-variant-tasks variant)))
      (goto-char (marker-position (evg-configure-variant-location variant)))
      (kill-line)
      (evg-configure-variant-insert variant))
    (read-only-mode)))

(defun evg-configure-set-select-at-point (selected)
  "Toggles the section at point"
  (if-let ((task (evg-configure-current-task)))
      (progn
        (evg-configure-task-set-selected task selected))
    (when-let ((variant (evg-configure-current-variant)))
        (evg-configure-variant-set-selected variant selected))))

(defun evg-configure-select-at-point ()
  (interactive)
  (evg-configure-set-select-at-point t))

(defun evg-configure-deselect-at-point ()
  (interactive)
  (evg-configure-set-select-at-point nil))

(defun evg-configure-schedule ()
  (interactive)
  (if-let
      ((selected-variants
        (seq-filter
         (lambda (variant) (> (evg-configure-variant-nselected-tasks variant) 0))
         evg-configure-variants)))
      (progn
        (message "Scheduling patch...")
        (evg-api-post
         (format "patches/%s/configure" (evg-patch-id evg-configure-target-patch))
         (lambda (_)
           (let ((configure-buffer (current-buffer)))
             (evg-view-patch evg-configure-target-patch)
             (kill-buffer configure-buffer)))
         (json-encode
          (list
           (cons "description" (evg-patch-description evg-configure-target-patch))
           (cons
            "variants"
            (seq-map
             (lambda (variant)
               (list
                (cons "id" (evg-configure-variant-name variant))
                (cons "tasks"
                      (seq-map 'evg-configure-task-name (evg-configure-variant-selected-tasks variant)))))
             selected-variants))))))))

(defun evg-get-patch-variants (patch-id)
  "Get list of variants and their associated tasks for the given patch."
  (let ((data
         (evg-api-graphql-request
          (format "{ patch(id: \"%s\") { project { variants { displayName,name,tasks }}}}" patch-id))))
    (gethash "variants" (gethash "project" (gethash "patch" data)))))

(defvar evg-configure-mode-map nil "Keymap for evg-configure buffers")

(progn
  (setq evg-configure-mode-map (make-sparse-keymap))
  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-configure-mode-map
             (kbd "<tab>") 'evg-configure-toggle-current-variant
             (kbd "m") 'evg-configure-select-at-point
             (kbd "u") 'evg-configure-deselect-at-point
             (kbd "x") 'evg-configure-schedule
             evg-back-key 'evg-back
             (kbd "r") (lambda ()
                         (interactive)
                         (evg-configure-patch evg-configure-target-patch '())))))

  (define-key evg-configure-mode-map (kbd "<tab>") 'evg-configure-toggle-current-variant)
  (define-key evg-configure-mode-map (kbd "m") 'evg-configure-select-at-point)
  (define-key evg-configure-mode-map (kbd "u") 'evg-configure-deselect-at-point)
  (define-key evg-configure-mode-map (kbd "x") 'evg-configure-schedule)
  (define-key evg-view-task-mode-map evg-back-key 'evg-back)

  (define-key evg-configure-mode-map (kbd "r") (lambda ()
                                                             (interactive)
                                                             (evg-configure-patch evg-configure-target-patch '()))))

(define-derived-mode
  evg-configure-mode
  fundamental-mode
  "Evergreen"
  "Major mode for evg-configure buffer")
  
(defface evg-configure-variant-unselected
  '((t (:inherit 'shadow)))
  "The face to use for variants that have no selected tasks"
  :group 'evg)

(defface evg-configure-task-selected
  '((t (:inherit 'success :bold nil)))
  "The face to use for a task that has been selected"
  :group 'evg)
