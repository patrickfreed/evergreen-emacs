;;; -*- lexical-binding: t; -*-

(provide 'evg-grid)

(require 'cl-lib)

(require 'evg-ui)

(cl-defstruct evg-grid-element description status data)

(defface evg-grid-rectangle
  '((t
     :overline "black"))
  "Base face for rectangles in an evergreen patch results grid."
  :group 'evg)

(defface evg-grid-undispatched
  '((t
     :background "#bfbfbe"
     :inherit 'evg-grid-rectangle))
  "Face used for undispatched task results in an evergreen patch results grid."
  :group 'evg)

(defface evg-grid-system-failed
  '((t
     :background "#800080"
     :inherit 'evg-grid-rectangle))
  "Face used for tasks that encountered a system failure in an evergreen patch results grid."
  :group 'evg)

(defface evg-grid-failed
  `((t
     :background ,(face-attribute 'error :foreground)
     :inherit 'evg-grid-rectangle))
  "Face used for failed task results in an evergreen patch results grid."
  :group 'evg)

(defface evg-grid-succeeded
  `((t
     :background ,(face-attribute 'success :foreground)
     :inherit 'evg-grid-rectangle))
  "Face used for successful task results in an evergreen patch results grid."
  :group 'evg)

(defface evg-grid-started
  `((t
     :background ,(face-attribute 'warning :foreground)
     :inherit 'evg-grid-element))
  "Face used for tasks that are still executing in an evergreen patch results grid."
  :group 'evg)

(defun evg-grid-get-face (status)
  (cond
   ((string= evg-status-success status) 'evg-grid-succeeded)
   ((string-match-p evg-status-failed-regex status) 'evg-grid-failed)
   ((evg-status-known-issue-p status) 'evg-grid-failed)
   ((string= evg-status-started status) 'evg-grid-started)
   ((string-match-p evg-status-undispatched-regex status) 'evg-grid-undispatched)
   ((evg-status-unscheduled-p status) 'evg-grid-undispatched)
   ((evg-status-system-failed-p status) 'evg-grid-system-failed)
   (t (message "unknown status: %s" status) 'evg-grid-undispatched)
   ))

(defun evg-grid-create (_title elements)
  (with-temp-buffer
    (seq-do
     (lambda (element)
       (let ((avg-font-width (ceiling (/ (+ (window-font-width) 2) 2.0)))
             (max-width (window-width nil t)))
         ;; give extra column of spacing to ensure line breaks work with buffers of similar size without reloading
         (when (> (* (current-column) avg-font-width) (- max-width (* 2 (* avg-font-width 2))))
           (let ((old-column (current-column)))
             (newline)
             ;; need extra line between grid rectangles on terminal since they blend together due to no overline
             (when (not (display-graphic-p))
               ;; fill line with spaces so navigating downwards doesn't jump to front of line
               (insert (make-string (- old-column 1) 32))
               (newline)))))
       (insert
        (concat
         (propertize
          " "
          'face (evg-grid-get-face (evg-grid-element-status element))
          'help-echo (evg-grid-element-description element)
          'cursor-sensor-functions (list
                                    (lambda (_w _p action)
                                      (if (eq action 'entered)
                                          (message "%s - %s" (evg-grid-element-description element) (evg-grid-element-status element)))))
          'evg-element-data (evg-grid-element-data element))
         (propertize
          " "
          'display '(space . (:width (1)))
          'cursor-intangible t
          'rear-nonsticky t
          'front-sticky t))))
     elements)
    (buffer-string)))
