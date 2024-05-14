;;; -*- lexical-binding: t; -*-

(provide 'evg-ui)

(require 'seq)

;; back navigation
(defconst evg-back-key (kbd "q"))
(defvar-local evg-previous-buffer nil)

(defun evg-back ()
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-buffer evg-previous-buffer)
    (kill-buffer buf)))

;; statuses
(defconst evg-status-started "started")
(defconst evg-status-failed "failed")
(defconst evg-status-success "success")
(defconst evg-status-aborted "aborted")
(defconst evg-status-undispatched "undispatched")
(defconst evg-status-unscheduled "unscheduled")
(defconst evg-status-willrun "will-run")
(defconst evg-status-blocked "blocked")
(defconst evg-status-system-failure "system-failed")

(defconst evg-system-failed-color  "#800080" "The color used to indicate system failed")
(defface evg-status-text-system-failed
  `((t
     :foreground ,evg-system-failed-color
     :weight bold))
  "Face used for system failure status text"
  :group 'evg)

(defconst evg-status-failed-regex
  "\\(^fail\\|abort\\|timed.out\\)"
  "Regular expression matching any task status associated with failure")

(defconst evg-status-undispatched-regex
  (format "\\(%s\\|%s\\|%s\\)" evg-status-undispatched evg-status-willrun evg-status-blocked)
  "Regular expression matching any task status associated with not being dispatched yet")

;; matches system-failed, system-unresponsive
(defconst evg-status-system-failed-regex
  "^system-"
  "Regular expression matching any task status associated with system failure.")

(defconst evg-status-success-regex
  "\\(succ\\|pass\\)"
  "Regular expression matching any task or patch success status.")

(defun evg-status-passed-p (status) (string-match-p evg-status-success-regex status))
(defun evg-status-failed-p (status) (string-match-p evg-status-failed-regex status))
(defun evg-status-system-failed-p (status) (string-match-p evg-status-system-failed-regex status))
(defun evg-status-known-issue-p (status) (string-match-p "known-issue" status))
(defun evg-status-undispatched-p (status) (string-match-p evg-status-undispatched-regex status))
(defun evg-status-unscheduled-p (status) (string-match-p evg-status-unscheduled status))
(defun evg-status-started-p (status) (string-match-p "start" status))

(defun evg-status-text (status)
  "Propertize the given status string appropriately according to the value of the status (e.g. green for \"success\")."
  (let ((status-face
         (cond
          ((evg-status-passed-p status) 'success)
          ((evg-status-system-failed-p status) 'evg-status-text-system-failed)
          ((evg-status-failed-p status) 'error)
          ((evg-status-started-p status) 'warning)
          (t 'shadow))))
    (propertize status 'face status-face)))

(defun evg-date-string (date)
  "Get human-readable string version of the provided iso8601 date string"
  (condition-case nil
      (format-time-string "%b %e, %Y, %r" (encode-time (iso8601-parse date)))
    (error (format "invalid date: %S" date))))

;; from section-header in magit
(defface evg-header
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey95")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey20"))
  "Face for the header portion of an Evergreen buffer"
  :group 'evg)

(defface evg-header-title
  '((t
     :bold t
     :height 1.25
     :underline t
     :rear-nonsticky t))
  "Face for the title of the header of an Evergreen buffer"
  :group 'evg)

(defun evg-ui-insert-header (items &optional title)
  (let ((start (point)))
    (when title
      (insert (propertize title 'face 'evg-header-title))
      (insert " ")
      (newline 2))
    (seq-do
     (lambda (pair)
       (when pair
        (insert
         (format "%-16s"
                 (with-temp-buffer
                   (insert (format "%s:" (car pair)))
                   (add-text-properties (point-min) (point-max) (list 'face 'bold))
                   (buffer-string)))
         (with-temp-buffer
           (setq fill-column (- (window-width) 26))
           (setq fill-prefix (make-string 16 ? ))
           (insert (cdr pair))
           (fill-paragraph)
           (buffer-string))
         )
        (newline)))
     items)
    (let ((overlay (make-overlay start (point))))
      (overlay-put overlay 'face 'evg-header))))
  
