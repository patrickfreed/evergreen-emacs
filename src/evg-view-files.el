;;; -*- lexical-binding: t; -*-

(provide 'evg-view-files)

(require 'cl-lib)

(require 'evg-ui)
(require 'evg-util)

(cl-defstruct evg-file
  name
  link)

(defun evg-file-parse (data)
  (make-evg-file
   :name (evg--gethash data "name")
   :link (evg--gethash data "link")))

(defvar-local evg-grouped-files nil)

(define-derived-mode
  evg-view-files-mode
  fundamental-mode
  "Evergreen Files"
  "Major mode for viewing a task's files")

(defvar evg-view-files-mode-map nil "Keymap for evg-view-files buffers")

(progn
  (setq evg-view-files-mode-map (make-sparse-keymap))

  (with-eval-after-load 'evil
    (eval '(evil-define-key 'normal evg-view-files-mode-map
             evg-back-key 'evg-back
             (kbd "M-j") 'evg--ui-goto-next-link
             (kbd "M-k") 'evg--ui-goto-previous-link
             "r" 'evg--refresh-files-buffer)))
  (define-key evg-view-files-mode-map evg-back-key 'evg-back))

(defun evg--get-files (task-id)
  (let* ((data (evg-api-graphql-request
                (format
                 "{
                    task(taskId: %S) {
                      files {
                        groupedFiles {
                          taskName
                          files {
                            name
                            link
                          }
                        }
                      }
                    }
                  }"
                 task-id)))
         (files-data (evg--gethash data "task" "files" "groupedFiles")))
    (seq-map
     (lambda (grouped-file)
       (cons (evg--gethash grouped-file "taskName")
             (seq-map 'evg-file-parse (evg--gethash grouped-file "files"))))
     files-data)))

(defun evg--refresh-files-buffer ()
  (interactive)
  (evg-view-files evg-current-task))

(defun evg--view-file (file)
  (message "Downloading %s..." (evg-file-link file))
  (evg-get-string-async (evg-file-link file)
                        (lambda (s)
                          (switch-to-buffer (get-buffer-create (format "evg-view-file: %s" (evg-file-name file))))
                          (message "Downloading %s...done" (evg-file-link file))
                          (fundamental-mode)
                          (erase-buffer)
                          (insert s)
                          (goto-char (point-min)))))

(defun evg--view-file-at-point (f)
  (when-let ((file (get-text-property (point) 'evg-file)))
    (evg--view-file file)))

(defun evg-view-files (task)
  (message "Viewing files for %s" (evg-task-display-name task))
  (let ((back-buffer (current-buffer))
        (files (evg--get-files (evg-task-id task))))
    (switch-to-buffer (get-buffer-create (format "evg-view-files: %s" (evg-task-display-name task))))
    (evg-view-files-mode)
    (setq display-line-numbers nil)
    (read-only-mode -1)
    (erase-buffer)
    (setq-local evg-previous-buffer back-buffer)
    (setq-local evg-current-task task)

    (evg-insert-task-header task)

    (seq-do
     (lambda (file-group)
       (insert (propertize (car file-group) 'face 'bold))
       (newline)
       (seq-do
        (lambda (file)
          (insert "  - ")
          (if (string-suffix-p ".txt" (evg-file-link file))
              (insert-text-button (evg-file-name file) 'evg-file file 'action 'evg--view-file-at-point)
            (evg--ui-insert-link (evg-file-name file) (evg-file-link file)))
          (newline))
        (cdr file-group))
       (newline))
     files)

    (read-only-mode)
    (goto-char (point-min))))
  
