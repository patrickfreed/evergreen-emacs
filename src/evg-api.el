;;; -*- lexical-binding: t; -*-

(require 'url)
(require 'request)

(provide 'evg-api)

(defvar evg-user
  nil
  "The API user to use to authenticate with the Evergreen API. If unset, this will be read from ~/.evergreen.yml")
(defvar evg-api-key
  nil
  "The API key to use to authenticate with the Evergreen API. If unset, this will be read from ~/.evergreen.yml")

(defun evg-api-init ()
  "Load credentials from ~/.evergreen.yml if unset.
   This function may be invoked repeatedly, all but the first
   invocation are no-ops."
  (if (not evg-api-key)
      (with-temp-buffer
        (insert-file-contents "~/.evergreen.yml")
        (goto-char (point-min))
        (if (search-forward-regexp "api_key: \"?\\([a-z0-9]*\\)\"?$")
            (setq evg-api-key (match-string 1))
          (error "api key not included in ~/.evergreen.yml"))
        (goto-char (point-min))
        (if (search-forward-regexp "user: \"?\\(.*\\)\"?$")
            (setq evg-user (match-string 1))
          (error "api user not included in ~/.evergreen.yml"))
        )))

(defun evg-read-project-name ()
  "Get the project name from user input, defaulting to the current projectile project."
  (or
   (and (boundp 'evg-project-name) evg-project-name)
   (let*
       ((default-project-name
          (when (require 'projectile nil t)
            (when-let ((name-projectile (projectile-project-name)))
              (when (not (string= name-projectile "-"))
                name-projectile))))
        (prompt
         (cond
          (default-project-name (format "Project name (%s): " default-project-name))
          (t "Project name: "))))
     (read-string prompt nil nil default-project-name))))

(defun evg-api-url (path)
  (if (string-prefix-p "http" path)
      path
   (concat "https://evergreen.mongodb.com/api/rest/v2/" path)))

(defun evg-api-get-async (url success-callback &optional params)
  (request
    (evg-api-url url)
    :type "GET"
    :headers (list (cons "Api-User" evg-user) (cons "Api-Key" evg-api-key))
    :params params
    :success success-callback
    :parser 'json-read))

;; From: https://github.com/rcy/graphql-elisp/blob/master/graphql.el
;; TODO: write macro to collapse sync / async
(defun evg-api-graphql-request (query &optional variables operation-name)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (list (cons "Content-Type"  "application/json")
                (cons "Api-User" evg-user)
                (cons "Api-Key" evg-api-key)))
         (url-request-data
          (json-encode (list (cons "operationName" operation-name)
                             (cons "query" query)
                             (cons "variables" variables))))
         (buffer (url-retrieve-synchronously "https://evergreen.mongodb.com/graphql/query" t)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (response (json-read)))
        (when-let ((errors (gethash "errors" response)))
          (error "GraphQL error: %s" (json-encode errors)))
        (gethash "data" response)))))

(defun evg-api-graphql-request-async (query success-handler &optional variables operation-name)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (list (cons "Content-Type" "application/json")
                (cons "Api-User" evg-user)
                (cons "Api-Key" evg-api-key)))
         (url-request-data
          (json-encode (list (cons "operationName" operation-name)
                             (cons "query" query)
                             (cons "variables" variables)))))
    (url-retrieve
     "https://evergreen.mongodb.com/graphql/query"
     (lambda (status)
       (goto-char url-http-end-of-headers)
       (let* ((json-object-type 'hash-table)
              (json-array-type 'list)
              (response (json-read)))
         (when-let ((errors (gethash "errors" response)))
           (error "GraphQL error: %s" (json-encode errors)))
         (funcall success-handler (gethash "data" response))))
     nil
     'silent)))

(defun evg-get-string-async (url handler &optional params)
  "Perform an asynchronous GET request against the given URL, passing result as string to the provided handler."
  (request
    url
    :type "GET"
    :headers (list (cons "Api-User" evg-user) (cons "Api-Key" evg-api-key))
    :params params
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler data)))
    :parser 'buffer-string))

(defun evg-api-post (url handler &optional data)
  "Perform an asynchronous POST request against the given URL. Result will be passed to handler"
  (request
    (evg-api-url url)
    :type "POST"
    :headers (list
              (cons "Api-User" evg-user)
              (cons "Api-Key" evg-api-key)
              (cons "Content-Type"  "application/json"))
    :data data
    :success (cl-function (lambda (&key data &allow-other-keys) (funcall handler data)))
    :parser 'json-read))
