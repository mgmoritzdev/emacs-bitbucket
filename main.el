(require 'oauth2)
(if (not (boundp 'moritz/bitbucket--v1))
    (load-file (expand-file-name "token.el")))

(defun moritz/list-repository (user callback &optional cbargs)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s?pagelen=10&page=2"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 (format endpoint user))
     callback
     cbargs)))

(defun moritz/get-repository (repo callback &optional cbargs)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s/%s")
        (user (cdr (assoc 'user repo)))
        (repo-slug (cdr (assoc 'repo-slug repo))))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 (format endpoint user repo-slug))
     callback
     cbargs)))

(defun moritz/parse-repositories (result)
  (let ((request-data (moritz/parse-json))
        (repo-names '()))
    (mapcar (lambda (element)
              (push (format "%s" (cdr (assoc 'name element))) repo-names))
            (cdr (assoc 'values request-data)))
    repo-names))

(defun moritz/select-repository (result &optional cbargs)
  (message cbargs)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/do-something (moritz/get-repo-uuid (moritz/get-repo-by-name data candidate))))))))
      (helm :sources '(repositories-helm-source)))))

(defun moritz/select-repo-and-run-action (result &optional callback)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (funcall callback (moritz/get-repo-by-name data candidate)))))))
      (helm :sources '(repositories-helm-source)))))

(defun moritz/select-repository-and-get-url (result)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/do-something (moritz/get-repo-ssh-url (moritz/get-repo-by-name data candidate))))))))
      (helm :sources '(repositories-helm-source)))))

;; parsing utils
(defun moritz/get-repo-uuid (repo)
  (cdr (assoc 'uuid repo)))

(defun moritz/do-something (uuid)
  (message uuid))

(defun moritz/get-repo-by-name (repos-vector name)
  (let ( ;; convert vector to list
        (repo-list (append (cdr (assoc 'values repos-vector)) nil))
        (value))
    (while (and repo-list (not value))
      (let ((item (car repo-list)))
        (if (string= name (cdr (assoc 'name item)))
            (setq value item))
        (setq repo-list (cdr repo-list))))
    value))

(defun moritz/get-repo-https-url (repo)
  (cdr (car (car (append (cdr (assoc 'clone
                                     (assoc 'links repo)))
                         nil)))))

(defun moritz/get-repo-ssh-url (repo)
  (cdr (car (car (cdr (append (cdr (assoc 'clone
                                          (assoc 'links repo)))
                              nil))))))

(defun moritz/parse-json ()
  (beginning-of-buffer)
  (search-forward "\n\n")
  (append '() (json-read)))

(defun moritz/http-status-code ()
  (message (buffer-substring 10 13)))

(defun moritz/repository-action-pullrequests (args)
  (let ((repo (car args)))
    (moritz/list-pullrequests (cdr (assoc 'username (assoc 'owner repo)))
                              (cdr (assoc 'name repo))
                              'moritz/select-pullrequest-and-run-action
                              '(moritz/run-pullrequest-action))))

(defun moritz/run-repository-action (repo)
  (moritz/helm-run-assoc-function
   '(("pull requests" . moritz/repository-action-pullrequests))
   `(,repo)))

(defun moritz/parse-and-run-repository-action (result)
  (let ((repo (moritz/parse-json)))
    (moritz/run-repository-action repo)))

(defun moritz/call-repository-action (action callback &optional cbargs)
  "List bitbucket pullrequests for user and repo"
  (let ((url-request-method "GET"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     action
     callback
     cbargs)))

;; for debugging purposes in case of Bad Request 400
;; (defun moritz/parse-json ()
;;   (beginning-of-buffer)
;;   (append '() (buffer-string)))


(defun bitbucket-actions ()
  "Apply action on the current repository.
The actions can be one of the following:
  - pull requests
    - approve
    - unapprove
"
  (interactive)
  (let  ((repo-data (get-user-and-repo-slug)))
    (moritz/get-repository repo-data
                           'moritz/parse-and-run-repository-action)))

;; tests and examples
;; (moritz/list-pull-requests "ptmtech" "portaltm.server")
;; (moritz/list-repository "mmoritz" 'moritz/select-repository '("test1"))

;; get repo and call a method on it: get-repo-uuid
;; (moritz/list-repository "mmoritz"
;;                        'moritz/select-repo-and-run-action
;;                        '((lambda (repo) (message (moritz/get-repo-uuid repo)))))

;; get repo and call a method on it: get-repo-ssh-url
;; (moritz/list-repository "mmoritz"
;;                        'moritz/select-repo-and-run-action
;;                        '((lambda (repo) (message (moritz/get-repo-ssh-url repo)))))
;; (moritz/list-repository "ptmtech"
;;                         'moritz/select-repo-and-run-action
;;                         '((lambda (repo) (message (moritz/get-repo-ssh-url repo)))))

;; (moritz/list-repository "mgmdevptm"
;;                         'moritz/select-repo-and-run-action
;;                         '(moritz/run-repository-action))

;; (moritz/get-repository '((user . "mgmdevptm") (repo-slug . "testrepo"))
;;                        'moritz/parse-and-run-repository-action)

;; (moritz/list-repository "mgmdevptm"
;;                         'moritz/select-repo-and-run-action
;;                         '(moritz/run-repository-action))


;; (moritz/list-repository "ptmtech" 'moritz/select-repository)

;; (moritz/get-remote "mmoritz" 'moritz/select-repository)
;; (moritz/list-repository "mmoritz" 'moritz/select-repository-and-get-url)


;; ideas
;; magit - add a button to bitbucket
;; pr - list pr's for this project
;; diff-pr - show the diff between merging branches
