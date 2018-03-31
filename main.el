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
    (moritz/get-repository-resource
     (cdr (assoc 'href ( assoc 'pullrequests (assoc 'links repo))))
     'moritz/select-pullrequest-and-run-action
     '(moritz/run-pullrequest-action))))

(defun moritz/repository-action-commits (args)
  (let ((repo (car args)))
    (moritz/get-repository-resource
     (cdr (assoc 'href ( assoc 'commits (assoc 'links repo))))
     'moritz/select-commits-and-run-action
     '(moritz/run-commits-action))))

(defun moritz/repository-action-branches (args)
  (let ((repo (car args)))
    (moritz/get-repository-resource
     (cdr (assoc 'href ( assoc 'branches (assoc 'links repo))))
     'moritz/select-branches-and-run-action
     '(moritz/run-branches-action))))

(defun moritz/run-repository-action (repo)
  (moritz/helm-run-assoc-function
   '(("List pull requests" . moritz/repository-action-pullrequests)
     ("Create pull request" . moritz/repository-action-create-pullrequest)
     ("Commits" . moritz/repository-action-commits)
     ("Branches" . moritz/repository-action-branches))
   `(,repo)))

(defun moritz/parse-and-run-repository-action (result)
  (let ((repo (moritz/parse-json)))
    (moritz/run-repository-action repo)))

(defun moritz/get-repository-resource (action callback &optional cbargs)
  "Get repository resource"
  (let ((request-method "GET"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     action
     callback
     cbargs)))

(defun moritz/create-pullrequest-callback (result)
  (message (format "%s" result)))

;; (cdr (assoc 'href (assoc 'pullrequests (assoc 'links repo))))
;; (moritz/repository-action-branches repo)
;; this will not work this way
(defun moritz/repository-action-create-pullrequest (args)
  (let ((repo (car args))
        (pullrequest-url (cdr (assoc 'href (assoc 'pullrequests (assoc 'links repo)))))
        (source-branch "dev")
        (destination-branch "homolog")
        (pullrequest-title "Dummy pull request"))
    (moritz/post-repository-resource
     pullrequest-url
     (json-encode `(("source" . (("branch" . (("name" . ,source-branch)))))
                    ("destination" . (("branch" . (("name" . ,destination-branch)))))
                    ("title" . ,pullrequest-title)))
     `(,(moritz/content-type-header "application/json"))
     ;; 'moritz/create-pullrequest-callback)))
     'moritz/diff-result)))

(defun moritz/post-repository-resource (action data headers callback &optional cbargs)
  "Get repository resource"
  (let ((request-method "POST")
        (request-data data)
        (request-extra-headers headers))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     action
     callback
     cbargs
     request-method
     request-data
     request-extra-headers)))

(defun bitbucket-actions ()
  "Apply action on the current repository.
The actions can be one of the following:
  - pull requests
    - approve
    - unapprove
  - commits
  - branches
  - merge
"
  (interactive)
  (let  ((repo-data (get-user-and-repo-slug)))
    (moritz/get-repository repo-data
                           'moritz/parse-and-run-repository-action)))

;; tests and examples
;; call from a file in some bitbucket repository: M-x bitbucket-actions

;; ideas
;; magit - add a button to bitbucket
;; DONE pr - list pr's for this project
;; diff-pr - show the diff between merging branches
