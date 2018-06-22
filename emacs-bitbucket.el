(load-file "oauth2-extension.el")
(load-file "emacs-bitbucket--tokens.el")
(load-file "emacs-bitbucket--pullrequests.el")
(load-file "emacs-bitbucket--branches.el")
(load-file "emacs-bitbucket--commits.el")
(load-file "emacs-bitbucket--cache.el")
(load-file "emacs-bitbucket--team-members.el")
(load-file "emacs-bitbucket--vc.el")

(require 'oauth2)
(require 'oauth2-extension)
(require 'json)
(require 'emacs-bitbucket--tokens)
(require 'emacs-bitbucket--pullrequests)
(require 'emacs-bitbucket--branches)
(require 'emacs-bitbucket--commits)
(require 'emacs-bitbucket--cache)
(require 'emacs-bitbucket--team-members)
(require 'emacs-bitbucket--vc)

(defun moritz/list-repository (user callback &optional cbargs)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s?pagelen=10&page=2"))
    (oauth2-url-retrieve
     (oauth2-extension--get-token)
     (concat moritz/bitbucket--v2 (format endpoint user))
     callback
     cbargs)))

(defun moritz/get-repository (callback &optional cbargs)
  "Get repository data"
  (let ((endpoint 'repository)
        (endpoint-params (moritz/get-user-and-repo-slug-list)))
    (emacs-bitbucket--retrieve endpoint
                               endpoint-params
                               callback
                               'json-read
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
             (candidates . ,(mapcar (lambda (element)
                                      (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/do-something (moritz/get-repo-uuid (moritz/get-repo-by-name data candidate))))))))
      (helm :sources '(repositories-helm-source)))))

(defun moritz/select-repo-and-run-action (result &optional callback)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar (lambda (element)
                                      (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (funcall callback (moritz/get-repo-by-name data candidate)))))))
      (helm :sources '(repositories-helm-source)))))

(defun moritz/select-repository-and-get-url (result)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar (lambda (element)
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
  (cdr (car (car (append (moritz/get-resource-link "clone" repo)
                         nil)))))

(defun moritz/get-repo-ssh-url (repo)
  (cdr (car (car (cdr (append (moritz/get-resource-link "clone" repo)
                              nil))))))

(defun moritz/repository-action-pullrequests (args)
  (let ((repo (car args)))
    (moritz/get-repository-resource
     (moritz/get-resource-link "pullrequests" repo)
     'moritz/select-pullrequest-and-run-action
     '(moritz/run-pullrequest-action))))

;; TODO replace with moritz/get-resource-link
(defun moritz/repository-action-commits (args)
  (let ((repo (car args)))
    (moritz/get-repository-resource
     (cdr (assoc 'href (assoc 'commits (assoc 'links repo))))
     'moritz/select-commits-and-run-action
     '(moritz/run-commits-action))))

;; TODO replace with moritz/get-resource-link
(defun moritz/repository-action-branches (args)
  (let* ((repo (car args))
         (url (moritz/get-resource-link "branches" repo)))
    (emacs-bitbucket--retrieve
     'branches
     (moritz/get-user-and-repo-slug-list)
     'moritz/select-branches-and-run-action
     'json-read
     '(moritz/run-branches-action))))

(defun moritz/repository-action-team-members (args)
  (let* ((repo (car args)))
    (emacs-bitbucket--retrieve
     'team-members
     (moritz/get-user-list)
     'moritz/select-team-members-and-run-action
     'moritz/parse-utf-8
     '(moritz/get-selected-team-members-uuid))))

(defun moritz/run-repository-action (args)
  (moritz/helm-run-assoc-function
   '(("List pull requests" . moritz/repository-action-pullrequests)
     ("Create pull request" . moritz/get-team-members-and-create-pull-request)
     ("Commits" . moritz/repository-action-commits)
     ("Branches" . moritz/repository-action-branches)
     ("Teams" . moritz/repository-action-team-members))
   args))

(defun moritz/parse-and-run-repository-action (result)
  ;; (setq tmp-result result)
  (condition-case nil
      (let ((repo (moritz/parse-json)))
        (moritz/run-repository-action repo))
    (error (message "%s" (assoc 'error result)))))

(defun moritz/get-repository-resource (action callback &optional cbargs)
  "Get repository resource"
  (let ((request-method "GET"))
    (oauth2-url-retrieve
     (oauth2-extension--get-token)
     action
     callback
     cbargs)))

(defun moritz/create-pullrequest-callback (result)
  (message (format "%s" result)))

(defun moritz/get-team-members-and-create-pull-request (args)
  (let ((repo (car args)))
    (emacs-bitbucket--retrieve
     'team-members
     (moritz/get-user-list)
     'moritz/get-branches-and-create-pull-request
     'moritz/parse-utf-8
     args)))

(defun moritz/get-branches-and-create-pull-request (args)
  (let ((team-members (car args))
        (repo (car (cdr args))))
    (emacs-bitbucket--retrieve
     'branches
     (moritz/get-user-and-repo-slug-list)
     'moritz/repository-action-create-pullrequest
     'json-read
     args)))

(defun moritz/repository-action-create-pullrequest (args)
  (let* ((branches (car args))
         (team-members (car (cdr args)))
         (repo (car (cdr (cdr args))))
         (source-branch (cdr (assoc 'name
                                    (moritz/select-branches-and-run-action `(,branches)
                                                                           "Select the source branch: "))))
         (destination-branch (cdr (assoc 'name
                                         (moritz/select-branches-and-run-action `(,branches)
                                                                                "Select the destination branch: "))))
         (reviewers (moritz/select-team-members-and-run-action `(,team-members)
                                                               "Select reviewers: "))
         (pullrequest-url (moritz/get-resource-link "pullrequests" repo))
         (pullrequest-title (read-string "Enter the pull request title: "))
         (callback 'moritz/diff-result)
         (request-method "POST")
         (request-data (json-encode `(("source" .
                                       (("branch" . (("name" . ,source-branch)))))
                                      ("destination" .
                                       (("branch" . (("name" . ,destination-branch)))))
                                      ("title" . ,pullrequest-title)
                                      ("reviewers" . (,reviewers))
                                      ("close_source_branch" . t))))
         (request-extra-headers `(,(moritz/content-type-header "application/json"))))
    (oauth2-url-retrieve
     (oauth2-extension--get-token)
     pullrequest-url
     callback
     nil
     request-method
     request-data
     request-extra-headers)))

(defun moritz/post-repository-resource (action data headers callback &optional cbargs)
  "Get repository resource"
  (let ((request-method "POST")
        (request-data data)
        (request-extra-headers headers))
    (oauth2-url-retrieve
     (oauth2-extension--get-token)
     action
     callback
     cbargs
     request-method
     request-data
     request-extra-headers)))

(defun emacs-bitbucket ()
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
  (condition-case nil
      (moritz/get-repository 'moritz/run-repository-action)
    (error (message "Failed to get repository"))))

;; tests and examples
;; call from a file in some bitbucket repository: M-x bitbucket-actions

;; ideas
;; magit - add a button to bitbucket
;; DONE pr - list pr's for this project
;; diff-pr - show the diff between merging branches

(provide 'emacs-bitbucket)
