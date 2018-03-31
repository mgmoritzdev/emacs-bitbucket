(require 'oauth2)

;; to be replaced by (require 'token)
(if (not (boundp 'moritz/bitbucket--v1))
    (load-file (expand-file-name "token.el")))

(if (not (boundp 'moritz/bitbucket--v1))
    (load-file (expand-file-name "main.el")))

(defun moritz/list-pullrequests (user repo callback &optional cbargs)
  "List bitbucket pullrequests for user and repo"
  (let ((url-request-method "GET"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 "repositories/" user "/" repo "/pullrequests")
     callback
     cbargs)))

(defun moritz/send-post (url data headers callback)
  "Approve the selected pull request"
  (let ((request-method "POST")
        (request-extra-headers headers)
        (request-data data))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     url
     callback
     nil
     request-method
     request-data
     request-extra-headers)))

(defun moritz/send-delete (url callback)
  "Approve the selected pull request"
  (let ((request-method "DELETE"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     url
     callback
     nil
     request-method
     nil)))

(defun moritz/content-type-header (content-type)
  "Return 'Content-Type: type' header."
  `("Content-Type" . ,(format "%s" content-type)))

(moritz/content-type-header "application/json")

(defun parse-pullrequests-response (result)
  (let ((request-data (moritz/parse-json))
        (pr-titles '()))
    (mapcar (lambda (element)
              (push (format "%s" (cdr (assoc 'title element))) pr-titles))
            (cdr (assoc 'values request-data)))))

(defun moritz/select-pullrequest (result)
  (let ((data (moritz/parse-json)))
    (let ((pr-helm-source
           `((name . "Select a pull request: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'title element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/get-pullrequest-by-name data candidate))))))
      (helm :sources '(pr-helm-source)))))

(defun moritz/select-pullrequest-and-run-action (result &optional callback)
  (let ((data (moritz/parse-json)))
    (let ((pr-helm-source
           `((name . "Select a pull request: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'title element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (funcall callback (moritz/get-pullrequest-by-name data candidate)))))))
      (helm :sources '(pr-helm-source)))))

(defun moritz/get-pullrequest-by-name (pr-vector title)
  (let (;; convert vector to list
        (pr-list (append (cdr (assoc 'values pr-vector)) nil))
        (value))
    (while (and pr-list (not value))
      (let ((item (car pr-list)))
        (if (string= title (cdr (assoc 'title item)))
            (setq value item))
        (setq pr-list (cdr pr-list))))
    value))

(defun moritz/request-status (success-callback failure-callback)
  (let ((status-code-family (* 100 (/ (string-to-number (moritz/http-status-code)) 100))))
    (cond ((= status-code-family 200) (funcall success-callback))
          ((= status-code-family 400) (funcall failure-callback))
          ((= status-code-family 500) (funcall failure-callback)))))

(defun moritz/message-approve-result (result)
  (moritz/request-status
   '(lambda () (message "Pull request approved!"))
   '(lambda () (let ((data (moritz/parse-json)))
            (message (cdr (assoc 'message (assoc 'error data))))))))

(defun moritz/message-unapprove-result (result)
  (moritz/request-status
   '(lambda () (message "Pull request UNapproved!"))
   '(lambda () (message "Failed to unapprove pull request"))))

(defun moritz/get-pullrequest-link (link-name pullrequest)
  (cdr (assoc 'href (assoc (intern link-name) (assoc 'links pullrequest)))))

(defun moritz/pullrequest-approve (args)
  (let ((pullrequest (car args)))
    (moritz/send-post
     (moritz/get-pullrequest-link "approve" pullrequest)
     nil
     nil
     'moritz/message-approve-result)))

(defun moritz/pullrequest-unapprove (args)
  (let ((pullrequest (car args)))
    (moritz/send-delete
     (moritz/get-pullrequest-link  "approve" pullrequest)
     'moritz/message-unapprove-result)))

(defun moritz/post-example-with-json (pullrequest)
  (moritz/send-post
   (moritz/get-pullrequest-link "approve" pullrequest)
   (json-encode '(("key1" . "value1")))
   `(,(moritz/content-type-header "application/json"))
   'moritz/message-approve-result))

(defun moritz/run-pullrequest-action (pullrequest)
  (moritz/helm-run-assoc-function
   '(("unapprove" . moritz/pullrequest-unapprove)
     ("approve" . moritz/pullrequest-approve)
     ;; ("decline" . moritz/pullrequest-decline)
     ;; ("commits" . moritz/pullrequest-commits)
     ;; ("self" . moritz/pullrequest-self)
     ;; ("comments" . moritz/pullrequest-comments)
     ;; ("merge" . moritz/pullrequest-merge)
     ;; ("html" . moritz/pullrequest-html)
     ;; ("activity" . moritz/pullrequest-activity)
     ;; ("diff" . moritz/pullrequest-diff)
     ;; ("statuses" . moritz/pullrequest-statuses)
     )
   `(,pullrequest)))

(defun moritz/get-pullrequest-action (pullrequest)
  (moritz/helm-select-and-run
   '("decline"
     "commits"
     "self"
     "comments"
     "merge"
     "html"
     "activity"
     "diff"
     "approve"
     "statuses")
   'moritz/get-pullrequest-link
   `(,pullrequest)))

;; (moritz/get-pullrequest-action tmp-pullrequest)

(defun moritz/helm-select-and-run (action-list callback &optional cbargs)
  (let ((pullrequest-actions-helm-source
         `((name . "Select an action: ")
           (candidates . action-list)
           (action . (lambda (candidate)
                       (apply callback candidate cbargs))))))
    (helm :sources '(pullrequest-actions-helm-source))))

(defun moritz/helm-run-assoc-function (action-list &optional cbargs)
  (let ((pullrequest-actions-helm-source
         `((name . "Select an action: ")
           (candidates . action-list)
           (action . (lambda (candidate)
                       (funcall candidate cbargs))))))
    (helm :sources '(pullrequest-actions-helm-source))))

(defun pull-request-actions ()
  "Apply some action in the current repository pull requests.
The actions can be one of the following:
  * approve
  * unapprove
  * decline (not implemented)
  * commits (not implemented)
  * self (not implemented)
  * comments (not implemented)
  * merge (not implemented)
  * html (not implemented)
  * activity (not implemented)
  * diff (not implemented)
  * statuses (not implemented)
"
  (interactive)
  (let  ((repo-data (get-user-and-repo-slug)))
    (moritz/list-pullrequests (cdr (assoc 'user repo-data))
                              (cdr (assoc 'repo-slug repo-data))
                              'moritz/select-pullrequest-and-run-action
                              '(moritz/run-pullrequest-action))))

(defun get-user-and-repo-slug ()
  (let ((default-directory (vc-root-dir))
        (git-ssh-regexp "^git\@")
        (git-https-regexp "^https"))
    (let ((remotes (split-string (shell-command-to-string "git remote -v")))
          (remote nil))
      (while remotes
        (let ((test-remote (car remotes)))
          (setq remotes (cdr remotes))
          (cond ((string= test-remote "origin")
                 (setq remote (car remotes))))))
      (cond ((string-match-p git-ssh-regexp remote)
             (save-match-data
               (and (string-match "\\`.+:\\([^:]+\\)\/\\([^\/]+\\)\.git\\'" remote)
                    (let ((user (match-string 1 remote))
                          (repo-slug (match-string 2 remote)))
                      `((user . ,user)
                        (repo-slug . ,repo-slug))
                      ))))
            ((string-match-p git-https-regexp remote)
             (save-match-data
               (and (string-match "\\`.+\/\\([^\/]+\\)\/\\([^\/]+\\)\\'" remote)
                    (let ((user (match-string 1 remote))
                          (repo-slug (replace-regexp-in-string "\.git" "" (match-string 2 remote))))
                      `((user . ,user)
                        (repo-slug . ,repo-slug))))))))))
