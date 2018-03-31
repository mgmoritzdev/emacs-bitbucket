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
  ;; (let ((request-method "POST")
  ;;       (request-extra-headers (moritz/content-type-header "application/json"))
  ;;       (request-data `(("key1" . "value1") ("key2" "value2")))
  ;;       (do-something)))
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

;; delete-me
(defun test-function (msg)
  (message msg))

(let ((action-pair '(("approve" . test-function))))
  (funcall (cdr (car action-pair)) (car (car action-pair))))

(funcall (quote test-function) "test")
;; end of delete-me


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

;; tests and examples
;; (moritz/list-pullrequests "mgmdevptm" "testrepo"
;;                           'moritz/select-pullrequest-and-run-action
;;                           '(moritz/post-example-with-json))

;; (moritz/list-pullrequests "mmoritz" ".emacs.d")
;; (moritz/approve-pullrequest "mgmdevptm" "testrepo" "7")


;; data structure
;; list-prs
;; https://api.bitbucket.org/2.0/repositories/<user>/<repo>/pullrequests
;; (insert (format "%s" moritz/tmp-pr-var))

;; [(
;;   (description . )
;;   (links
;;    (decline (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/decline))
;;    (commits (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/commits))
;;    (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167))
;;    (comments (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/comments))
;;    (merge (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/merge))
;;    (html (href . https://bitbucket.org/ptmtech/frontend2/pull-requests/167))
;;    (activity (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/activity))
;;    (diff (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/diff))
;;    (approve (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/approve))
;;    (statuses (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/statuses)))
;;   (title . WEB-726 #time 5h #comment Fix admin fontes multiple search by nomeClirea)
;;   (close_source_branch . t)
;;   (merge_commit)
;;   (destination
;;    (commit (hash . 11adb135a350) (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/commit/11adb135a350))))
;;    (repository
;;     (links
;;      (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2))
;;      (html (href . https://bitbucket.org/ptmtech/frontend2))
;;      (avatar (href . https://bitbucket.org/ptmtech/frontend2/avatar/32/)))
;;     (type . repository)
;;     (name . frontend2)
;;     (full_name . ptmtech/frontend2)
;;     (uuid . {9a1a1aa2-67af-4f1e-9e27-f30b258bebee}))
;;    (branch (name . development)))
;;   (state . OPEN)
;;   (closed_by)
;;   (summary (raw . "") (markup . markdown) (html . ) (type . rendered))
;;   (source (commit (hash . 319ba1e39647) (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/commit/319ba1e39647)))) (repository (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2)) (html (href . https://bitbucket.org/ptmtech/frontend2)) (avatar (href . https://bitbucket.org/ptmtech/frontend2/avatar/32/))) (type . repository) (name . frontend2) (full_name . ptmtech/frontend2) (uuid . {9a1a1aa2-67af-4f1e-9e27-f30b258bebee})) (branch (name . fix-admin-fontes-multiple-search)))
;;   (comment_count . 0)
;;   (author (username . dhiegohenrique) (display_name . Dhiego Henrique) (type . user) (uuid . {ef6b5cd7-7dfc-498d-a596-3316a179c36a}) (links (self (href . https://api.bitbucket.org/2.0/users/dhiegohenrique)) (html (href . https://bitbucket.org/dhiegohenrique/)) (avatar (href . https://bitbucket.org/account/dhiegohenrique/avatar/32/))))
;;   (created_on . 2018-03-08T19:03:37.688441+00:00)
;;   (reason . "")
;;   (updated_on . 2018-03-09T12:25:19.381327+00:00)
;;   (type . pullrequest)
;;   (id . 167) (task_count . 0))


;;  ((description . * WEB-725 #comment add request time condition to update data grid. #time 3h * WEB-725 #comment add header time when exist a token. #time 20m * WEB-725 #comment fix white space.)
;;   (links (decline (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/decline)) (commits (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/commits)) (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166)) (comments (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/comments)) (merge (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/merge)) (html (href . https://bitbucket.org/ptmtech/frontend2/pull-requests/166)) (activity (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/activity)) (diff (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/diff)) (approve (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/approve)) (statuses (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/166/statuses)))
;;   (title . Add time update data grid)
;;   (close_source_branch . t)
;;   (merge_commit)
;;   (destination (commit (hash . 11adb135a350) (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/commit/11adb135a350)))) (repository (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2)) (html (href . https://bitbucket.org/ptmtech/frontend2)) (avatar (href . https://bitbucket.org/ptmtech/frontend2/avatar/32/))) (type . repository) (name . frontend2) (full_name . ptmtech/frontend2) (uuid . {9a1a1aa2-67af-4f1e-9e27-f30b258bebee})) (branch (name . development)))
;;   (state . OPEN)
;;   (closed_by)
;;   (summary (raw . * WEB-725 #comment add request time condition to update data grid. #time 3h* WEB-725 #comment add header time when exist a token. #time 20m * WEB-725 #comment fix white space.) (markup . markdown) (html . <ul></ul>) (type . rendered))
;;   (source (commit (hash . da76a4a27e6d) (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/commit/da76a4a27e6d)))) (repository (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2)) (html (href . https://bitbucket.org/ptmtech/frontend2)) (avatar (href . https://bitbucket.org/ptmtech/frontend2/avatar/32/))) (type . repository) (name . frontend2) (full_name . ptmtech/frontend2) (uuid . {9a1a1aa2-67af-4f1e-9e27-f30b258bebee})) (branch (name . add-time-update-data-grid)))
;;   (comment_count . 0)
;;   (author (username . renanptm) (display_name . Renan Bet Rodrigues) (type . user) (uuid . {d88c1c7c-0cb0-497e-897c-2188f3984646}) (links (self (href . https://api.bitbucket.org/2.0/users/renanptm)) (html (href . https://bitbucket.org/renanptm/)) (avatar (href . https://bitbucket.org/account/renanptm/avatar/32/))))
;;   (created_on . 2018-03-08T13:21:21.603045+00:00)
;;   (reason . "") (updated_on . 2018-03-08T13:36:53.050759+00:00)
;;   (type . pullrequest)
;;   (id . 166)
;;   (task_count . 0))
;;  ]


;; (insert (format "%s" tmp-pullrequest))
