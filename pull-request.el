;; to be replaced by (require 'token)
(if (not (boundp 'moritz/bitbucket--v1))
    (load-file (expand-file-name "token.el")))

(defun moritz/list-pull-requests (user repo)
  "List bitbucket pullrequests for user and repo"
  (let ((url-request-method "GET"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 "repositories/" user "/" repo "/pullrequests")
     'moritz/select-pr)))

;; /repositories/{username}/{repo_slug}/pullrequests/{pull_request_id}/approve
(defun moritz/approve-pull-request (user repo pull-request-id)
  "Approve the selected pull request"
  (let ((request-method "POST")
        (request-extra-headers (moritz/content-type-header "application/json")))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2
             "repositories/" user
             "/" repo
             "/pullrequests/" pull-request-id
             "/approve")
     'moritz/pull-request-approval-result
     nil
     request-method
     nil
     request-extra-headers)))

(defun moritz/content-type-header (content-type)
  "Return 'Content-Type: type' header."
  `(("Content-Type" . ,(format "%s" content-type))))

(moritz/content-type-header "application/json")

(defun parse-pull-requests-response (result)
  (let ((request-data (moritz/parse-json))
        (pr-titles '()))
    (mapcar (lambda (element)
              (push (format "%s" (cdr (assoc 'title element))) pr-titles))
            (cdr (assoc 'values request-data)))))

(defun moritz/pull-request-approval-result (result)
  (let ((data (moritz/parse-json)))
    (message (format "%s" data))))

(defun moritz/select-pr (result)
  (let ((data (moritz/parse-json)))
    (let ((pr-helm-source
           `((name . "Select a pull-request: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'title element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/do-something (moritz/get-repo-uuid (moritz/get-repo-by-name data candidate))))))))
      (helm :sources '(pr-helm-source)))))

(defun moritz/get-pr-by-name (pr-vector name)
  (let (;; convert vector to list
        (pr-list (append (cdr (assoc 'values pr-vector)) nil))
        (value))
    (while (and pr-list (not value))
      (let ((item (car pr-list)))
        (if (string= name (cdr (assoc 'name item)))
            (setq value item))
        (setq pr-list (cdr pr-list))))
    value))

;; tests and examples
(moritz/list-pull-requests "mgmdevptm" "testrepo")
(moritz/list-pull-requests "mmoritz" ".emacs.d")
(moritz/approve-pull-request "mgmdevptm" "testrepo" "7")

;; data structure
;; list-prs
;; https://api.bitbucket.org/2.0/repositories/<user>/<repo>/pullrequests
;; (insert (format "%s" moritz/tmp-pr-var))

;; [(
;;   (description . )
;;   (links (decline (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/decline)) (commits (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/commits)) (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167)) (comments (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/comments)) (merge (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/merge)) (html (href . https://bitbucket.org/ptmtech/frontend2/pull-requests/167)) (activity (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/activity)) (diff (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/diff)) (approve (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/approve)) (statuses (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/pullrequests/167/statuses)))
;;   (title . WEB-726 #time 5h #comment Fix admin fontes multiple search by nomeClirea)
;;   (close_source_branch . t)
;;   (merge_commit)
;;   (destination (commit (hash . 11adb135a350) (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2/commit/11adb135a350)))) (repository (links (self (href . https://api.bitbucket.org/2.0/repositories/ptmtech/frontend2)) (html (href . https://bitbucket.org/ptmtech/frontend2)) (avatar (href . https://bitbucket.org/ptmtech/frontend2/avatar/32/))) (type . repository) (name . frontend2) (full_name . ptmtech/frontend2) (uuid . {9a1a1aa2-67af-4f1e-9e27-f30b258bebee})) (branch (name . development)))
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
