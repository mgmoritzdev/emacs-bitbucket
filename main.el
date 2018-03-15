(require 'oauth2)

(defun moritz/get-repository (user callback &optional cbargs)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 (format endpoint user))
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

;; tests and examples
;; (moritz/list-pull-requests "ptmtech" "portaltm.server")

(moritz/get-repository "mmoritz" 'moritz/select-repository '("test1"))

;; get repo and call a method on it: get-repo-uuid
(moritz/get-repository "mmoritz"
                       'moritz/select-repo-and-run-action
                       '((lambda (repo) (message (moritz/get-repo-uuid repo)))))

;; get repo and call a method on it: get-repo-ssh-url
(moritz/get-repository "mmoritz"
                       'moritz/select-repo-and-run-action
                       '((lambda (repo) (message (moritz/get-repo-ssh-url repo)))))


(moritz/get-repository "ptmtech"
                       'moritz/select-repo-and-run-action
                       '((lambda (repo) (message (moritz/get-repo-ssh-url repo)))))

(moritz/get-repository "ptmtech" 'moritz/select-repository)

(moritz/get-remote "mmoritz" 'moritz/select-repository)
(moritz/get-repository "mmoritz" 'moritz/select-repository-and-get-url)


;; ideas
;; magit - add a button to bitbucket
;; pr - list pr's for this project
;; diff-pr - show the diff between merging branches


(insert (format "%s" teste-repo))

(get-repo-https-url teste-repo)
(get-repo-ssh-url teste-repo)

(insert (format "%s" (cdr (car (car (cdr (append (cdr (assoc 'clone
                                                             (assoc 'links teste-repo)))
                                                 nil)))))))

(href . https://mmoritz@bitbucket.org/mmoritz/flute-friend.git)

[((href . https://mmoritz@bitbucket.org/mmoritz/flute-friend.git) (name . https))
 ((href . git@bitbucket.org:mmoritz/flute-friend.git) (name . ssh))]

(clone . [((href . https://mmoritz@bitbucket.org/mmoritz/flute-friend.git) (name . https)) ((href . git@bitbucket.org:mmoritz/flute-friend.git) (name . ssh))])
