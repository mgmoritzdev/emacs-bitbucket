(require 'oauth2)

(defun moritz/get-repository (user callback)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 (format endpoint user))
     callback)))

(defun moritz/get-repository (user callback)
  "List bitbucket user's repositories"
  (let ((url-request-method "GET")
        (endpoint "repositories/%s"))
    (oauth2-url-retrieve
     moritz/bitbucket--token
     (concat moritz/bitbucket--v2 (format endpoint user))
     callback)))


(defun moritz/parse-repositories (result)
  (let ((request-data (moritz/parse-json))
        (repo-names '()))
    (mapcar (lambda (element)
              (push (format "%s" (cdr (assoc 'name element))) repo-names))
            (cdr (assoc 'values request-data)))
    repo-names))


(defun moritz/select-repository (result)
  (let ((data (moritz/parse-json)))
    (let ((repositories-helm-source
           `((name . "Select the repository: ")
             (candidates . ,(mapcar '(lambda (element)
                                       (cdr (assoc 'name element)))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (moritz/do-something (moritz/get-repo-uuid (moritz/get-repo-by-name data candidate))))))))
      (helm :sources '(repositories-helm-source)))))


;; parsing utils
(defun moritz/get-repo-uuid (repo)
  (cdr (assoc 'uuid repo)))

(defun moritz/do-something (uuid)
  (message uuid))

(defun moritz/get-repo-by-name (repos-vector name)
  (let (;; convert vector to list
        (repo-list (append (cdr (assoc 'values repos-vector)) nil))
        (value))
    (while (and repo-list (not value))
      (let ((item (car repo-list)))
        (if (string= name (cdr (assoc 'name item)))
            (setq value item))
        (setq repo-list (cdr repo-list))))
    value))

(defun moritz/parse-json ()
  (beginning-of-buffer)
  (search-forward "\n\n")
  (append '() (json-read)))

;; tests and examples
(moritz/list-pull-requests "ptmtech" "portaltm.server")

(moritz/get-repository "ptmtech" 'moritz/select-repository)

(moritz/get-repository "ptmtech" 'moritz/select-repository)

my-data-after-parsing-json
(insert (format "%s"  (assoc 'values my-data-after-parsing-json)))
(cdr (assoc 'name (elt (cdr (assoc 'values my-data-after-parsing-json)) 0)))

(cdr (assoc 'name (elt (cdr (assoc 'values my-data-after-parsing-json)) 0)))

(cdr (assoc 'name (elt (cdr (assoc 'values my-data-after-parsing-json)) 0)))

(assoc 'values my-data-after-parsing-json)

;; ideas
;; magit - add a button to bitbucket
;; pr - list pr's for this project
;; diff-pr - show the diff between merging branches
