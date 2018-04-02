(require 'json)
(require 'cl)

(defun moritz/parse-json ()
  (goto-char (point-min))
  (search-forward "\n\n")
  (json-read))

(defun moritz/http-status-code ()
  (message (buffer-substring 10 13)))

(defun moritz/get-resource-link (link-name response-object)
  (cdr (assoc 'href (assoc (intern link-name) (assoc 'links response-object)))))

(defun moritz/anonymize-next ()
  (interactive)
  (search-forward-regexp "//^\".+\"//"))

(cl-defstruct emacs-bitbucket--test-data
  username
  repo-slug
  pull-request-id
  commit-hash
  destination-commit-hash)

;; setup for testing in restclient file bitbucket.rest
(defun emacs-bitbucket--get-test-data ()
  (make-emacs-bitbucket--test-data
   :username "YOUR-USER"
   :repo-slug "YOUR-REPOSITORY-SLUG"
   :pull-request-id "YOUR-REPOSITORY-ID (INTEGER)"
   :commit-hash "A-COMMIT-HASH"
   :destination-commit-hash "A-PULL-REQUEST-DESTINATION-COMMIT-HASH"))

(if (file-exists-p (expand-file-name ".secrets"))
    (load-file ".secrets"))

(provide 'emacs-bitbucket--utils)
