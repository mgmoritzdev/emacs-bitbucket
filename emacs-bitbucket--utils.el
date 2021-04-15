(require 'json)
(eval-when-compile (require 'cl))
(require 'emacs-bitbucket--cache)
(require 'oauth2-extension)


(defun emacs-bitbucket--retrieve (endpoint endpoint-params callback parser &optional cbargs method extra-headers)
  (if (and emacs-bitbucket--use-cache (moritz/has-valid-cache endpoint))
      (funcall callback (append `(,(moritz/get-cache-value endpoint)) cbargs))
    (emacs-bitbucket--request endpoint
                              endpoint-params
                              callback
                              parser
                              cbargs
                              method
                              extra-headers)))

(defun emacs-bitbucket--request (endpoint endpoint-params callback parser &optional cbargs method extra-headers)
  (lexical-let* ((url (moritz/get-url endpoint endpoint-params))
                 (endpoint endpoint)
                 (endpoint-params endpoint-params)
                 (callback callback)
                 (parser parser)
                 (cbargs cbargs)
                 (method method)
                 (extra-headers extra-headers)
                 (headers (append extra-headers `(,(moritz/get-authorization-header))))
                 (success-callback (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (moritz/save-repository-data endpoint (append '() data))
                                      (funcall callback (append `(,data) cbargs)))))
                 (error-callback (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                (if (string= (format "%s" error-thrown) "(error http 401)")
                                                    (oauth2-extension-refresh-token 'emacs-bitbucket--request
                                                                                    `(,endpoint
                                                                                      ,endpoint-params
                                                                                      ,callback
                                                                                      ,parser
                                                                                      ,cbargs
                                                                                      ,method
                                                                                      ,extra-headers)))))))
    (request
     url
     :type method
     :headers headers
     :parser parser
     :success success-callback
     :error error-callback)))

(defun moritz/parse-json ()
  (goto-char (point-min))
  (search-forward "\n\n")
  (json-read))

(defun moritz/parse-utf-8 ()
  (json-read-from-string
   (decode-coding-string (buffer-string) 'utf-8)))

(defun mortiz/parse-and-run-action (result callback)
  (let ((data (moritz/parse-json)))
    (funcall callback data)))

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

(defun moritz/get-token ()
  (oauth2-token-access-token (oauth2-extension--get-token)))

(defun moritz/get-authorization-header ()
  (moritz/authorization-header (moritz/get-token)))

(defun moritz/get-url (endpoint &optional args)
  (concat
   moritz/bitbucket--v2
   (apply 'format (append `(,(cdr (assoc endpoint moritz/endpoints))) args))))

;; (moritz/get-url 'repository '("mgmdevptm" "testrepo"))
;; (moritz/get-url 'repositories)
(defvar moritz/endpoints
  '((repositories . "repositories")
    (repository . "repositories/%s/%s")
    (branches . "repositories/%s/%s/refs/branches?pagelen=100")
    (team-members . "teams/%s/members")))

(defun moritz/get-user-and-repo-slug ()
  (condition-case nil
      (let ((default-directory (get-git-root))
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
                            (repo-slug . ,repo-slug)))))))))
    (error (message "Could not get repository associated with the current directory"))
    ))

(defun moritz/get-user-and-repo-slug-list ()
  (let ((data (moritz/get-user-and-repo-slug)))
    `(,(cdr (assoc 'user data))
      ,(cdr (assoc 'repo-slug data)))))

(defun moritz/get-user-list ()
  (let ((data (moritz/get-user-and-repo-slug)))
    `(,(cdr (assoc 'user data)))))

(provide 'emacs-bitbucket--utils)
