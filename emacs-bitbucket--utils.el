(require 'json)
(require 'cl)

(defun emacs-bitbucket--retrieve (endpoint endpoint-params callback parser &optional cbargs method extra-headers)
  (if (moritz/has-valid-cache endpoint)
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
                 (callback callback)
                 (cbargs cbargs)
                 (headers (append extra-headers `(,(moritz/get-authorization-header))))
                 (success-callback (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (moritz/save-repository-data endpoint (append '() data))
                                      (funcall callback (append `(,data) cbargs)))))
                 (error-callback (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                (if (string= (format "%s" error-thrown) "(error http 401)")
                                                    (progn
                                                      (oauth2-extension-refresh-token)
                                                      ;; 'emacs-bitbucket--request
                                                      ;; (cons url callback parser cbargs method extra-headers)
                                                      ))))))
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
    (repository . "repositories/%s/%s")))

(provide 'emacs-bitbucket--utils)
