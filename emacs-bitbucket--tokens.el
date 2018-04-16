;; token related functions and variables

(require 'oauth2)

(defvar moritz/bitbucket--v1 "https://api.bitbucket.org/1.0/")
(defvar moritz/bitbucket--v2 "https://api.bitbucket.org/2.0/")

;; Get a OAuth2 token and save it with asymmetric key pointing with your e-mail public key
;; (oauth2-extension-auth-and-store "https://bitbucket.org/site/oauth2/authorize"
;;                                  "https://bitbucket.org/site/oauth2/access_token"
;;                                  nil
;;                                  "YOUR-CLIENT-ID"
;;                                  "YOUR-CLIENT-SECRET"
;;                                  "YOUR-EMAIL-ADDRESS"
;;                                  "REDIRECT-URL")

(defun moritz/refresh-token (&optional callback cbargs)
  "Refresh bitbucket token"
  (interactive)
  (oauth2-extension-refresh-token callback cbargs))

(provide 'emacs-bitbucket--tokens)
