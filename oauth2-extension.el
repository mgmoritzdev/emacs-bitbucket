(require 'oauth2)

(defgroup oauth2-extension nil
  "An extension to oauth2 to allow the use of asymmetric cryptography"
  :prefix "oauth2-extension-")

(defcustom oauth2-extension--base-dir "~/.emacs.d"
  "The directory where the encrypted oauth2 key will be saved"
  :type '(string)
  :group 'oauth-extension)

(defvar oauth2-extension--asymmetric-encripted-file
  (let ((default-directory oauth2-extension--base-dir))
    (expand-file-name "oauth2-token.el.gpg" default-directory)))

(defun oauth2-extension-auth-and-store (auth-url token-url resource-url client-id client-secret
                                                 asymmetric-encryption-recipient
                                                 &optional redirect-uri)
  "Request access to a resource and store it in a encrypted file'."
  (let ((default-directory oauth2-extension--base-dir)
        (save-silently t))
    (cond ((file-exists-p oauth2-extension--asymmetric-encripted-file)
           (oauth2-extension-refresh-access (oauth2-extension--get-token)))
          (t
           (let ((token (oauth2-auth auth-url
                                     token-url
                                     client-id
                                     client-secret
                                     resource-url
                                     nil
                                     redirect-uri)))
             (with-current-buffer (get-buffer-create oauth2-extension--asymmetric-encripted-file)
               (if (stringp asymmetric-encryption-recipient)
                   (insert (format ";; -*- epa-file-encrypt-to: (\"%s\") -*-\n"
                                   asymmetric-encryption-recipient)))
               (insert (format "%S" token))
               (save-buffer)
               (kill-this-buffer)))
           token))))

(defun oauth2-extension-refresh-token (&optional callback cbargs)
  "Refresh OAuth access TOKEN"
  (oauth2-extension-refresh-access (oauth2-extension--get-token)
                                   callback
                                   cbargs))

(defun oauth2-extension-refresh-access (token &optional callback cbargs)
  "Refresh OAuth access TOKEN.
TOKEN should be obtained with `oauth2-request-access'."
  (let ((previous-access-token (oauth2-token-access-token token)))
    (oauth2-extension--make-access-request
     (oauth2-token-token-url token)
     (concat "client_id=" (oauth2-token-client-id token)
             "&client_secret=" (oauth2-token-client-secret token)
             "&refresh_token=" (oauth2-token-refresh-token token)
             "&grant_type=refresh_token")
     callback
     cbargs)))

(defun oauth2-extension--make-access-request (url data &optional callback cbargs)
  "Make an access request to URL using DATA in POST."
  (lexical-let* ((callback callback)
                 (cbargs cbargs)
                 (data data)
                 (success-callback (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (oauth2-extension--update-token data)
                                      (if callback
                                          (apply callback cbargs)))))
                 (error-callback (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                (message (format "Failed to refresh token. %s" error-thrown))))))
    (request url
             :type "POST"
             :data data
             :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
             :parser 'json-read
             :success success-callback
             :error error-callback)))

(defun oauth2-extension--update-token (new-token)
  (let* ((token (oauth2-extension--get-token))
         (previous-access-token (oauth2-token-access-token token)))
    (setf (oauth2-token-access-token token)
          (cdr (assoc 'access_token new-token)))
    (oauth2-extension--save-access-token token previous-access-token)))

(defun oauth2-extension--save-access-token (token previous-access-token)
  (let ((default-directory oauth2-extension--base-dir)
        (save-silently t))
    (with-current-buffer (find-file-noselect
                          (expand-file-name
                           oauth2-extension--asymmetric-encripted-file))
      (while (search-forward previous-access-token nil t)
        (replace-match (oauth2-token-access-token token)))
      (save-buffer)
      (kill-this-buffer))))

(defun oauth2-extension--get-token ()
  (let* ((default-directory oauth2-extension--base-dir)
         (filename oauth2-extension--asymmetric-encripted-file))
    (with-temp-buffer
      (insert-file-contents filename)
      (car (read-from-string (buffer-string))))))

(provide 'oauth2-extension)
