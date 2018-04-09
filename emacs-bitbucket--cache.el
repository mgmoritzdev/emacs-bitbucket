(defvar emacs-bitbucket--cache-directory ".emacs-bitbucket")

(defun moritz/save-repository-data (key value)
  (let* ((key (symbol-name key))
         (default-directory (concat (vc-root-dir) emacs-bitbucket--cache-directory))
         (cache-filename (expand-file-name key))
         (save-silently t)
         (data `((data . ,value)
                 (expire . ,(time-add (current-time) 3600)))))
    (with-current-buffer (find-file-noselect cache-filename)
      (erase-buffer)
      (insert (format "%S" data))
      (save-buffer)
      (kill-this-buffer))))

(defun moritz/get-cache-data (key)
  (let* ((key (symbol-name key))
         (default-directory (concat (vc-root-dir) emacs-bitbucket--cache-directory))
         (cache-filename (expand-file-name key)))
    (if (file-exists-p cache-filename)
        (with-current-buffer (find-file-noselect cache-filename)
          (let ((cache-content (read-from-string (buffer-string))))
            (kill-this-buffer)
            (car cache-content)))
      (error "Cache file could not be found"))))

(defun moritz/get-cache-value (key)
  (cdr (assoc 'data (moritz/get-cache-data key))))

(defun moritz/get-cache-expire-date (key)
  (cdr (assoc 'expire (moritz/get-cache-data key))))

(defun moritz/has-valid-cache (key)
  (let* ((default-directory (concat (vc-root-dir) emacs-bitbucket--cache-directory))
         (cache-filename (expand-file-name (symbol-name key))))
    (if (file-exists-p cache-filename)
        (time-less-p (current-time) (moritz/get-cache-expire-date key))
      nil)))



(provide 'emacs-bitbucket--cache)