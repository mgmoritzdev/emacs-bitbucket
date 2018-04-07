(defun moritz/save-repository-data (repo)
  (let* ((default-directory (vc-root-dir))
         (cache-filename (expand-file-name
                          ".emacs-bitbucket--cache-repository"))
         (save-silently t)
         (data `((data (repository . ,repo))
                 (expire ,(time-add (current-time) 3600)))))
    (with-current-buffer (find-file-noselect cache-filename)
      (erase-buffer)
      (insert (format "%S" data))
      (save-buffer)
      (kill-this-buffer))))

(defun moritz/get-repository-cache-data ()
  (let ((default-directory (vc-root-dir))
        (cache-filename (expand-file-name
                         ".emacs-bitbucket--cache-repository"))
        (save-silently t))
    (with-current-buffer (find-file-noselect cache-filename)
      (let ((cache-content (read-from-string (buffer-string))))
        (kill-this-buffer)
        (cdr (car (cdr (car (car cache-content)))))))))

(defun moritz/get-repository-cache-expire-date ()
  (let ((default-directory (vc-root-dir))
        (cache-filename (expand-file-name
                         ".emacs-bitbucket--cache-repository"))
        (save-silently t))
    (with-current-buffer (find-file-noselect cache-filename)
      (let ((cache-content (read-from-string (buffer-string))))
        (kill-this-buffer)
        (car (cdr (car (cdr (car cache-content)))))))))


;; tmp
;; create tests with this:
(let ((default-directory "~/workspace/projects/testrepo"))
  (moritz/get-repository-cache-data))

(let ((default-directory "~/workspace/projects/testrepo"))
  (moritz/get-repository-cache-expire-date))

;; time functions
(current-time-string)

(insert (format "%S" (current-time)))
(insert (format "%S" (time-add (current-time) 3600)))

(time-add (current-time) 3600)

(format-time-string "%Y-%m-%dT%T")

(time-less-p '(23241 409 739736 389000) '(23241 4027 324184 988000))
(time-less-p '(23241 4027 324184 988000) '(23241 409 739736 389000))
