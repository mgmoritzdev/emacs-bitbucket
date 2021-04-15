(require 'magit)

(defun get-git-root ()
  "Get the current file git root directory"
  (interactive)
  (magit-toplevel))

(defun is-root-git-directory (directory)
  (if (member ".git" (directory-files directory)) t nil))

(defun get-location ()
  "Try to find the best path to run a path dependent function"
  (let ((file (buffer-file-name))
        (buffer (buffer-name)))
    (cond ((file-exists-p file) (expand-file-name file))
          (buffer (expand-file-name buffer)))))

(defun dig-for-dotgit (file)
  (let ((value 0)
        (directory (file-name-directory file))
        (git-root nil))
    (while (and (< value 9) (not git-root) (not (string= directory "/")))
      (if (is-root-git-directory directory)
          (progn
            (setq git-root directory))
        (progn (setq value (1+ value))
               (setq directory (parent-directory directory)))))
    (message git-root)))

(defun parent-directory (directory)
  (let ((directory (substring directory 0 (- (length directory) 1))))
    (file-name-directory directory)))

(defun get-files-in-dir (dir)
  (let ((default-directory dir))
    (let ((files (directory-files default-directory))
          (names-and-files ()))
      files)))

(provide 'emacs-bitbucket--vc)

;; (parent-directory "/home/moritz/workspace/projects/portal.webapi/Portal.DomainService/Services/ExamService.cs")
;; (is-root-git-directory "/home/moritz/workspace/projects/portal.webapi/Portal.WebApi")
;; (is-root-git-directory "/home/moritz/.emacs.d")
;; (parent-directory "/home/moritz/workspace/projects/portal.webapi/Portal.WebApi")
;; (get-files-in-dir "~/.emacs.d")
;; (buffer-name)
