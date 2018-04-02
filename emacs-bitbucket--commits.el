(require 'emacs-bitbucket--utils)

(defun moritz/select-commits-and-run-action (result &optional callback)
  (let ((data (moritz/parse-json)))
    (let ((commits-helm-source
           `((name . "Select a commit: ")
             (candidates . ,(mapcar '(lambda (element)
                                       `(,(car (split-string
                                                (cdr (assoc 'raw (assoc 'summary element)))
                                                "\\\n")) . ,element))
                                    (cdr (assoc 'values data))))
             (action . (lambda (candidate)
                         (funcall callback candidate))))))
      (helm :sources '(commits-helm-source)))))


(defun moritz/run-commits-action (commit)
  (message (cdr (assoc 'hash commit))))

(provide 'emacs-bitbucket--commits)
