(require 'emacs-bitbucket--utils)

(defun moritz/select-branches-and-run-action (args)
  (let* ((data (car args))
         (callback (car (cdr args)))
         (branches-helm-source
          `((name . "Select a branch: ")
            (candidates . ,(mapcar '(lambda (element)
                                      `(,(cdr (assoc 'name element)) . ,element))
                                   (cdr (assoc 'values data))))
            (action . (lambda (candidate)
                        (if (functionp callback)
                            (funcall callback candidate)
                          candidate))))))
    (helm :sources '(branches-helm-source))))


(defun moritz/run-branches-action (branch)
  (message (format "Branch %s target hash: %s"
                   (cdr (assoc 'name branch))
                   (cdr (assoc 'hash (assoc 'target branch))))))

(provide 'emacs-bitbucket--branches)
