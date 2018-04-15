(require 'helm)

(defun moritz/select-branches-and-run-action (args &optional prompt)
  "Select a branch in branches and optionally run a callback. The args must contain
a list with a branches object in the first element and a callback in the second. The
optional parameter prompt overrides de default text."
  (let* ((data (car args))
         (callback (car (cdr args)))
         (prompt (if (stringp prompt)
                     prompt
                   "Select a branch: "))
         (branches-helm-source
          `((name . ,prompt)
            (candidates . ,(mapcar (lambda (element)
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
