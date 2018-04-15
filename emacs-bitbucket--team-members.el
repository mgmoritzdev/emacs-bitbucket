(require 'helm)

(defun moritz/select-team-members-and-run-action (args &optional prompt)
  "Select a team-member in team-members and optionally run a callback. The args must contain
a list with a team-members object in the first element and a callback in the second. The
optional parameter prompt overrides de default text."
  (let* ((data (car args))
         (callback (car (cdr args)))
         (prompt (if (stringp prompt)
                     prompt
                   "Select a team-member: "))
         (team-members-helm-source
          `((name . ,prompt)
            (candidates . ,(mapcar (lambda (element)
                                     `(,(cdr (assoc 'display_name element)) . ,element))
                                   (cdr (assoc 'values data))))
            (action . (lambda (candidate)
                        (if (functionp callback)
                            (funcall callback candidate)
                          (moritz/get-selected-team-members-uuid candidate)))))))
    (helm :sources '(team-members-helm-source))))

(defun moritz/get-selected-team-members-uuid (team-member)
  (mapcar 'moritz/get-team-member-uuid
          (helm-marked-candidates)))

(defun moritz/get-team-member-uuid (team-member)
  (assoc 'uuid team-member))

(provide 'emacs-bitbucket--team-members)
