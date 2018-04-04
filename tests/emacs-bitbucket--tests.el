(require 'emacs-bitbucket)

;; about flet deprecation and the substitutes cl-flet and cl-letf
;; http://nullprogram.com/blog/2017/10/27/
;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html

(ert-deftest bitbucket-actions--test--failed-to-find-repo ()
  "bitbucket-actions should return nil in a directory without version control"
  (cl-letf (((symbol-function 'moritz/get-user-and-repo-slug)
             (lambda () "Failed to retrieve repository data")))
    (should (stringp (bitbucket-actions)))))

(ert-deftest bitbucket-actions--test--succeded-finding-repo ()
  "bitbucket-actions should return nil in a directory without version control"
  (let ((test-buffer "ert-tests-repository-buffer"))
    (cl-letf (((symbol-function 'moritz/get-user-and-repo-slug)
               (lambda () "Failed to retrieve repository data"))
              ((symbol-function 'moritz/get-repository)
               (lambda (data callback) (get-buffer-create test-buffer))))
      (should (bufferp (bitbucket-actions))))
    (kill-buffer test-buffer)))
