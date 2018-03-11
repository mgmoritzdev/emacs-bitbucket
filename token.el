;; token related functions and variables

(require 'oauth2)

(defvar moritz/bitbucket--v1 "https://api.bitbucket.org/1.0/")
(defvar moritz/bitbucket--v2 "https://api.bitbucket.org/2.0/")
(defvar moritz/bitbucket--token
  (oauth2-auth-and-store "https://bitbucket.org/site/oauth2/authorize"
                         "https://bitbucket.org/site/oauth2/access_token"
                         nil
                         "S46SAPrK8gvfVRPuXy"
                         "wWpcJEy6QUfWp2yCHf88jWUmJBQATv9k"
                         "https://localhost:5000"))

(oauth2-refresh-access moritz/bitbucket--token)
