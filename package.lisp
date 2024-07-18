(defpackage gapi
  (:use #:cl #:alexandria)
  (:import-from #:local-time
                #:timestamp-to-unix
                #:now)
  (:export #:client
           #:client-project-id
           #:client-client-email
           #:client-scopes
           #:client-token-uri
           #:client-access-token
           #:client-access-token-expires-at
           #:client-access-token-expired-p
           #:client-authorized-p
           #:auth
           #:generate-jwt
           #:request
           #:make-client-with-service-account
           #:gapi-error
           #:gapi-error-code
           #:gapi-error-message
           #:gapi-error-status
           #:gapi-error-details))
