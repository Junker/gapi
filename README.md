# GAPI

Google API client for Common Lisp.

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

## Installation

This system can be installed from [UltraLisp](https://ultralisp.org/) like this:

```common-lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
(ql:quickload "gapi")
```

## Usage

```common-lisp
(defvar *service-file* "/myproject/config/google-service.json")

;; Translate
(defparameter *client*
  (gapi:make-client-with-service-account
   *service-file* :scopes '("https://www.googleapis.com/auth/cloud-translation")))
(defparameter *project-id* (gapi:client-project-id *client*))
(gapi:auth *client*)
(gapi:request *client* (format nil
                               "https://translate.googleapis.com/v3beta1/projects/~A:detectLanguage"
                               *project-id*)
              :method :POST :payload '(:|content| "Hello"))
;; => (:|languages| ((:|confidence| 1 :|languageCode| "en")))


;; FCM
(defparameter *client*
  (gapi:make-client-with-service-account
   *service-file* :scopes '("https://www.googleapis.com/auth/firebase.messaging")))
(defparameter *project-id* (gapi:client-project-id *client*))
(gapi:auth *client*)
(defvar *message*
  (list :|token| *token*
        :|notification| (list :|title| "Message Title"
                              :|body| "Message body")))
(gapi:request *client* (format nil "https://fcm.googleapis.com/v1/projects/~A/messages:send"
                               *project-id*)
              :method :POST :data (list :|message| *message*))

;;etc
```

## Documentation

- [Google APIs Explorer](https://developers.google.com/apis-explorer)
