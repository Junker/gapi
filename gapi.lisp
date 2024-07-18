(in-package #:gapi)

(defparameter *jwt-token-expiry-length* 3600)

(define-condition gapi-error (error)
  ((code :initarg :code
         :reader gapi-error-code
         :type integer)
   (message :initarg :message
            :reader gapi-error-message
            :type string)
   (status :initarg :status
           :reader gapi-error-status
           :type string)
   (details :initarg :details
            :reader gapi-error-details))
  (:report (lambda (condition stream)
             (with-slots (code message status details) condition
               (format stream "Google API request failed with code ~D and status ~S: ~A~% Details:~%~S"
                       code status message details)))))

(defclass client ()
  ((project-id :initarg :project-id :accessor client-project-id :type string)
   (private-key :initarg :private-key :accessor client-private-key :type ironclad:rsa-private-key)
   (client-email :initarg :client-email :accessor client-client-email :type string)
   (token-uri :initarg :token-uri :accessor client-token-uri :type string)
   (scopes :initarg :scopes :accessor client-scopes :type list)
   (access-token :initarg nil :accessor client-access-token :initform nil)
   (access-token-expires-at :initarg :access-token-expires-at :accessor client-access-token-expires-at :type integer))
  (:default-initargs
   :project-id (error "PROJECT-ID required.")
   :private-key (error "PRIVATE-KEY required.")
   :client-email (error "CLIENT-EMAIL required.")
   :access-token-expires-at 0))


;; PRIV
(defun parse-service-account-file (path)
  (jojo:parse (uiop:read-file-string path)))

(defun read-pkcs8-private-key (pem)
  (let* ((pkcs8-der (asn1:decode (base64:base64-string-to-usb8-array
                                  (cdar (pem:parse (make-string-input-stream pem))))))
         (pkcs1-der (asn1:decode (cdr (fourth (car pkcs8-der))))))
    (trivia:match pkcs1-der
      ((asn1:rsa-private-key :private-exponent d :modulus n)
       (ironclad:make-private-key :rsa :d d :n n)))))

(defun %generate-jwt (private-key client-email token-uri scopes expiry-length)
  (jose:encode :rs256 private-key `(("iss" . ,client-email)
                                    ("iat" . ,(get-unix-time))
                                    ("exp" . ,(+ (get-unix-time) expiry-length))
                                    ("scope" . ,(format nil "~{~A~^ ~}" scopes))
                                    ("aud" . ,token-uri))))

(defun %auth (token-url jwt)
  (jojo:parse (dex:post token-url
                        :content (format nil "grant_type=urn:ietf:params:oauth:grant-type:jwt-bearer&assertion=~A" jwt)
                        :headers '(("Content-Type" . "application/x-www-form-urlencoded")))))

;; PUBLIC

(defun make-client-with-service-account (path &key scopes)
  (let ((acc (parse-service-account-file path)))
    (make-instance 'client
                   :project-id (getf acc :|project_id|)
                   :private-key (read-pkcs8-private-key (getf acc :|private_key|))
                   :client-email (getf acc :|client_email|)
                   :token-uri (getf acc :|token_uri|)
                   :scopes scopes)))

(defmethod generate-jwt ((client client) &key (expiry-length *jwt-token-expiry-length*))
  (with-slots (private-key client-email token-uri scopes) client
    (%generate-jwt private-key client-email token-uri scopes expiry-length)))


(defmethod auth ((client client))
  (let ((response (%auth (client-token-uri client)
                         (generate-jwt client))))
    (setf (client-access-token-expires-at client) (+ (get-universal-time)
                                                     (getf response :|expires_in|))
          (client-access-token client) (getf response :|access_token|))))

(defmethod client-access-token-expired-p ((client client))
  (> (get-universal-time)
     (client-access-token-expires-at client)))

(defmethod client-authorized-p ((client client))
  (not (null (client-access-token client))))

(defmethod request ((client client) url &key (method :GET) payload)
  (assert (client-access-token client)
          nil "Client is not authorized, use (gapi:auth client)")
  (handler-case
      (jojo:parse
       (dex:request url
                    :method method
                    :headers `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format nil "Bearer ~A"
                                                           (client-access-token client))))
                    :content (etypecase payload
                               ((or string vector null) payload)
                               (list (jojo:to-json payload)))))
    (dex:http-request-failed (err)
      (if (not (equal (gethash "content-type" (dex:response-headers err))
                      "application/json; charset=UTF-8"))
          (error err)
          (let* ((data (jojo:parse (dex:response-body err)))
                 (error-data (getf data :|error|)))
            (if (not error-data)
                (error err)
                (error (make-condition 'gapi-error
                                       :code (getf error-data :|code|)
                                       :status (getf error-data :|status|)
                                       :message (getf error-data :|message|)
                                       :details (getf error-data :|details|)))))))))
