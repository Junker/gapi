(defsystem "gapi"
  :version "0.1.0"
  :author "Dmitrii Kosenkov"
  :license "MIT"
  :depends-on ("dexador"
               "alexandria"
               "jonathan"
               "cl-base64"
               "jose"
               "ironclad"
               "asn1"
               "pem")
  :description "Google Cloud API Client"
  :homepage "https://github.com/Junker/gapi"
  :source-control (:git "https://github.com/Junker/gapi.git")
  :components ((:file "package")
               (:file "time")
               (:file "gapi")))
