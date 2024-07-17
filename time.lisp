(in-package #:gapi)

;; source: https://github.com/ruricolist/serapeum/blob/master/time.lisp

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0)
  "The Unix epoch as a universal time.")

(defun universal-to-unix (time)
  "Convert a universal time to a Unix time."
  (- time +unix-epoch+))

(defun get-unix-time ()
  "The current time as a count of seconds from the Unix epoch."
  (universal-to-unix (get-universal-time)))
