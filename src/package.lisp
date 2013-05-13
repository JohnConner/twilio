(defpackage :twilio
  (:use :cl)
  (:documentation "Common Lisp interface to the twilio SMS service")
  (:export
   #:*twilio-account-sid*
   #:send-sms))

(in-package :twilio)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
