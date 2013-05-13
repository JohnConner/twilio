(in-package :twilio)

(declaim #.*compile-decl*)

(define-condition twilio-error (error)
  ()
  (:documentation "Errors raised when using the twilio API"))

(define-condition no-account-sid (twilio-error)
  ()
  (:report "No account SID or auth code supplied")
  (:documentation "Error that is raised if no account SID or auth code was given"))

(define-condition send-error (twilio-error)
  ()
  (:documentation "Error that is raised when a message could not be sent"))

(defun read-new-account-sid-and-token ()
  (format t "Enter new account SID: ")
  (finish-output)
  (let ((account (read-line)))
    (format t "Enter new auth token: ")
    (finish-output)
    (list account (read-line))))

(defun check-account-sid (account token)
  (restart-case
      (if (or (null account) (null token))
          (error 'no-account-sid)
          (values account token))
    (specify-account (new-account-sid new-token)
      :report "Restart with different credentials"
      :interactive read-new-account-sid-and-token
      (check-account-sid new-account-sid new-token))
    (specify-account-and-update (new-account-sid new-token)
      :report "Restart with different credentials and update the default settings"
      :interactive read-new-account-sid-and-token
      (progn
        (setq *twilio-account-sid* new-account-sid)
        (setq *twilio-auth-token* new-token)
        (check-account-sid new-account-sid new-token)))))
