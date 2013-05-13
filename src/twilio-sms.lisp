(in-package :twilio)

(declaim #.*compile-decl*)

(defun authenticated-request (url account token &rest args)
  (multiple-value-bind (new-account new-token)
      (check-account-sid account token)
    (apply #'drakma:http-request (replace-all url "#ACC" account)
           :basic-authorization (list new-account new-token)
           args)))

(defun send-sms (from destination text &key
                                         (account *twilio-account-sid*)
                                         (auth-token *twilio-auth-token*))
  (authenticated-request "https://api.twilio.com/2010-04-01/Accounts/#ACC/SMS/Messages"
                         account auth-token
                         :method :post
                         :parameters `(("From" . ,from)
                                       ("To" . ,destination)
                                       ("Body" . ,text))))
