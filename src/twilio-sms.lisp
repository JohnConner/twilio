(in-package :twilio)

(declaim #.*compile-decl*)

(defclass sms-message-response ()
  ((sid         :type string
                :initarg :sid
                :reader sms-message-sid)
   (created     :initarg :created
                :reader sms-message-created)
   (updated     :initarg :updated
                :reader sms-message-updated)
   (sent        :initarg :sent
                :reader sms-message-sent)
   (account-sid :type string
                :initarg :account-sid
                :reader sms-message-account-sid)
   (to          :type string
                :initarg :to
                :reader sms-message-to)
   (from        :type string
                :initarg :from
                :reader sms-message-from)
   (text        :type string
                :initarg :text
                :reader sms-message-text)
   (url         :type string
                :initarg :url
                :reader sms-message-url))
  (:documentation "Class that describes an SMS message"))

(defmethod print-object ((obj sms-message-response) out)
  (print-unreadable-safely (sid from to text) obj out
    (format out "ID ~s FROM ~s TO ~s TEXT ~s" sid from to text)))

(defun make-sms-message (node)
  (labels ((read-param (key &optional accept-empty)
             (let ((nodes (xpath:evaluate (format nil "~a/text()" key) node)))
               (if (xpath:node-set-empty-p nodes)
                   (if accept-empty
                       nil
                       (error "Value ~s not found in document" key))
                   (dom:node-value (xpath:first-node nodes))))))
    (make-instance 'sms-message-response
                   :sid (read-param "Sid")
                   :created (read-param "DateCreated")
                   :updated (read-param "DateUpdated")
                   :sent (read-param "DateSent" t)
                   :account-sid (read-param "AccountSid")
                   :to (read-param "To")
                   :from (read-param "From")
                   :text (read-param "Body")
                   :url (read-param "Uri"))))

(defun authenticated-request (url account token expected-code &rest args)
  (multiple-value-bind (new-account new-token)
      (check-account-sid account token)
    (let ((result (multiple-value-list (apply #'drakma:http-request (format nil "https://api.twilio.com/2010-04-01/Accounts/~a/~a"
                                                                            new-account url)
                                              :basic-authorization (list new-account new-token)
                                              args))))
      (when (and expected-code (/= (cadr result) expected-code))
        (error "Illegal result code from HTTP request. Got ~a, expected ~a. Message: ~a"
               (car result) expected-code (nth 6 result)))
      (apply #'values result))))

(defun authenticated-request-parse (url account token expected-code &rest args)
  (let ((result (apply #'authenticated-request url account token expected-code args)))
    (cxml:parse-octets result (rune-dom:make-dom-builder))))

(defun send-sms (from destination text &key
                                         (account *twilio-account-sid*)
                                         (auth-token *twilio-auth-token*)
                                         status-callback
                                         application-sid)
  (let ((response (authenticated-request-parse "SMS/Messages" account auth-token 201
                                               :method :post
                                               :parameters `(("From" . ,from)
                                                             ("To" . ,destination)
                                                             ("Body" . ,text)
                                                             ,@(when status-callback `(("StatusCallback" . ,status-callback)))
                                                             ,@(when application-sid `(("ApplicationSid" . ,application-sid)))))))
    (make-sms-message (xpath:first-node (xpath:evaluate "TwilioResponse/SMSMessage" response)))))

(defun load-sms (id &key (account *twilio-account-sid*) (auth-token *twilio-auth-token*))
  (let ((response (authenticated-request-parse (format nil "SMS/Messages/~a" id) account auth-token 200)))
    (make-sms-message (xpath:first-node (xpath:evaluate "TwilioResponse/SMSMessage" response)))))

(defun list-all-sms (&key (account *twilio-account-sid*) (auth-token *twilio-auth-token*))
  (let* ((response (authenticated-request-parse "SMS/Messages" account auth-token 200))
         (messages-node (xpath:first-node (xpath:evaluate "/TwilioResponse/SMSMessages" response)))
         (nodes (xpath:map-node-set->list #'(lambda (node)
                                              (make-sms-message node))
                                          (xpath:evaluate "SMSMessage" messages-node))))
    (values nodes
            (parse-integer (dom:get-attribute messages-node "total")))))
