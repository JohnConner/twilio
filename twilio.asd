(asdf:defsystem #:twilio
  :description "Common Lisp interface to twilio"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on (:cxml
               :xpath
               :drakma
               :cl-ppcre)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "conditions")
                                     (:file "twilio-sms")))))
