(in-package :twilio)

(declaim #.*compile-decl*)

(defvar *twilio-account-sid* nil
  "Default account SID to use")

(defvar *twilio-auth-token* nil
  "Default auth token to use")

(defun replace-all (string key replacement)
  (let ((match (cl-ppcre:all-matches (cl-ppcre:quote-meta-chars key) string)))
    (with-output-to-string (out)
      (loop
         with last = 0
         for (start end) on match by #'cddr
         do (progn
              (princ (subseq string last start) out)
              (princ replacement out )
              (setq last end))
         finally (princ (subseq string last) out)))))

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar #'(lambda (slot-name)
                                     `(,slot-name (if (and (slot-exists-p ,object-copy ',slot-name)
                                                           (slot-boundp ,object-copy ',slot-name))
                                                      (slot-value ,object-copy ',slot-name)
                                                      :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))
