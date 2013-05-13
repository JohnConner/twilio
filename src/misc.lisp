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
