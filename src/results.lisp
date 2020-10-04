(defpackage :cl-fd/src/results
	(:use :cl :cl-fd/src/utilities :cl-fd/src/types)
	(:export :fd-function-success 
         :fd-function-value-multiple 
         :fd-function-value
         :fd-function-extra-values
         :fd-function-return
         :fd-function-explain))

(in-package :cl-fd/src/results)

(defun-inline fd-function-success ((result fd-return)) T
  (car result))

(defun-inline fd-function-value-multiple ((result fd-return)) T
  (> (length result) 2))

(defun-inline fd-function-extra-values ((result fd-return)) 
    (or null cons) 
    (and (fd-function-success result)
         (nthcdr 2 result)))

(defun-inline fd-function-value ((result fd-return)) T
  (second result))

(defun-inline fd-function-return  (success result) fd-return
  (append (list success) (if (atom result) (list result)  result)))

(defun-inline fd-function-explain ((result fd-return) &optional (stream *standard-output*)) T
  (if (fd-function-success result)
      (format stream "Nothing to explain, function call successful.~%")
    (format stream "~a: ~a~%" (type-of #1=(fd-function-value result)) #1#)))

