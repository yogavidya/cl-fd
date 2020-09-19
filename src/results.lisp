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
  (consp (second result)))

(defun-inline fd-function-value ((result fd-return)) T
  (if (and (fd-function-success result)
           (fd-function-value-multiple result))
      (first (second result))
    (second result)))

(defun-inline fd-function-extra-values ((result fd-return)) cons
  (if (and (fd-function-success result)
           (fd-function-value-multiple result))
      (rest (second result))
    (error "No multiple values")))

(defun-inline fd-function-return  (success result) fd-return
  (list success result))

(defun-inline fd-function-explain ((result fd-return)) T
  (if (fd-function-success result)
      (print "Nothing to explain, function call successful")
    (format nil "~a~%" (fd-function-value result))))

