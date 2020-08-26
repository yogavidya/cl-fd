(defpackage :safer-code/src/conditions
  (:use :cl)
  (:export 
   :*conditions*
   :return-type-error 
   :arguments-type-error :mismatched-parameters :function-model))

(in-package :safer-code/src/conditions)

(defparameter *conditions* (list))

(define-condition return-type-error (type-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Return type mismatch: expected type ~a, but found ~a, which is a ~a."
		     (type-error-expected-type condition)
		     #1=(type-error-datum condition)
		     (type-of #1#)))))

(define-condition arguments-type-error (error)
  ((mismatched-parameters
    :accessor mismatched-parameters
    :initarg :mismatched-parameters
    :type cons)
   (function-model
    :accessor function-model
    :initarg :function-model
    :type cons))
  (:report (lambda (condition stream)
	     (format stream
		     "Argument type check failed in ~a: ~:{~%* ~a: (~a): found ~a, which is a ~a~}"
		     (function-model condition)
		     (mismatched-parameters condition)))))
