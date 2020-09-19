(defpackage :cl-fd/src/conditions
  (:use :cl :iterate :cl-fd/src/types)
  (:export 
   :*conditions*
   :return-type-error 
   :arguments-check-error 
   :mismatched-parameters 
   :invalid-parameters
   :function-model))

(in-package :cl-fd/src/conditions)

(defparameter *conditions* (list))

(define-condition return-type-error (type-error)
  ((function-model
    :accessor function-model
    :initarg :function-model
    :type cons))
  (:report (lambda (condition stream)
             (format stream "Return type mismatch for ~a: expected type ~a, but found ~a, which is a ~a."
                     (function-model condition)
                     (type-error-expected-type condition)
                     #1=(type-error-datum condition)
                     (type-of #1#)))))


(define-condition arguments-check-error (error)
  ((mismatched-parameters
    :accessor mismatched-parameters
    :initarg :mismatched-parameters
    :type (cons mismatched-argument))
   (invalid-parameters
    :accessor invalid-parameters
    :initarg :invalid-parameters
    :type (cons invalid-argument))
   (function-model
    :accessor function-model
    :initarg :function-model
    :type cons))
  (:report (lambda (condition stream)
             (format stream "~a"
             (with-output-to-string (s)
               (format t "Failed checks for function ~a:~%~{~a~}~{~a~}"
                       (function-model condition)
                       (if #1=(mismatched-parameters condition)
                         (iter (for c in #1#)
                           (collect 
                            (format nil "TYPE: PARAMETER ~a EXPECTED ~a BUT FOUND ~s (~a)~%"
                                    (mismatched-argument-name c)
                                    (mismatched-argument-type c)
                                    #2=(mismatched-argument-value c)
                                    (type-of #2#))))
                         nil)
                       (if #3=(invalid-parameters condition)
                         (iter (for c in #3#)
                           (collect 
                            (format nil "INVALID: ~a~%"
                                    (invalid-argument-op c))))
                         nil))
               s)))))

