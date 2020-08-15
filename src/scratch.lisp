(defpackage :safer-code/src/scratch
  (:use :cl)
  (:export :scratch
           :safe-defun :safe-function-success :safe-function-value
           :safe-function-return
           :safe-function-value-multiple :safe-function-extra-values))

(defpackage :safer-code-restarts)
(in-package :safer-code/src/scratch)

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

(defmacro safe-defun (name lambda-list &key documentation restarts function-body (in-types nil) (out-type T))
  `(progn
     ,(when in-types `(declaim (ftype (function ,in-types (cons boolean ,out-type)),name)))
     (defun ,name ,lambda-list
      (symbol-macrolet ((formal-parameters '(,@lambda-list))
                        (formal-parameter-types '(,@in-types))
                        (actual-parameters (list ,@lambda-list)))
       (restart-case
           (handler-bind
               ((condition
                 #'(lambda(e)
                     (let
                         ((restart (find-restart
                                    (type-of e))))
                       (push (list (quote ,name) e) *conditions*)
                       (if restart
                         (return-from ,name (invoke-restart restart ,@lambda-list e))
                        ;; handlers are invoked as (handler @,function-parameters condition-object)
                         (return-from ,name (safe-function-return nil e))))))) ;
               ;;;;
               ;#|
               (when (not (null formal-parameter-types))
                (let ((mismatched-parameters
                  (remove nil
                 (loop for index from 0 below (length formal-parameters)
                  collect
                    (when
                      (not
                       (typep (nth index actual-parameters)
                              (nth index formal-parameter-types)))
                      (list (nth index formal-parameters)
                           (nth index formal-parameter-types)
                           (nth index actual-parameters)
                            (type-of (nth index actual-parameters))))))))
                 (when mismatched-parameters
                  (signal (make-condition 'arguments-type-error
                                         :function-model (list (quote ,name) formal-parameters)
                                         :mismatched-parameters mismatched-parameters)))))

               ;|#
               ;;;;
               (let ((result (multiple-value-list (progn ,@function-body))))
                (if (not (typep (first result) (quote ,out-type)))
                  (signal
                   (make-condition 'return-type-error
                                   :expected-type (quote ,out-type)
                                   :datum (first result))))
                  (safe-function-return T result)))
         ,@restarts
         (arguments-type-error (,@lambda-list error-info)
                            ,(cons 'declare (list (append '(ignore) lambda-list)))
                            (safe-function-return nil (format nil "~a" error-info)))
         (return-type-error (,@lambda-list error-info)
                            ,(cons 'declare (list (append '(ignore) lambda-list)))
                            (safe-function-return nil (format nil "~a" error-info))))))
     (when ,documentation
       (setf (documentation (quote ,name) 'function) ,documentation))
     (symbol-function (quote ,name))))

(declaim (inline safe-function-success))
(defun safe-function-success (result)
  (car result))

(declaim (inline safe-function-value-multiple))
(defun safe-function-value-multiple (result)
  (> (length (cdr result)) 1))

(declaim (inline safe-function-value))
(defun safe-function-value (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (first (cdr result))
    (first (cdr result))))

(declaim (inline safe-function-extra-values))
(defun safe-function-extra-values (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (cdr (cdr result))
    (error "No multiple values")))


(declaim (inline safe-function-return))
(defun safe-function-return (success result)
  (list success result))

(define-condition pippo (condition) nil)

(safe-defun scratch (m n)
            :documentation
            "A first test for safe-defun"
            :in-types (number number)
            :out-type number
            :restarts
            ((pippo (a b)
                            (safe-function-return nil (list "arguments" a b)))
             (division-by-zero (a b error-info)
                                       (declare (ignore b error-info)) (scratch (1+ a) 1)))
  ;:function-body ((signal (make-condition 'pippo))))
            :function-body
            ((/ m n)))
