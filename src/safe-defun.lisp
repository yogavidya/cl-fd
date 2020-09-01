(defpackage :safer-code/src/safe-defun
  (:use :cl 
   :safer-code/src/conditions 
   :safer-code/src/types 
   :safer-code/src/function-parameters
   :safer-code/src/lambda-list-parser)
  (:export 
   :safe-defun :safe-function-success :safe-function-value :safe-function-return
   :safe-function-value-multiple :safe-function-extra-values))

(defpackage :safer-code-restarts)
(in-package :safer-code/src/safe-defun)

(defun @return-type-form (form)  
  (when (consp form) 
    (equalp (symbol-name (first form)) "=>")))

(defun @restarts-form (form)  
  (when (consp form) 
    (equalp (symbol-name (first form)) "/RESTARTS")))


(defmacro safe-defun (name lambda-list &rest function-body)
  (let* ((function-descriptor
          (make-function-parameters-description lambda-list))
         ($formal-parameters 
          (fpd-lambda-list function-descriptor))
         ($formal-parameters-flat
          (fpd-flat-lambda-list function-descriptor))
         ($formal-parameter-types
          (fpd-type-list function-descriptor))
         (#1=$return-type (find-if '@return-type-form
                           function-body))
         (#1# (if #1# (second #1#) 'T))
         ($restarts (rest (find-if '@restarts-form
                           function-body)))
         (body (copy-list function-body))
         (body (delete-if '@return-type-form body))
         (body (delete-if '@restarts-form body))) ; no need to optimize
    (fmakunbound name)
    ;(declaim (ftype (function ,in-types safer-code-return),name))
    `(defun ,name ,$formal-parameters
         (symbol-macrolet ((formal-parameters '(,@$formal-parameters))
                           (formal-parameter-types '(,@$formal-parameter-types))
                           (actual-parameters (list ,@$formal-parameters-flat)))
           (restart-case
               (handler-bind
                   ((condition
                     #'(lambda(e)
                         (let
                             ((restart (find-restart
                                        (type-of e))))
                           (push (list (quote ,name) e) *conditions*)
                           (if restart
                               (return-from ,name (invoke-restart restart ,@$formal-parameters-flat e))
                             ;; handlers are invoked as (handler @,function-parameters condition-object)
                             (return-from ,name (safe-function-return nil e))))))) ;
                 (when (not (null formal-parameters))
                   (let 
                       ((mismatched-parameters
                         (remove nil
                                 (loop for formal in formal-parameters 
                                       and actual in actual-parameters
                                       and formal-type in formal-parameter-types
                                       collect
                                       (let 
                                           ((check-results 
                                             (function-parameter-check 
                                              ,function-descriptor formal actual)))
                                         (when (not (first check-results))
                                           (list 
                                            formal 
                                            formal-type 
                                            actual 
                                            (type-of actual))))))))
                     (when mismatched-parameters
                       (signal (make-condition 'arguments-type-error
                                               :function-model (list (quote ,name) formal-parameters)
                                               :mismatched-parameters mismatched-parameters)))))

                 (let ((result (multiple-value-list (progn ,@body))))
                   (if (not (typep (first result) (quote ,$return-type)))
                       (signal
                        (make-condition 'return-type-error
                                        :expected-type (quote ,$return-type)
                                        :datum (first result))))
                   (safe-function-return T result)))
             ,@$restarts
             (arguments-type-error (,@$formal-parameters-flat error-info)
               ,(cons 'declare (list (append '(ignore) $formal-parameters-flat)))
               (safe-function-return nil (format nil "~a" error-info)))
             (return-type-error (,@$formal-parameters-flat error-info)
               ,(cons 'declare (list (append '(ignore) $formal-parameters-flat)))
               (safe-function-return nil (format nil "~a" error-info))))))))

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

