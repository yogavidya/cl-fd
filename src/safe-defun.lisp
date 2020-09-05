(defpackage :safer-code/src/safe-defun
  (:use 
   :cl 
   :lisp-unit
   :iterate
   :safer-code/src/utilities
   :safer-code/src/results
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
  (let* (($fd
          (make-function-parameters-description lambda-list))
         ($formal-parameters 
          (fpd-lambda-list $fd))
         ($formal-parameters-flat
          (fpd-flat-lambda-list $fd))
         ($formal-parameter-types
          (fpd-type-list $fd))
         (#1=$return-type (find-if '@return-type-form
                                   function-body))
         (#1# (if #1# (second #1# ) 'T))
         ($restarts (rest (find-if '@restarts-form
                                   function-body)))
         (body (copy-list function-body))
         (body (delete-if '@return-type-form body))
         (body (delete-if '@restarts-form body))) ; no need to optimize
    (fmakunbound name)
    ;(declam (ftype (function ,in-types safer-code-return),name))
    `(defun ,name ,$formal-parameters
       (symbol-macrolet ((formal-parameters '(,@$formal-parameters-flat))
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
                               (iter (for formal in formal-parameters) 
                                 (for actual in actual-parameters)
                                 (for formal-type in formal-parameter-types)
                                 (collect
                                  (let 
                                      ((check-results 
                                        (function-parameter-check 
                                         ,$fd formal actual)))
                                    (when (not (first check-results))
                                      (list 
                                       formal 
                                       formal-type 
                                       actual 
                                       (type-of actual)))))))))
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
             (safe-function-return nil error-info))
           (return-type-error (,@$formal-parameters-flat error-info)
             ,(cons 'declare (list (append '(ignore) $formal-parameters-flat)))
             (safe-function-return nil error-info)))))))




