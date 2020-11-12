(defpackage :cl-fd/src/instantiator
  (:use 
   :cl 
   :iterate
   :cl-fd/src/utilities
   :cl-fd/src/types 
   :cl-fd/src/results
   :cl-fd/src/conditions 
   :cl-fd/src/descriptors/function)
  (:export 
   :fd-instantiate))

(defpackage :cl-fd-restarts)
(in-package :cl-fd/src/instantiator)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *backup-readtable* (copy-readtable))
  (setf *readtable* *subst-readtable*))

(defun fd-instantiate (fd &key name as-lambda)
  #{
     #[ '%fmakunbound-form 
           (when (null as-lambda) 
             (list 'fmakunbound (list 'quote
                                      (or name (funcall fd :name))))) ]
     #[ '%unintern-form
           (when (null as-lambda) 
             (list 'unintern (list 'quote
                                      (or name (funcall fd :name))))) ]
     #[ '%fd fd ]
     #[@ '@function-header 
           (cond (as-lambda 'lambda)
                 (name (list 'defun name))
                 (T (list 'defun (funcall fd :name)))) ]
     #[  '%ansi-lambda-list (funcall fd :ansi-lambda-list) ] ; as a list
     #[@  '@formal-parameters (funcall fd :parameter-symbols) ] ; destructured
     #[@*  '@formal-parameters-not-null (funcall fd :parameter-symbols) ] ; destructured not-null
     #[  '%documentation (funcall fd :documentation) ]
     #[  '%block-name (intern (symbol-name (gensym))) ]
     #[  '%function-model (funcall fd :function-model) ]
     #[@  '@body (funcall fd :body) ] ; destructured
     #[  '%return-type (funcall fd :return-type) ]
     #[@*  '@restarts (funcall fd :restarts) ]
     #[@*  '%declare-ignore-parameters ; destructured not-null
	   (let ((syms (funcall fd :parameter-symbols)))
	     (when syms
	       (list (list 'declare
			   (append (list 'ignore)
				   syms))))) ]
    '(progn
       %fmakunbound-form
       %unintern-form
       (@function-header %ansi-lambda-list %documentation
                         (block %block-name
                           (restart-case
                               (handler-bind
                                   ((condition
                                     #'(lambda(e)
                                         (let
                                             ((restart (find-restart
                                                        (type-of e))))
                                           (if restart
                                               (return-from %block-name 
                                                 (invoke-restart restart @formal-parameters-not-null e)) ; #1# = parameter-symbols
                                             ;; handlers are invoked as (handler @,function-parameters condition-object)
                                             (return-from %block-name (fd-function-return nil e))))))) 
                                 (when (not (null '%ansi-lambda-list))
                                   (let* 
                                       ((type-checks 
                                         (apply %fd 
                                                (list :apply-all-parameters 
                                                      (list :check-type (list @formal-parameters))))) 
                                        (validate-checks 
                                         (apply %fd 
                                                (list :apply-all-parameters 
                                                      (list :validate (list @formal-parameters)))))
                                        (type-errors 
                                         (alexandria:flatten 
                                          (remove-if (lambda (x) (cdr x)) type-checks)))
                                        (validate-errors
                                         (alexandria:flatten
                                          (remove-if (lambda (x) (cdr x)) validate-checks)))
                                        (bad-parameters (union type-errors validate-errors)))
                                     (when bad-parameters
                                       (signal 
                                        (make-condition 
                                         'arguments-check-error
                                         :function-model 
                                         '%function-model
                                         :mismatched-parameters 
                                         (iter (for p in type-errors)
                                           (let ((pd (funcall %fd :symbol-parameter-descriptor p))
                                                 (p-pos (position p '(@formal-parameters))))
                                             (collect (list p (funcall pd :type) 
                                                            (nth p-pos (list @formal-parameters))))))
                                         :invalid-parameters
                                         (iter (for p in validate-errors)
                                           (let* ((pd (apply %fd
                                                             (list :symbol-parameter-descriptor p)))
                                                  (p-pos (position p '(@formal-parameters)))
                                                  (p-value (nth p-pos (list @formal-parameters))))
                                             (collect 
                                              (list p 
                                                    (funcall pd :describe-validate-op p-value)
                                                    p-value)))))))))
				 (let ((result (multiple-value-list (progn @body))))
                                     (if (not (typep (first result) '%return-type))
                                         (signal
                                          (make-condition 'return-type-error
                                                          :function-model '%function-model
                                                          :expected-type '%return-type
                                                          :datum (first result)))
                                       (fd-function-return T result))))
                             @restarts
                             (arguments-check-error (@formal-parameters-not-null error-info)
                               %declare-ignore-parameters
                               (fd-function-return nil error-info))
                             (return-type-error (@formal-parameters-not-null error-info)
                               %declare-ignore-parameters
                               (fd-function-return nil error-info)))))) } ) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* *backup-readtable*))



