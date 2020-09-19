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

(defmacro fd-instantiate (fd &key name as-lambda)
  (when (and name as-lambda)
    (error "keywords :NAME and :AS-LAMBDA are mutually exclusive."))
  (let ((block-name (intern (symbol-name (gensym "BLOCK"))))
        (function-name 
         (cond
          (as-lambda 'lambda)
          (name name)
          (T (funcall (symbol-value fd) :name)))))
    `(progn
       (when (null ,as-lambda) (fmakunbound ',function-name))
       (,@(if
              as-lambda '(lambda)
	    (list 'defun function-name))
	,(funcall (symbol-value fd) :ansi-lambda-list)
        ,(funcall (symbol-value fd) :documentation)
        (block ,block-name
          (restart-case
              (handler-bind
                  ((condition
                    #'(lambda(e)
                        (let
                            ((restart (find-restart
                                       (type-of e))))
                          (push (list (quote ,name) e) *conditions*)
                          (if restart
                              (return-from ,block-name 
                                (invoke-restart restart #1=,@(funcall (symbol-value fd) :parameter-symbols) e))
                            ;; handlers are invoked as (handler @,function-parameters condition-object)
                            (return-from ,block-name (fd-function-return nil e))))))) ; TODO
                (when (not (null '(#1#)))
                  (let* 
                      ((type-checks 
                        (apply ,fd 
                               (list :apply-all-parameters 
                                     (list :check-type #2=(list ,@(funcall (symbol-value fd) :parameter-symbols))))))
                       (validate-checks 
                        (apply ,fd 
                               (list :apply-all-parameters 
                                     (list :validate #2#))))
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
                        '(,@(funcall (symbol-value fd) :function-model))
                        :mismatched-parameters 
                        (iter (for p in type-errors)
                          (let ((pd (funcall ,fd :symbol-parameter-descriptor p))
                                (p-pos (position p '(#1#))))
                            (collect (list p (funcall pd :type) 
                                           (nth p-pos #2#)))))
                        :invalid-parameters
                        (iter (for p in validate-errors)
                          (let* ((pd (apply ,fd
                                           (list :symbol-parameter-descriptor p)))
                                (p-pos (position p '(#1#)))
                                (p-value (nth p-pos #2#)))
                            (collect 
                             (list p 
                                   (funcall pd :describe-validate-op p-value)
                                   p-value)))))))))
                
                
                (let ((result (multiple-value-list (progn ,@(funcall (symbol-value fd) :body)))))
                  (if (not (typep (first result) (quote ,(funcall (symbol-value fd) :return-type))))
                      (signal
                       (make-condition 'return-type-error
                                       :function-model '(,@(funcall (symbol-value fd) :function-model))
                                       :expected-type (quote ,(funcall (symbol-value fd) :return-type))
                                       :datum (first result))))
                  (fd-function-return T result)))
            ,@(funcall (symbol-value fd) :restarts)
            (arguments-check-error (#1# error-info)
              ,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
              (fd-function-return nil error-info))
            (return-type-error (#1# error-info)
              ,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
              (fd-function-return nil error-info))))))))