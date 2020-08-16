(defpackage :safer-code/src/scratch
  (:use :cl)
  (:export :scratch
           :safe-defun :safe-function-success :safe-function-value
           :safe-function-return
   :safe-function-value-multiple :safe-function-extra-values))
; a-comment
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

(progn
  (defun @valid(x)
    (and (not (null x)) (listp x) (>= (length x) 2))
    (typep (first x) 'boolean))
  (deftype safer-code-return ()
    `(satisfies safer-code/src/scratch::@valid)))


(defun @args-valid (#1=lambda-list in-types)
  ;; examples:
  ;; (a b c &key d e) :in-types () :keyword-types ((e integer)(d string))
  ;; (a b c &rest stuff) :rest-types (or nil #validate-rest)
  (declare (ignore in-types))
  (let* ((optional-start (position '&optional lambda-list))
	 (keywords-start (position '&key lambda-list))
	 (rest-start (position '&rest lambda-list))
	 (check-1 (when (and keywords-start rest-start optional-start)
		    (error "mixing &rest, &key and &optional, if somehow admitted, is not advisable.")))
	 (not-required-start (first (remove nil (list keywords-start rest-start optional-start))))
	 (required-args (subseq lambda-list 0 not-required-start))
	 (optional-args (when #2=optional-start (subseq lambda-list (1+ #2#))))
	 (keyword-args (when #3=keywords-start (subseq lambda-list (1+ #3#))))
	 (rest-arg (when #4=rest-start (subseq lambda-list (1+ #4#)))))
    (declare (ignore check-1))
    (format t "required: ~a~%optional: ~a~%keywords: ~a~%rest: ~a~%"
	    required-args optional-args keyword-args rest-arg)))
  
  

(defmacro safe-defun (name lambda-list 
                           &key documentation restarts function-body 
                           (in-types nil) (out-type T))
  (when (not (= (length in-types) (length lambda-list)))
    (error "in-types and lambda list must have matching lengths"))
  `(progn
     (fmakunbound (quote ,name))
     (declaim (ftype (function ,in-types safer-code-return),name))
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
