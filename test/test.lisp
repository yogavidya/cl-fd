(in-package :cl-fd/test)



(defparameter *fd* nil)
(defparameter *result* nil)

(defun %cl-fd-test-body (&key label lambda-list args fail body)
  ;(setf *fd* (eval `(make-function-descriptor temp ,lambda-list ,@body)))
  (car (subst-markers
   (list
    (list '%label label)
    (list '%lambda-list lambda-list)
    (list '@lambda-list lambda-list T) ; destructure
    (list '@lambda-list-not-null lambda-list T T) ; destructure not null
    (list '%args args) 
    (list '@args args T) ; destructure
    (list '@args-not-null args T T) ; destructure not-null
    (list '@body body T ) ; destructure 
    (list '%fail fail))
     '(define-test %label
        (setf *fd* (make-function-descriptor temp (@lambda-list-not-null) @body))
       (fd-instantiate *fd*)
       (setf *result* (temp @args-not-null))
       ;(format *error-output* "lambda list: ~a, return type: ~a, expected: ~a~%" 
               ;'%lambda-list
               ;(type-of (fd-function-value *result*))
               ;'%return-type)
       (format *error-output* "--------------------------------~%")
       (funcall *fd* :query)
       (format *error-output* "temp ~s => success = ~a, value = ~a~%" 
	       '%args 
	       (fd-function-success *result*)
	       (fd-function-value *result*))
       (if %fail
	   (assert-false (fd-function-success *result*))
         (assert-true (fd-function-success *result*)))))))

(defun cl-fd-test (&key label lambda-list args fail body)
  (eval (%cl-fd-test-body :label label :lambda-list lambda-list :args args :fail fail :body body)))

(remove-tests :all (find-package :cl-fd/test))

(cl-fd-test :label '01-empty 
            :lambda-list () 
            :args ())
(cl-fd-test :label '02-identity-atom 
            :lambda-list '(a) 
            :args '(1) 
            :body '(a))
(cl-fd-test :label '03-identity-list 
            :lambda-list '(a) 
            :args '((list 1 2 3)) 
            :body '(a))
(cl-fd-test :label '04-identity-atom-typed-arg 
            :lambda-list '((a string)) 
            :args '("pippo") 
            :body '(a))
(cl-fd-test :label '05-identity-atom-typed-arg+return 
            :lambda-list '((a string)) 
            :args '("pippo") 
            :body '((:function-return-type string) a))
(cl-fd-test :label '06-identity-atom-typed-arg+return-arg-error 
            :lambda-list '((a string)) 
            :args '(12)
            :fail t
            :body '((:function-return-type string) a))
(cl-fd-test :label '07-identity-atom-typed-arg+return-return-error 
            :lambda-list '((a string)) 
            :args '("pippo")
            :fail t
            :body '((:function-return-type number) a))
(cl-fd-test :label '08-identity-atom-required+keyword
            :lambda-list '(a &key b) 
            :args '("pippo")
            :body '((list a b)))


(setf lisp-unit:*print-errors* T)
(setf lisp-unit:*print-failures* T)

(defun test()
  (let ((result 
         (run-tests 
          (sort (list-tests (find-package :cl-fd/test)) 
                (lambda (a b) 
                  (string<= (symbol-name a)(symbol-name b)))) 
          (find-package :cl-fd/test))))
    result))

