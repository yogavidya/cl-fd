(in-package :cl-fd/test)



(defparameter *fd* nil)
(defparameter *result* nil)

(defun %cl-fd-test-body (&key label lambda-list args fail body)
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
        (setf *fd* (make-function-descriptor test-function (@lambda-list-not-null) @body))
       ;(fd-instantiate *fd*)
       (setf *result* (funcall (fd-instantiate *fd* :as-lambda T) @args-not-null))
       (format *error-output* #1="~a~%" #2=(make-string 80 :initial-element #\-))
       (funcall *fd* :query)
       (format *error-output* "temp ~s output: ~%*  ~:[condition report: \"~a\"~a~;value: ~s~]~%"
	       '%args 
	       (fd-function-success *result*)
	       (fd-function-value *result*)
               (format nil "~%(*** NOTE: a condition was~:[ NOT~;~] expected)~%" %fail))
       (if %fail
	   (assert-false (fd-function-success *result*))
         (assert-true (fd-function-success *result*)))
       (format *error-output* #1# #2#)
))))

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
            :fail T
            :body '((:function-return-type number) a))
(cl-fd-test :label '08-identity-atom-required+keyword
            :lambda-list '(a &key b) 
            :args '("pippo")
            :body '((list a b)))
(cl-fd-test :label '09-identity-values 
            :lambda-list '(a) 
            :args '((list 1 2 3)) 
            :body '((values a)))
(cl-fd-test :label '10-boolean-keyword
            :lambda-list '((a string) 
                           (b fixnum) 
                           &key (add-prefix boolean nil) 
                           (prefix string nil "=> "))
            :args '("test" 12 :add-prefix t) 
            :body '((if add-prefix 
                        (format nil "~a ~s ~d" prefix a b) 
                      (format nil "~s ~d" a b))))


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

#|
(cl-fd/src/descriptors/function:make-function-descriptor pippo ((a string) (b fixnum) &key (add-prefix boolean T) (prefix string "=> ")) (:function-return-type string) (if add-prefix (format nil "~a ~s ~n" prefix a b) (format nil "~s ~n" a b)))
|#
