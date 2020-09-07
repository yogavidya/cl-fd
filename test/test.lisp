(in-package :safer-code/test)

(defparameter *sep* (make-string 80 :initial-element #\-))
(defparameter *result* nil)
(defparameter *test-names* (list))

(defun sorted-test-names ()
  (reverse (sort *test-names* 
                 (lambda (a b) 
                   (string> (symbol-name a) (symbol-name b))))))

(defmacro print-safe-defun-diagnostic(test-name lambda-list args &key expected-fail)
  `(format *error-output*
           "~a~%SAFE_DEFUN test: ~a ~% lambda list = ~a ~% args = ~a:~%~%Error: ~a
~:[~;~%NOTE: This function was expected to fail by the test, so this *can* still be OK~]~%~a~%~%"
           *sep*
           ',test-name
           '(,@lambda-list) ',args 
           (second *result*)
           ,expected-fail
           *sep*))

(defmacro test-safe-defun (test-name &key (expected-fail nil) lambda-list args xtra-tests body)
  `(progn
     (when (fboundp '%temp)
       (fmakunbound '%temp))
     (pushnew ',test-name *test-names*)
     (define-test ,test-name 
       (let*
           ((f (safe-defun %temp ,lambda-list ,@body)))
         (setf *result* (funcall f ,@args))
         (fmakunbound '%temp)
         (assert-true (consp *result*))
         (if ,expected-fail
             (assert-false (safe-function-success *result*))
           (assert-true (safe-function-success *result*)))
         ,@xtra-tests
         (when (and (consp *result*) (null (safe-function-success *result*)))
           (print-safe-defun-diagnostic ,test-name ,lambda-list ,args :expected-fail ,expected-fail))))))

(remove-tests :all :safer-code/test)

(defun safe-defun-test-01 ()
  (test-safe-defun 01-safe-defun/empty 
                   :lambda-list () 
                   :args nil 
                   :xtra-tests ((assert-equalp  (safe-function-value *result*) "Hello")) 
                   :body ("Hello")))

(defun safe-defun-test-02 ()
  (test-safe-defun 02-safe-defun/returning-typed-string 
                   :lambda-list () 
                   :args  nil 
                   :xtra-tests ((assert-equalp  (safe-function-value *result*) "Hello")) 
                   :body((=> string)
                         "Hello")))

(defun safe-defun-test-03 ()
  (test-safe-defun 03-safe-defun/identity 
                   :lambda-list (a) 
                   :args (42) 
                   :xtra-tests ((assert-eq  (safe-function-value *result*) 42)) 
                   :body (a)))

(defun safe-defun-test-04 ()
  (test-safe-defun 04-safe-defun/typed-args-mismatch/errortest 
                   :lambda-list ( (a string) )
                   :args (42) 
                   :expected-fail T
                   :xtra-tests ((assert-true (typep (safe-function-value *result*) 'safer-code/src/conditions:arguments-type-error)))
                   :body ((=> string) 
                          a)))

(defun safe-defun-test-05 ()
  (test-safe-defun 05-safe-defun/typed-arg/typed-return
                   :lambda-list ( (a string) )
                   :args ("pippo") 
                   :xtra-tests ((assert-equalp "pippo" (safe-function-value *result*)))
                   :body ((=> string) 
                          a)))

(defun safe-defun-test-06 ()
  (test-safe-defun 06-safe-defun/typed-arg/return-mismatch/errortest
                   :lambda-list  ( (a string) )  
                   :args ("pippo")
                   :expected-fail T
                   :xtra-tests ((assert-true (typep (safe-function-value *result*) 'safer-code/src/conditions:return-type-error)))
                   :body ((=> number) 
                          a)))

(defun safe-defun-test-07 ()
  (test-safe-defun 07-safe-defun/keyword-arg-identity
                   :lambda-list    ( (&key a)) 
                   :args ( :a "pippo")
                   :xtra-tests ((assert-equalp (safe-function-value *result*) "pippo"))
                   :body (a)))

(defun safe-defun-test-08 ()
  (test-safe-defun 08-safe-defun/keyword-typed-arg
                   :lambda-list    ( (&key ( a string))) 
                   :args ( :a "pippo")
                   :xtra-tests ((assert-equalp (safe-function-value *result*) "pippo"))
                   :body (a)))

(defun safe-defun-test-09 ()
  (test-safe-defun 09-safe-defun/keyword-arg-default
                   :lambda-list    ((&key (a string nil "pippo"))) 
                   :args ()
                   :xtra-tests ((assert-equalp (safe-function-value *result*) "pippo"))
                   :body (a)))

(defun safe-defun-test-10 ()
  (test-safe-defun 10-safe-defun/keyword-typed-arg/null-arg-value-check/arg-default
                   :lambda-list    ((&key (a string nil "pippo"))) 
                   :args ()
                   :xtra-tests ((assert-equalp (safe-function-value *result*) "pippo"))
                   :body (a)))

(defun safe-defun-test-11 ()
  (test-safe-defun 11-safe-defun/keyword-typed-arg/function-arg-value-check/arg-default
                   :lambda-list    ((&key (a fixnum evenp 2))) 
                   :args ()
                   :xtra-tests ((assert-eq (safe-function-value *result*) 2))
                   :body (a)))
#|
 Nota a me stesso:
il test 11 fallisce perch� la funzione restituisce una condizione.
Tema: il velore (second) del safer-code-return dovrebbe sempre essere una lista?
Come gestire la lettura del valore restituito?
Pensaci, Salvatorino!
|#

(defun collect-safe-defun-test-generators ()
  (let ((flist (list)))
    (with-package-iterator (next-symbol '(:safer-code/test)
                                        :internal)
      (loop
       (multiple-value-bind (more? symbol) (next-symbol)
         (if more?
             (when (and 
                    (> (length #2=(symbol-name symbol)) 
                       #3=(length #4="SAFE-DEFUN-TEST-")) 
                    (equalp #4#  (subseq #2# 0 #3#)))
               (push symbol flist))
           (return)))))
    (reverse flist)))

(defun define-safe-defun-tests ()
  (dolist (f (collect-safe-defun-test-generators))
    (funcall f)))     

(defun define-all-tests ()
  (define-safe-defun-tests))

(defun test ()
  (define-all-tests)
  (setf *print-errors* T)
  (setf *print-failures* T)
  (use-debugger T)
  (let 
      ((test-results (run-tests (sorted-test-names) :safer-code/test)))
    (print-errors test-results *error-output*)))
