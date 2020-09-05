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

(defun safe-defun-test-1 ()
  (test-safe-defun 01-safe-defun-empty 
                   :lambda-list () 
                   :args nil 
                   :xtra-tests ((assert-equalp  (car (safe-function-value *result*)) "Hello")) 
                   :body ("Hello")))

(defun safe-defun-test-2 ()
  (test-safe-defun 02-safe-defun-returning-typed-string 
                   :lambda-list () 
                   :args  nil 
                   :xtra-tests ((assert-equalp  (car (safe-function-value *result*)) "Hello")) 
                   :body((=> string)
                         "Hello")))

(defun safe-defun-test-3 ()
  (test-safe-defun 03-safe-defun-identity 
                   :lambda-list (a) 
                   :args (42) 
                   :xtra-tests ((assert-eq  (car (safe-function-value *result*)) 42)) 
                   :body (a)))

(defun safe-defun-test-4 ()
  (test-safe-defun 04-safe-defun-typed-args-mismatch 
                   :lambda-list ( (a string) )
                   :args (42) 
                   :expected-fail T
                   :xtra-tests ((assert-true (typep (safe-function-value *result*) 'safer-code/src/conditions:arguments-type-error)))
                   :body ((=> string) 
                          a)))

(defun safe-defun-test-5 ()
  (test-safe-defun 05-safe-defun-typed-fixed-return
                   :lambda-list ( (a string) )
                   :args ("pippo") 
                   :xtra-tests ((assert-equalp "pippo" (first (safe-function-value *result*))))
                   :body ((=> string) 
                          a)))

(defun safe-defun-test-6 ()
  (test-safe-defun 06-safe-defun-typed-arg-return-mismatch
                   :lambda-list  ( (a string) )  
                   :args ("pippo")
                   :expected-fail T
                   :xtra-tests ((assert-true (typep (second *result*) 'safer-code/src/conditions:return-type-error)))
                   :body ((=> number) 
                          a)))


(defun collect-safe-defun-test-generators ()
  (let ((flist (list)))
    (with-package-iterator (next-symbol '(:safer-code/test)
                                        :internal)
      (loop
       (multiple-value-bind (more? symbol) (next-symbol)
         (if more?
             (when (and (> (length #2=(symbol-name symbol)) #3=(length #4="SAFE-DEFUN-TEST-")) (equalp #4#  (subseq #2# 0 #3#)))
               (push symbol flist))
           (return)))))
    (reverse flist)))

(defun define-safe-defun-tests ()
  (dolist (f (collect-safe-defun-test-generators))
    (funcall f)))
#|
      (safe-defun temp (&key a) a)
      (setf #1# (temp :a "pippo-key"))
      (assert-true (consp #1#))
      (assert-true (first #1#))
      (assert-equalp (caadr #1#) "pippo-key")
      (safe-defun temp (&key (a string)) a)
      (setf #1# (temp :a "pippo-key-typed"))
      (assert-true (consp #1#))
      (assert-true (first #1#))
      (assert-equalp (caadr #1#) "pippo-key-typed")
        ;(safe-defun temp (&key (a string "pippo-key-typed-default")))
        ;(setf #1# (temp))
        ;(assert-true (consp #1#))
      (test-safe-defun safe-defun-typed-keyword-with-default
                       :lambda-list (&key (a string "pippo-key-typed-default"))
                       :args nil
                       :body (a))
      ;(assert-equalp (safe-function-return #1#) "pippo-key-typed-default")
|#
     

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
