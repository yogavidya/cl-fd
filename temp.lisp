

(defparameter *ll* 
  '(&key (a string :check check-fn :default [default])))

(defun extra-sub-lambda-lists-positions (ll &optional (start 0))
  (let ((found 
          (position-if 
	   (lambda (x) (member x '(&key &optional &rest)))
	   ll :start start)))
    (when found (cons found (extra-sub-lambda-lists-positions ll (1+ found))))))

(defun extra-sub-lambda-list (ll)
  (let* ((xtra-pos (extra-sub-lambda-lists-positions ll))
         (xtra-pos 
           (if (<= (length xtra-pos) 1) 
               (first xtra-pos)
               (error "more than one occurrence of &KEY|&OPTIONAL|&REST in lambda list.")))
         (xtra (when xtra-pos (nthcdr xtra-pos ll))))
    xtra))

(defun split-lambda-list (ll)
  (let* ((xtra (extra-sub-lambda-list ll))
         (required (if xtra
                       (subseq ll 0 (- (length ll)  (length xtra)))
                       ll)))
    (list required xtra)))


(defun make-parameter-descriptor (p)
;; SYM | (SYM [TYPE] [CHECK-FN | (CHECK-FN CHECK-FORM)] [DEFAULT])
  (assert (or (atom p) (and (listp p) (>= #1=(length p) 1) (<= #1# 4))))
  (let* ((p-structured (not (atom p)))
         (p-len (if p-structured (length p) 1))
         (p-typed-p (>= p-len 2))
         (p-validated-p (and (>= p-len 3) (third p)))
         (p-validated-structured-p (and p-validated-p (consp (third p))))
         (p-name (if p-structured (first p) p))
         (p-type (or (and p-typed-p (second p)) 'T))
         (p-validate-fn-input
           (and p-validated-p 
                    (third p)))
         (p-validate-fn
	   (cond
	     ((and 
		(symbolp p-validate-fn-input) 
		(ignore-errors (fboundp p-validate-fn-input))) 
	       (symbol-function p-validate-fn-input))
	      ((and 
		(consp p-validate-fn-input) 
		(eq (car p-validate-fn-input) 'lambda))
	       (let ((maybe-fn (eval (read-from-string (format nil "~s" p-validate-fn-input)))))
		 (when (functionp maybe-fn) maybe-fn)))
	      (p-validate-fn-input (error "Unknown validate function: ~a" p-validate-fn-input))
	      (t nil)))
         (p-default (and p-structured (fourth p))))
    (lambda (request &optional request-arg) 
      (case request
        (:name p-name)
        (:type p-type)
        (:default p-default)
        (:check-type (subtypep (type-of request-arg) p-type ))
        (:validate (ignore-errors 
		    (cond
		      ((not p-validated-p) T)
		      (T (funcall p-validate-fn request-arg)))))
        (:describe-validate-op
         (let ((vo (list 'FOR 'PARAMETER p-name '= request-arg)))
	   (if p-validated-structured-p
	     (push p-validate-fn-input vo)
             (push `(,p-validate-fn-input ,p-name) vo))
	   (and p-validate-fn-input (format nil "~{~s ~}" vo))))
        (otherwise 
	 (error "Unknown request to parameter descriptor: ~a ~a" request request-arg))))))

(defun parse-sub-lambda-list (sub-ll)
  (mapcar #'make-parameter-descriptor sub-ll))

(defun parse-extra-sub-lambda-list  (xtra)
  (let ((xtra-type (first xtra)))
    (list xtra-type (parse-sub-lambda-list (rest xtra)))))

(defun parse-lambda-list (ll)
  (let* ((split-ll (split-lambda-list ll))
         (required (first split-ll))
         (parsed-required (parse-sub-lambda-list required))
         (xtra (second split-ll))
         (parsed-xtra (parse-extra-sub-lambda-list xtra)))
    (list parsed-required  parsed-xtra)))

(defun parameter-descriptor-list-type (parameter-descriptors)
  (let ((maybe-xtra (first parameter-descriptors)))
    (if (typep maybe-xtra 'function)
        'required
        maybe-xtra)))

(defun parameter-names-from-descriptor-list (parameter-descriptors)
  (let* ((pd-list-type (parameter-descriptor-list-type parameter-descriptors))
         (pd-list 
           (if (eq pd-list-type 'required) 
               parameter-descriptors 
               (cadr parameter-descriptors))))
    (mapcar
     (lambda (p) (funcall p :name))
     pd-list)))

(defun valid-return-type-specification (exp)
  (and
   (consp exp)
   (= (length exp) 2)
   (eq (first exp) :function-return-type)))

(defun valid-restarts-specification (exp parameter-symbols)
  (and
   (consp exp)
   (>= (length exp) 2)
   (eq (first exp) :function-restarts)
   (iter check-restarts-syntax 
     (for r in (rest exp)) 
     (when 
	 (not (ignore-errors (subtypep (first r) 'condition))) 
       (error "invalid condition type in restart: ~a" (first r)))
     (when (not (= (length (second r)) (1+ (length parameter-symbols))))  
       (error "restart for condition ~a must have exactly this lambda list: ~a"
	      (first r)
	      (destructuring-bind 
		  ((&rest args) condition) 
		  (list parameter-symbols 'condition-instance) 
		`(,@args ,condition))))
     (finally (return-from check-restarts-syntax T)))))

(declaim (inline count-if-true))
(defun count-if-true (x)
  (if x 1 0))


(defun list-head-if (l test)
  (labels 
      ((rec (l) 
         (if (funcall test (car l)) 
             (cons (car l) (rec (rest l))) 
           nil))) 
    (rec l)))

(defun step-request-list (rl)
  (if (atom (car rl))
      (nthcdr 2 rl)
    (cdr rl)))


(defmacro make-function-descriptor (name lambda-list &body body)
  `(let* ((name ',name)
          (original-lambda-list ',lambda-list)
          (parameter-sets (parse-lambda-list original-lambda-list)) ;all parameter descriptors, structured
          (parameter-descriptors (append (car parameter-sets) (cadadr parameter-sets))) ;all parameter descriptors, flat
          (required-parameters 
            (parameter-names-from-descriptor-list  (first parameter-sets)))
          ;; next block generates bindings for lists for extra parameter symbols
          ;; symbols: optional-parameters, keyword-parameters, rest-parameter
          ,@(iter 
              (for (sym . pd-type) in
                   '((optional-parameters . &optional)
                     (keyword-parameters . &key)
                     (rest-parameter . &rest)))
              (collect
		  `(,sym
		    (when (eq (parameter-descriptor-list-type (second parameter-sets)) 
			      ',pd-type)
		      (parameter-names-from-descriptor-list 
                       (second parameter-sets))))))
          (xtra-parameters ; list of symbols for non-required parameters 
            (first (delete-if #'null 
                              (list optional-parameters keyword-parameters rest-parameter))))
          (xtra-type ; keyword for non-required parameters (&optional | &key | &rest)
            (when xtra-parameters 
              (cdr 
	       (first 
		(delete-if 
		 (lambda (x) (null (car x)))
		 (list (cons keyword-parameters  '&key)
		       (cons optional-parameters '&optional)
		       (cons rest-parameter '&rest)))))))
          (flat-parameters-list (append required-parameters xtra-parameters)) ; all parameter symbols
          (ansi-lambda-list ; the lambda list to be consumed by DEFUN 
            (let ((result (list)))
              (iter (for p in (first parameter-sets))
		(push (funcall p :name) result))
              (and xtra-type
                   (push xtra-type result)
                   (iter (for p in (cadr (second parameter-sets)))
		     (if (eq xtra-type '&optional)
			 (push #1=(funcall p :name) result)
			 (if #2=(funcall p :default)
			     (push (list #1# #2#) result)
			     (push #1# result)))))
              (reverse result)))
          (original-body '(,@body)) ; the BODY argument to make-function-descriptor
          (documentation-strings   (list-head-if original-body #'stringp))
          (meta-forms 
           (list-head-if 
            (nthcdr (length documentation-strings) original-body)
            #'(lambda (f) 
              (and (consp f)
                   (member (car f) '(:function-restarts :function-return-type))))))
          (return-type (let ((rf (find-if #'(lambda (f) (eq (car f) :function-return-type)) meta-forms)))
                         (or
                          (and (valid-return-type-specification rf)
                               (rest rf))
                          T)))
          (restarts (let ((rf (find-if #'(lambda (f) (eq (car f) :function-restarts)) meta-forms)))
                      (and (valid-restarts-specification rf flat-parameters-list)
                           (rest rf))))
          (body 
            (nthcdr 
	     (+ (length meta-forms) 
		(length documentation-strings)) 
	     original-body))
          (request-list
           (list
            :name name 
            :ansi-lambda-list ansi-lambda-list
            :parameter-symbols flat-parameters-list
            :parameter-descriptors-required (first parameter-sets)
            :parameter-descriptors-xtra (cadadr parameter-sets)
            :parameter-descriptors-all (append 
                                        (car parameter-sets) 
                                        (cadadr parameter-sets))
            '(:symbol-parameter-descriptor PARAMETER-SYMBOL) 
            :function-model (list name ansi-lambda-list)
            :return-type (if return-type return-type 'T)
            :body body
            :restarts restarts
            '(:apply-parameter 
              (SYMBOL PARAMETER-DESCRIPTOR-REQUEST [ARGUMENT])) 
            '(:apply-all-parameters
              (PARAMETER-DESCRIPTOR-REQUEST [ARGUMENT LIST])) 
            :documentation (let 
                               ((s 
                                 (and documentation-strings 
                                      (format nil 
                                              "~{~a~%~}" 
                                              documentation-strings))))
                             (if s 
                                 (subseq s 0 (2- (length s)))
                               "TODO: documentation"))
            '(:query)))
          (query-ht 
           (let ((qht (make-hash-table :test 'eq)))
             (iter (for r on request-list by #'step-request-list)
               (when (atom (car r))
                 (setf 
                  (gethash (car r) qht)
                  (second r))))
             qht))
          (help 
           (iter (for r on request-list by #'step-request-list)
                 (collect (car r))))
          (request-tokens-parameterless 
           (iter (for r on request-list by #'step-request-list)
             (when (atom (car r))
                 (collect (car r)))))
	  (closure 
	    (lambda (this request &optional request-arg)
              (cond 
               ((member request request-tokens-parameterless)
                (gethash request query-ht))
               ((eq request :help) (pprint help))
               ((eq request :query)
                (iter (for r in request-tokens-parameterless)
                  (format t "~s: ~s~%" r (gethash r query-ht))))
               ((eq request :symbol-parameter-descriptor)
		 (find-if 
		  (lambda (pd) 
		    (eq request-arg (funcall pd :name))) 
		  parameter-descriptors))
               ((eq request :apply-parameter) ; (sym pd-request [pd-request-arg])
                (let 
                    ((pd (@self :symbol-parameter-descriptor (first request-arg)))) 
                  (if pd
                      (funcall pd (second request-arg) (third request-arg))
                    (error "Invalid parameter ~a: should be in ~a"
                           (first request-arg)
                           ansi-lambda-list))))
               ((eq request :apply-all-parameters) ; (sym pd-request [request-arglist]
                (let* ((arglist (second request-arg))
                       (request (first request-arg)))
                  (iter (for i from 0 below (length parameter-descriptors))
                    (let ((p-name (funcall (nth i parameter-descriptors) :name)))
                      (collect 
                       (cons p-name 
                             (funcall 
                              (nth i parameter-descriptors) 
                              request 
                              (nth i arglist))))))))
               (T (error "Unknown request for function descriptor: ~a" request))))))
     (lambda (command &optional request-arg)
       (apply closure (list closure command request-arg)))))


;; (in-package :cl-fd/src/types)


(progn ; TYPE: FD-RETURN
  (defun @fd-return-valid(x)
    (and (not (null x)) (listp x) (>= (length x) 2))
    (typep (first x) 'boolean))
  (deftype fd-return ()
    `(satisfies @fd-return-valid)))


;; (in-package :cl-fd/src/conditions)

(defparameter *conditions* (list))

(define-condition return-type-error (type-error)
  ((function-model
    :accessor function-model
    :initarg :function-model
    :type cons))
  (:report (lambda (condition stream)
             (format stream "Return type mismatch for ~a: expected type ~a, but found ~a, which is a ~a."
                     (function-model condition)
                     (type-error-expected-type condition)
                     #1=(type-error-datum condition)
                     (type-of #1#)))))

(defun check-mismatched-argument (e)
  (and (= (length e) 3)
       (symbolp (first e))
       (symbolp (second e))))
(deftype mismatched-argument ()
  `(and
    (cons)
    (satisfies check-mismatched-argument)))
(defun mismatched-argument-name (e)
  (first e))
(defun mismatched-argument-type (e)
  (second e))
(defun mismatched-argument-value (e)
  (third e))

(defun check-invalid-argument (e)
  (and (= (length e) 3)
       (symbolp (first e))
       (consp #1=(second e))
       (or
        (= #2=(length #1#) 2)
        (= #2# 3))
       (not (null (first #1#)))))
(deftype invalid-argument ()
  `(and
    (cons)
    (satisfies check-invalid-argument)))
(defun invalid-argument-name (e)
  (first e))
(defun invalid-argument-op (e)
  (second e))
(defun invalid-argument-value (e)
  (third e))


(define-condition arguments-check-error (error)
  ((mismatched-parameters
    :accessor mismatched-parameters
    :initarg :mismatched-parameters
    :type (cons mismatched-argument))
   (invalid-parameters
    :accessor invalid-parameters
    :initarg :invalid-parameters
    :type (cons invalid-argument))
   (function-model
    :accessor function-model
    :initarg :function-model
    :type cons))
  (:report (lambda (condition stream)
             (print
             (with-output-to-string (s)
               (format t "Failed checks for function ~a:~%~{~a~}~{~a~}"
                       (function-model condition)
                       (if #1=(mismatched-parameters condition)
                         (iter (for c in #1#)
                           (collect 
                            (format nil "TYPE: PARAMETER ~a EXPECTED ~a BUT FOUND ~s (~a)~%"
                                    (mismatched-argument-name c)
                                    (mismatched-argument-type c)
                                    #2=(mismatched-argument-value c)
                                    (type-of #2#))))
                         nil)
                       (if #3=(invalid-parameters condition)
                         (iter (for c in #3#)
                           (collect 
                            (format nil "INVALID: ~a~%"
                                    (invalid-argument-op c))))
                         nil))
               (format nil "~a" s))
             stream))))
                       


;; (in-package :cl-fd/src/results)

(declaim (inline fd-success))
(defun fd-success (result)
  (car result))

(declaim (inline fd-value-multiple))
(defun fd-value-multiple (result)
  (consp (second result)))

(declaim (inline fd-value))
(defun fd-value (result)
  (if (and (fd-success result)
           (fd-value-multiple result))
      (first (second result))
      (second result)))

(declaim (inline fd-extra-values))
(defun fd-extra-values (result)
  (if (and (fd-success result)
           (fd-value-multiple result))
      (rest (second result))
      (error "No multiple values")))

(declaim (inline fd-return))
(defun fd-return (success result)
  (list success result))


(defpackage :cl-fd-restarts)
;;(in-package :safer-code/src/safe-defun)

(defmacro @instantiate-function-descriptor (fd &key name as-lambda)
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
                            (return-from ,block-name (fd-return nil e))))))) ; TODO
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
                  (fd-return T result)))
            ,@(funcall (symbol-value fd) :restarts)
            (arguments-type-error (#1# error-info)
              ,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
              (fd-return nil error-info))
            (return-type-error (#1# error-info)
              ,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
              (fd-return nil error-info))))))))



(defmacro test (a b)
  `(progn 
     (defparameter *fd* 
       (make-function-descriptor temp 
           ((a number) &key (b number (lambda (x) (< x 10)) 3))
         "This is a function of two arguments, A and B"
         "returning printed A / B"
         (:function-restarts (division-by-zero (a b condition) (temp a :b 1)))
         (:function-return-type string)
         (format nil "~a" (/ a b))))
     (let ((fn
            (@instantiate-function-descriptor *fd*))) 
       (defparameter *a* (temp ,a :b ,b)) 
       (format t "~a" (second *a*)))))

