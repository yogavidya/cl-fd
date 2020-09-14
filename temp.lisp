					;( (a string (check-fn check-form)) )
;;; => (&key (a string :check check-fn :default [default]))


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
         (p-validated-p (>= p-len 3))
         (p-validated-structured-p (and p-validated-p (consp (third p))))
         (p-name (if p-structured (first p) p))
         (p-type (or (and p-typed-p (second p)) 'T))
         (p-validate-fn-sym
           (and p-validated-p 
                (if p-validated-structured-p
                    (first (third p)) 		    
                    (third p))))
         (p-validate-fn 
           (if (fboundp p-validate-fn-sym)
               (symbol-function p-validate-fn-sym)
               (and p-validate-fn-sym
                    (error "Unknown validate function: ~a" p-validate-fn-sym))))
         (p-validate-parameter 
           (and p-validated-p p-validated-structured-p (second (third p))))
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
		      (p-validated-structured-p 
		       (funcall p-validate-fn request-arg p-validate-parameter))
		      (T (funcall p-validate-fn request-arg)))))
        (:describe-validate-op
         (let ((vo (list)))
	   (when p-validated-structured-p
	     (push p-validate-parameter vo))
	   (push p-name vo)
	   (push p-validate-fn-sym vo)
	   (and p-validate-fn-sym vo)))
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

(defmacro @self (request &optional request-arg)
  `(apply this (list  this ,request ,request-arg)))

(defmacro make-function-descriptor (name lambda-list &body body)
  `(let* ((name ',name)
          (original-lambda-list ',lambda-list)
          (parameter-sets (parse-lambda-list original-lambda-list)) ;all parameter descriptors
          (required-parameters 
            (parameter-names-from-descriptor-list  (first parameter-sets)))
          ;; next block generates lists for parameter symbols, like
          ;; the previous required-parameters.
          ;; bindings: optional-parameters, keyword-parameters, rest-parameter
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
          (documentation-strings  
            (iter  
              (while (<= i (length original-body)))
              (generate i upfrom 0)
              (next i)
              (when (stringp (first (nthcdr i original-body)))
                (collect (first (nthcdr i original-body))))))
          (return-type
					; NIL means unspecified, ie 'T
            (let 
		((first-form 
		   (first (nthcdr (length documentation-strings) original-body)))) 
              (when (valid-return-type-specification first-form)
                (second first-form))))
          (restarts (let
			((first-form (first (nthcdr (+ (length documentation-strings) (if return-type 1 0)) original-body))))
                      (when 
			  (valid-restarts-specification first-form flat-parameters-list)
                        (rest first-form))))
          (body 
            (nthcdr 
	     (+ (count-if-true return-type) 
		(count-if-true restarts) 
		(length documentation-strings)) 
	     original-body))
	  (closure 
	    (lambda (this request &optional request-arg)
	      (case request
		(:help 
		 '(:name :ansi-lambda-list :parameter-symbols :parameter-descriptors-required
		   :parameter-descriptors-xtra :parameter-descriptors-all 
		   :symbol-parameter-descriptor :return-type
		   :body :restarts :apply-parameter :apply-all-parameters :query))
		(:name name)
		(:ansi-lambda-list ansi-lambda-list)
		(:parameter-symbols flat-parameters-list)
		(:parameter-descriptors-required (first parameter-sets))
		(:parameter-descriptors-xtra
		 (cadadr parameter-sets))
		(:parameter-descriptors-all 
		 (append (car parameter-sets) (cadadr parameter-sets)))
		(:symbol-parameter-descriptor
		 (find-if 
		  (lambda (pd) 
		    (eq request-arg (funcall pd :name))) 
		  (apply this (list this :parameter-descriptors-all))))
		(:return-type (if return-type return-type 'T))
		(:body body)
		(:restarts restarts)
		(:apply-parameter ; (sym pd-request [pd-request-arg])
		 (let 
		     ((pd (@self :symbol-parameter-descriptor (first request-arg)))) 
		   (if pd
		       (funcall pd (second request-arg) (third request-arg))
		       (error "Invalid parameter ~a: should be in ~a"
			      (first request-arg)
			      ansi-lambda-list))))
		(:apply-all-parameters ;(sym pd-request [request-arglist]
                 (let* ((pd-list (@self :parameter-descriptors-all))
                        (arglist (second request-arg))
                        (request (first request-arg)))
		 (iter (for i from 0 below (length pd-list))
		   (let ((p-name (funcall (nth i pd-list) :name)))
		     (collect 
                      (cons p-name 
                            (funcall 
                             (nth i pd-list) 
                             request 
                             (nth i arglist))))))))
		(:query
		 (reverse (pairlis
			   (list :name
				 :original-lambda-list
				 :required
				 :optional
				 :keyword
				 :rest
				 :has-xtra
				 :xtra-type
				 :parameter-symbols
				 :ansi-lambda-list
				 :documentation
				 :return-type
				 :restarts
				 :body)
			   (list name
				 original-lambda-list
				 required-parameters
				 optional-parameters
				 keyword-parameters
				 rest-parameter
				 (not (null xtra-parameters))
				 xtra-type
				 flat-parameters-list
				 ansi-lambda-list
				 documentation-strings
				 return-type
				 restarts
				 body))))
		(otherwise (error "Unknown request for function descriptor: ~a" request))))))
     (lambda (command &optional request-arg)
       (apply closure (list closure command request-arg)))))


;; (in-package :safer-code/src/types)


(progn ; SAFER-CODE-RETURN
  (defun @safer-code-raturn-valid(x)
    (and (not (null x)) (listp x) (>= (length x) 2))
    (typep (first x) 'boolean))
  (deftype safer-code-return ()
    `(satisfies @safer-code-raturn-valid)))


;; (in-package :safer-code/src/conditions)

(defparameter *conditions* (list))

(define-condition return-type-error (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Return type mismatch: expected type ~a, but found ~a, which is a ~a."
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
                            (format nil "TYPE: PARAMETER ~a EXPECTED ~a BUT FOUND ~a (~a)~%"
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
                       


;; (in-package :safer-code/src/results)

(declaim (inline safe-function-success))
(defun safe-function-success (result)
  (car result))

(declaim (inline safe-function-value-multiple))
(defun safe-function-value-multiple (result)
  (consp (second result)))

(declaim (inline safe-function-value))
(defun safe-function-value (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (first (second result))
      (second result)))

(declaim (inline safe-function-extra-values))
(defun safe-function-extra-values (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (rest (second result))
      (error "No multiple values")))

(declaim (inline safe-function-return))
(defun safe-function-return (success result)
  (list success result))


(defpackage :safer-code-restarts)
;;(in-package :safer-code/src/safe-defun)

(defmacro @instantiate-function-descriptor (fd &key name as-lambda)
  (when (and name as-lambda)
    (error "keywords :NAME and :AS-LAMBDA are mutually exclusive."))
  (let ((block-name (gensym "BLOCK"))
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
	(symbol-macrolet 
	    ((formal-parameters '(,@(funcall (symbol-value fd) :parameter-symbols)))
	     (actual-parameters (list ,@(funcall (symbol-value fd) :parameter-symbols))))
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
				   (invoke-restart restart formal-parameters e))
				 ;; handlers are invoked as (handler @,function-parameters condition-object)
				 (return-from ,block-name (safe-function-return nil e))))))) ; TODO
		  (when (not (null formal-parameters))
		    (let* 
			((type-checks 
                          (apply ,fd 
                                 (list :apply-all-parameters 
                                       (list :check-type actual-parameters))))
                         (validate-checks 
                          (apply ,fd 
                                 (list :apply-all-parameters 
                                       (list :validate actual-parameters))))
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
                          (list (quote ,function-name) formal-parameters)
                          :mismatched-parameters 
                          (iter (for p in type-errors)
				(let ((pd (funcall ,fd :symbol-parameter-descriptor p)))
				  (collect (list p (funcall pd :type) 
                                                 (nth 
                                                  (position p formal-parameters) 
                                                  actual-parameters)))))
			  :invalid-parameters
                          (iter (for p in validate-errors)
				(let ((pd (apply ,fd
                                                 (list :symbol-parameter-descriptor p))))
				  (collect 
                                   (list p 
                                         (funcall pd :describe-validate-op)
                                         (nth 
                                                  (position p formal-parameters) 
                                                  actual-parameters))))))))))
			  

		  (let ((result (multiple-value-list (progn ,@(funcall (symbol-value fd) :body)))))
		    (if (not (typep (first result) (quote ,(funcall (symbol-value fd) :return-type))))
			(signal
			 (make-condition 'return-type-error
					 :expected-type (quote ,(funcall (symbol-value fd) :return-type))
					 :datum (first result))))
		    (safe-function-return T result)))
	      ,@(funcall (symbol-value fd) :restarts)
	      (arguments-type-error (formal-parameters-flat error-info)
		,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
		(safe-function-return nil error-info))
	      (return-type-error (formal-parameters-flat error-info)
		,(cons 'declare (list (append '(ignore) (funcall (symbol-value fd) :parameter-symbols))))
		(safe-function-return nil error-info)))))))))



(defmacro test ()
  `(progn 
     (defparameter *fd* 
       (make-function-descriptor temp 
           ((a string) &key (b number (< 10) 3)) 
         (:function-return-type string)
         (format nil "~a" (list a b)))) 
     (@instantiate-function-descriptor *fd*) 
     (defparameter *a* (temp "pippo" :b 3)) 
     (format t "~a" (second *a*))))

