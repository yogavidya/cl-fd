(defpackage :cl-fd/src/descriptors/function
  (:use :cl :iterate :cl-fd/src/descriptors/parameter)
  (:export :make-function-descriptor))

(in-package :cl-fd/src/descriptors/function)

(defun parameter-names-from-descriptor-list (parameter-descriptors)
  (let* ((pd-list-type (parameter-descriptor-list-type parameter-descriptors))
         (pd-list 
           (if (eq pd-list-type 'required) 
               parameter-descriptors 
               (cadr parameter-descriptors))))
    (mapcar
     (lambda (p) (funcall p :name))
     pd-list)))

(defun parameter-descriptor-list-type (parameter-descriptors)
  (let ((maybe-xtra (first parameter-descriptors)))
    (if (typep maybe-xtra 'function)
        'required
        maybe-xtra)))

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

(defun list-head-if (l test)
  (labels 
      ((rec (l) 
         (if (funcall test (car l)) 
             (cons (car l) (rec (rest l))) 
           nil))) 
    (rec l)))

(defun parse-sub-lambda-list (sub-ll)
  (mapcar #'make-parameter-descriptor sub-ll))

(defun parse-extra-sub-lambda-list  (xtra)
  (let ((xtra-type (first xtra)))
    (list xtra-type (parse-sub-lambda-list (rest xtra)))))

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

(defun parse-lambda-list (ll)
  (let* ((split-ll (split-lambda-list ll))
         (required (first split-ll))
         (parsed-required (parse-sub-lambda-list required))
         (xtra (second split-ll))
         (parsed-xtra (parse-extra-sub-lambda-list xtra)))
    (list parsed-required  parsed-xtra)))

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
                                 (subseq s 0 (1- (length s)))
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
