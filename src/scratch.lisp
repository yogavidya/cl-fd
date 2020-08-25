(defpackage :safer-code/src/scratch
  (:use :cl)
  (:export 
   :safe-defun :safe-function-success :safe-function-value :safe-function-return
   :safe-function-value-multiple :safe-function-extra-values))

(defpackage :safer-code-restarts)
(in-package :safer-code/src/scratch)

;;;
;;; CONDITIONS
;;;

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
;;; /CONDITIONS

;;;
;;; TYPES
;;;
(progn ; SAFER-CODE-RETURN
  (defun @valid(x)
    (and (not (null x)) (listp x) (>= (length x) 2))
    (typep (first x) 'boolean))
  (deftype safer-code-return ()
    `(satisfies safer-code/src/scratch::@valid)))

(progn ;FUNCTION-PARAMETER-GROUP
  (defun @valid-parameter-group (pg)
    (declare (type symbol pg))
    (let ((name (symbol-name pg)))
      (find name '("required" "&optional" "&key" "&rest") :test #'equalp)))
  (deftype function-parameter-group ()
    `(and  symbol (satisfies @valid-parameter-group))))
;;; /TYPES

;;;
;;; STRUCTURES, CLASSES AND METHODS
;;;
(defstruct function-parameter ; (&key (p number (evenp nil) 4)
  (name 'none :type symbol)
  (type 'none :type (or symbol list))
  (value-check-function nil :type (or null function))
  (value-check-form nil :type T)
  (default-value nil :type T))

(defclass function-parameters-description ()
  ((required :reader function-parameters-required :type list :initform (list))
   (keywords :reader function-parameters-keyword :type list :initform (list))
   (optional :reader function-parameters-optional :type list :initform (list))
   (rest :reader function-parameters-rest :type list :initform (list))))

(defmethod function-parameters-required :around ((desc function-parameters-description))
  (reverse (slot-value desc 'required)))

(defmethod function-parameters-add ((what symbol) (desc function-parameters-description) (fp function-parameter))
  (macrolet ((eq-group (group-sym)
               `(equalp (symbol-name what) (symbol-name ,group-sym))))
    (check-type what function-parameter-group)
    (cond
     ((eq-group 'required)
      (push fp (slot-value desc 'required)))
     ((eq-group '&optional)
      (push fp (slot-value desc 'optional)))
     ((eq-group '&key)
      (push fp (slot-value desc 'keywords)))
     ((eq-group '&rest)
      (push fp (slot-value desc 'rest)))
     (T
      (error "in function-parameters-add: bad parameter group ~s" what)))
    fp))

(defmethod function-parameters-get-parameter ((this function-parameters-description) (p symbol))
  (flet ((parameter-name= (fp)
           (eq p (function-parameter-name fp))))
    (or 
     (find-if #'parameter-name= (function-parameters-required this))
     (find-if #'parameter-name= (function-parameters-optional this))
     (find-if #'parameter-name= (function-parameters-keyword this))
     (find-if #'parameter-name= (function-parameters-rest this)))))

(defmethod function-parameters-defun-lambda-list ((this function-parameters-description))
  (let ((ll (list)))
    (dolist (p (function-parameters-required this))
      (push (function-parameter-name p) ll))
    (when (function-parameters-optional this)
      (push '&optional ll)
      (dolist (p (function-parameters-optional this))
        (push (function-parameter-name p) ll)))
    (when (function-parameters-keyword this)
      (push '&key ll)
      (dolist (p (function-parameters-keyword this))
        (push (list (function-parameter-name p) 
                    (function-parameter-default-value p)) 
              ll)))
    (when (function-parameters-rest this)
      (push '&rest ll)
      (dolist (p (function-parameters-rest this))
        (push (function-parameter-name p) ll)))
    (reverse ll)))

(defmethod function-parameter-check ((this function-parameters-description) (p symbol) (v T))
  (let* ((fp (function-parameters-get-parameter this p))
         (type-check (when fp (typep v (function-parameter-type fp))))
         (value-check 
          (cond
           ((and 
             fp 
             (function-parameter-value-check-function fp))
            (funcall 
             (function-parameter-value-check-function fp) 
             v (function-parameter-value-check-form fp)))
           (fp T)
           (T nil))))
    (format t "type check: ~a, value-check: ~a~%" type-check value-check)))
    

;;; /STRUCTURES, CLASSES AND METHODS

(flet
    ((add-implicit-T (arglist)
       (loop for index from 0 below (length arglist)
	     with current
	     do
		(setf current (nth index arglist))
		(cond
		  ((and (listp current) (> (length current) 4))
		   (error "lambda list entry ~a has length > 3" current))
		  ((and (listp current) (= (length current) 4))
		   (when (or (not (listp #1=(third current)))
			     (or
			      (not (functionp (ignore-errors (symbol-function (first #1#)))))))
		     (error
		      (format nil "in parameter ~a, specializer ~a should be (check-function form)"
			      current #1#))))
		  ((and (atom current) (not (find current '(&key &optional &rest))))
		   (setf (nth index arglist) (list current 'T)))))))
  
  (defun make-function-parameters-description ( lambda-list )
    ;; regole: obbligatori all'inizio
    ;; gli altri dopo inclusi in una lista
    ;; che inizia con &key, &rest o &optional
    
    (let*
	((in (copy-list lambda-list))
         (index-extra
	   (position-if
	    #'(lambda(entry)
		(and (listp entry)
		     (or (eq (first entry) '&key)
			 (eq (first entry) '&optional)
			 (eq (first entry) '&rest))))
	    in))
	 (check (when (and index-extra (> (length in) (1+  index-extra)))
		  (error "lambda list is malformed")))
	 (required-args (if index-extra (subseq in 0 index-extra)
			    in))
	 (extra-args (when index-extra (first (last in))))
	 (descriptor (make-instance 'function-parameters-description)))
      (declare (ignore check))
      (add-implicit-T required-args)
      (add-implicit-T extra-args)
      (flet
          ((fp-name (arg) (first arg))
           (fp-type (arg) (second arg))
           (fp-value-check (arg) 
             (if (consp (third arg))
                 (symbol-function (first (third arg)))
               #'(lambda (a b)
                   (declare (ignore b))
                   (funcall (symbol-function (third arg)) a))))
           (fp-has-default-value (arg) (= (length arg) 4))
           (fp-default-value (arg) (fourth arg))
           (fp-value-check-form (arg) 
             (when (consp (third arg))
                 (second (third arg))))
           (fp-default-value (arg) (fourth arg)))
        (dolist (arg required-args)
          (function-parameters-add 
           'required 
           descriptor				 
           (make-function-parameter
            :name (fp-name arg)
            :type (fp-type arg)
            :value-check-function (fp-value-check arg)
            :value-check-form (fp-value-check-form arg))))
        (when extra-args
          (dolist (arg (rest extra-args))
            (function-parameters-add 
             (first extra-args) 
             descriptor 
             ( make-function-parameter
               :name (fp-name arg)
               :type (fp-type arg)
               :value-check-function (fp-value-check arg)
               :value-check-form (fp-value-check-form arg)
               :default-value (fp-default-value arg))))))
      descriptor)))

  
(defmacro safe-defun (name lambda-list
                           &key documentation restarts function-body
                           (in-types nil) (out-type T))
  (let* ((function-descriptor
          (make-function-parameters-description lambda-list))
         (defun-lambda-list (function-parameters-defun-lambda-list function-descriptor)))
    `(progn
       (fmakunbound (quote ,name))
       (declaim (ftype (function ,in-types safer-code-return),name))
       (defun ,name ,defun-lambda-list ;( ,@lambda-list)
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
         (compile (quote ,name))
         (symbol-function (quote ,name)))))

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

#|
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

|#