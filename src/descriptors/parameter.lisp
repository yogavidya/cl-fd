(defpackage :cl-fd/src/descriptors/parameter
  (:use :cl :cl-fd/src/conditions :cl-fd/src/types)
  (:export 
   :make-parameter-descriptor
   ))

(in-package :cl-fd/src/descriptors/parameter)


(defun make-parameter-descriptor (p &key type )
;; SYM | (SYM [TYPE] [CHECK-FN | (CHECK-FN CHECK-FORM)] [DEFAULT])
  (let* ((check 
          (when 
              (not
               (or 
                (and (atom p) (symbolp p))
                (and (listp p) (symbolp (car p)))))
            (error "parameter description must be a symbol or a list starting with a symbol")))
         (p-structured (not (atom p)))
         (p-len (if p-structured (length p) 1))
         (p-max-allowed-length 
          (cond
            ((null type) 3) ; symbol type check 
            ((eq type '&rest) 3) ; same
            (T 4))) ; symbol type check default
         (p-name (if p-structured (first p) p))
         (p-fields '(symbol type check-function default))
         (check (when (and p-structured (> (length p) p-max-allowed-length))
                  (error "description for parameter ~a longer than ~a" p-name (subseq p-fields 0 p-max-allowed-length))))
         (p-typed-p (>= p-len 2))
         (p-validated-p (and (>= p-len 3) (third p)))
         (p-validated-structured-p (and p-validated-p (consp (third p))))
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
         (p-default 
          (and p-structured
               (if (and 
                    (or (null type) (eq type '&rest))
                    (> (length p) p-max-allowed-length))
                   (error "Default value not admitted for ~a parameter" p-name) 
                        (fourth p)))))
    (declare (ignore check))
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
         (let ((vo (list "FOR" "PARAMETER" p-name '= (format nil "~s" request-arg))))
	   (if p-validated-structured-p
	     (push p-validate-fn-input vo)
             (push `(,p-validate-fn-input ,p-name) vo))
	   (and p-validate-fn-input (format nil "~{~a ~}" vo))))
        (otherwise 
	 (error "Unknown request to parameter descriptor: ~a ~a" request request-arg))))))
