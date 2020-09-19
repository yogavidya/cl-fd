(defpackage :cl-fd/src/descriptors/parameter
  (:use :cl :cl-fd/src/conditions :cl-fd/src/types)
  (:export 
   :make-parameter-descriptor
   ))

(in-package :cl-fd/src/descriptors/parameter)


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
         (let ((vo (list "FOR" "PARAMETER" p-name '= (format nil "~s" request-arg))))
	   (if p-validated-structured-p
	     (push p-validate-fn-input vo)
             (push `(,p-validate-fn-input ,p-name) vo))
	   (and p-validate-fn-input (format nil "~{~a ~}" vo))))
        (otherwise 
	 (error "Unknown request to parameter descriptor: ~a ~a" request request-arg))))))