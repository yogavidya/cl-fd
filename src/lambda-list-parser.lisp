(defpackage :safer-code/src/lambda-list-parser
  (:use :cl :safer-code/src/function-parameters)
  (:export :make-function-parameters-description))

(in-package :safer-code/src/lambda-list-parser)

(defun @add-implicit-T (arglist)
  "Convert all untyped lambda list entries X to T-typed entries (X T)"
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
          (setf (nth index arglist) (list current 'T))))))
  
(defun @extra-args-position (lambda-list &optional (start 0))
  (position-if
   #'(lambda(entry)
       (or
        (member entry '(&key &optional &rest))
        (and (listp entry)
             (or (eq (first entry) '&key)
                 (eq (first entry) '&optional)
                 (eq (first entry) '&rest)))))
   lambda-list :start start))

(defun @extract-extra-args(lambda-list position)
  (and position
       (if (atom (nth position lambda-list))
           (nthcdr position lambda-list)
         (first (last lambda-list)))))

(defun make-function-parameters-description ( lambda-list )
  ;; regole: obbligatori all'inizio
  ;; gli altri dopo inclusi in una lista
  ;; che inizia con &key, &rest o &optional
    
  (let*
      ((in (copy-list lambda-list))
       (index-extra (@extra-args-position in))
       (check (when (and index-extra (@extra-args-position in (1+ index-extra)))
                (error "lambda list is malformed")))
       (required-args (if index-extra (subseq in 0 index-extra)
                        in))
       (extra-args (@extract-extra-args lambda-list index-extra))
       (descriptor (make-instance 'function-parameters-description)))
    (declare (ignore check))
    (@add-implicit-T required-args)
    (@add-implicit-T extra-args)
    (flet
        ((fp-name (arg) (first arg))
         (fp-type (arg) (second arg))
         (fp-value-check (arg) 
           (if (consp (third arg))
               (symbol-function (first (third arg)))
             #'(lambda (a b)
                 (declare (ignore b))
                 (funcall (symbol-function (third arg)) a))))
         (fp-value-check-form (arg) 
           (when (consp (third arg))
             (second (third arg))))
         (fp-has-default-value (arg) (= (length arg) 4))
         (fp-default-value (arg) (fourth arg))
         (fp-default-value (arg) (fourth arg)))
      (dolist (arg required-args)
        (fpd-add 
         'required 
         descriptor				 
         (make-function-parameter
          :name (fp-name arg)
          :type (fp-type arg)
          :value-check-function (fp-value-check arg)
          :value-check-form (fp-value-check-form arg))))
      (when extra-args
        (dolist (arg (rest extra-args))
          (fpd-add 
           (first extra-args) 
           descriptor 
           ( make-function-parameter
             :name (fp-name arg)
             :type (fp-type arg)
             :value-check-function (fp-value-check arg)
             :value-check-form (fp-value-check-form arg)
             :default-value (fp-default-value arg))))))
    descriptor))
