(defpackage :safer-code/src/function-parameters
  (:use :cl :safer-code/src/conditions :safer-code/src/types)
  (:export 
   :function-parameter 
   :make-function-parameter 
   :fp-name 
   :fp-type 
   :fp-value-check-function 
   :fp-value-check-form 
   :fp-default-value
   :function-parameters-description
   :fpd-required
   :fpd-keyword
   :fpd-optional
   :fpd-rest
   :fpd-add
   :fpd-get
   :fpd-type-list
   :fpd-lambda-list
   :fpd-flat-lambda-list
   :function-parameter-check))

(in-package :safer-code/src/function-parameters)


(defstruct (function-parameter (:conc-name fp-))
  (name 'none :type symbol :read-only T)
  (type 'T :type (or symbol list) :read-only T)
  (value-check-function nil :type (or null function) :read-only T)
  (value-check-form nil :type T :read-only T)
  (default-value nil :type T) :read-only T)

(defclass function-parameters-description ()
  ((required :reader fpd-required :type list :initform (list))
   (keywords :reader fpd-keyword :type list :initform (list))
   (optional :reader fpd-optional :type list :initform (list))
   (rest :reader fpd-rest :type list :initform (list))))

(defmethod fpd-required :around ((desc function-parameters-description))
  (reverse (slot-value desc 'required)))

(defmethod fpd-add ((what symbol) (desc function-parameters-description) (fp function-parameter))
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
      (error "in fpd-add: bad parameter group ~s" what)))
    fp))

(defmethod fpd-get ((this function-parameters-description) (p symbol))
  (flet ((parameter-name= (fp)
           (eq p (fp-name fp))))
    (or 
     (find-if #'parameter-name= (fpd-required this))
     (find-if #'parameter-name= (fpd-optional this))
     (find-if #'parameter-name= (fpd-keyword this))
     (find-if #'parameter-name= (fpd-rest this)))))

(defmethod fpd-lambda-list ((this function-parameters-description))
  (let ((ll (list)))
    (dolist (p (fpd-required this))
      (push (fp-name p) ll))
    (when (fpd-optional this)
      (push '&optional ll)
      (dolist (p (fpd-optional this))
        (push (fp-name p) ll)))
    (when (fpd-keyword this)
      (push '&key ll)
      (dolist (p (fpd-keyword this))
        (push (list (fp-name p) 
                    (fp-default-value p)) 
              ll)))
    (when (fpd-rest this)
      (push '&rest ll)
      (dolist (p (fpd-rest this))
        (push (fp-name p) ll)))
    (reverse ll)))

(defmethod fpd-flat-lambda-list ((this function-parameters-description))
  "used internally by safe-defun"
  (mapcar
   #'(lambda (x) (if (consp x) (first x) x))
   (delete-if 
    #'(lambda(token)
        (member token '(&key &optional &rest)))
    (fpd-lambda-list this))))


(defmethod fpd-type-list ((this function-parameters-description))
  (let ((ll (fpd-lambda-list this))
        (result (list)))
    (dolist (p ll)
      (cond
       ((member p '(&key &optional &rest))
        nil) ; ignore
       ((atom p)
        (push 
         (fp-type 
          (fpd-get this p))
         result))
       ((consp p)
        (push 
         (fp-type 
          (fpd-get this (first p)))
         result ))
       (t (error 
           "fpd-type-list: don't know what to do of ~a" p))))
    (reverse result)))

(defmethod function-parameter-check ((this function-parameters-description) (p symbol) (v T))
  (let* ((fp (fpd-get this p))
         (type-check (when fp (typep v (fp-type fp))))
         (value-check 
          (cond
           ((and 
             fp 
             (fp-value-check-function fp))
            (funcall 
             (fp-value-check-function fp) 
             v (fp-value-check-form fp)))
           (fp T)
           (T nil))))
    (list 
     #1=(and type-check value-check)
     (when (null #1#)
       (format nil 
               "~a type check: ~a; value-check: ~a"
               (fp-name fp)
               (if (null type-check)
                   (format nil
                           "expected: ~a, found: ~a"
                           (fp-type fp) v)
                 "OK")
               (if (null value-check)
                   (format nil
                           "(~a ~a ~a) not satisfied by value ~a."
                           (fp-value-check-function fp) 
                           v
                           (eval (fp-value-check-form fp))
                           v)                 
                 "OK"))))))
