(defpackage :safer-code/src/function-parameters
  (:use :cl :safer-code/src/conditions :safer-code/src/types)
  (:export 
   :function-parameter :make-function-parameter
   :function-parameters-description
   :function-parameters-required
   :function-parameters-keyword
   :function-parameters-optional
   :function-parameters-rest
   :function-parameters-add
   :function-parameters-get-parameter
   :function-parameters-defun-lambda-list
   :function-parameter-check))

(in-package :safer-code/src/function-parameters)


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
