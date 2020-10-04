(defpackage :cl-fd/src/utilities
  (:use :cl :iterate)
  (:export :*scratch* 
   :macroexpand-to-file 
   :defun-inline 
   :append* 
   :maptree 
   :destructuring-maptree 
   :subst-markers))

(in-package :cl-fd/src/utilities)

(defvar *scratch* 
  (merge-pathnames "quicklisp/local-projects/scratch.lisp" (user-homedir-pathname)))

(defun macroexpand-to-file (exp &optional (path *scratch*)) 
  (with-open-file 
      (s path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (pprint (macroexpand-1 exp) s)))


(defmacro defun-inline (name typed-lambda-list return-type &body body) 
            `(progn
               (fmakunbound ',name)
               (declaim (ftype 
                         (function 
                          ,(iter (for a in typed-lambda-list) 
                             (if (atom a) (collect T) (collect (second a)))) 
                          ,return-type) 
                         ,name)
                        (inline ,name))
               (defun ,name 
                      ,(iter (for a in typed-lambda-list) 
                         (if (atom a) (collect a) (collect (first a)))) 
                 (declare (optimize (speed 3)))
                 ,@body)))

(defun append*  (to-list node &key destructured) 
  (cond
    ((and (null to-list) destructured)
     (if (atom node) (list node) node))
    ((null to-list) (list node))
    (T (rplacd 
	(nthcdr 
	 (1- (length to-list)) 
	 to-list) 
	(if (and destructured (listp node)) node (list node)))
       to-list)))

(defun maptree (l f) 
  (iter (for x in l) 
    (collect (if (atom x) (funcall f x) (maptree x f)))))

(defun destructuring-maptree (l f)
  (reduce 
   (lambda (acc x)
     (if (atom x)
         (let ((result (funcall f x)))
           (cond
	     ((eq result :destructuring-maptree-remove-node) acc)
	     ((atom result)
	      (append acc (list result)))
             (t (append acc result))))
       (append acc (list (destructuring-maptree x f)))))
   l
   :initial-value (list))) 


(defun subst-markers (subst-list &rest forms)
  (flet ((new-value (x marker value &key not-null)
           (if (and (symbolp x) (equalp (symbol-name x) (symbol-name marker)))
               (if not-null 
                   (or value :destructuring-maptree-remove-node) 
                 value)
             x)))
    (let ((result (copy-tree forms)))
      (iter (for (marker value destructured not-null) in subst-list)
          (setf result 
                (funcall (if destructured 'destructuring-maptree 'maptree) 
                         result 
                         (lambda (x) (new-value x marker value :not-null not-null)))))
      result)))


