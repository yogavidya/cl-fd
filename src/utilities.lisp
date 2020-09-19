(defpackage :cl-fd/src/utilities
  (:use :cl :iterate)
  (:export :*scratch* :macroexpand-to-file :defun-inline))

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

