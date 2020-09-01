(defpackage :safer-code/src/utilities
  (:use :cl)
  (:export :*scratch* :macroexpand-to-file))

(in-package :safer-code/src/utilities)

(defvar *scratch* 
  (merge-pathnames "quicklisp/local-projects/scratch.lisp" (user-homedir-pathname)))

(defun macroexpand-to-file (exp &optional (path *scratch*)) 
  (with-open-file 
      (s path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (pprint (macroexpand-1 exp) s)))


