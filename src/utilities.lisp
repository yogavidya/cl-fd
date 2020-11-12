(defpackage :cl-fd/src/utilities
  (:use :cl :iterate)
  (:export :*scratch* 
   :macroexpand-to-file 
   :symbol-name=
   :defun-inline 
   :append* 
   :maptree 
   :destructuring-maptree 
   :subst-markers
   :*subst-readtable*))

(in-package :cl-fd/src/utilities)

(defvar *scratch* 
  (merge-pathnames "quicklisp/local-projects/scratch.lisp" (user-homedir-pathname)))

(defun macroexpand-to-file (exp &optional (path *scratch*)) 
  (with-open-file 
      (s path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (pprint (macroexpand-1 exp) s)))


(declaim (ftype (function (symbol symbol) boolean) symbol-name=))
(declaim (inline symbol-name=))
(defun symbol-name= (s1 s2)
              (declare (optimize speed))
              (equalp (symbol-name s1) (symbol-name s2)))

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
           (if (and (symbolp x) (symbol-name= x marker))
               (if not-null 
                   (or value :destructuring-maptree-remove-node) 
                 value)
             x)))
    (let ((result (copy-tree forms)))
      (iter (for (marker value destructured not-null) in subst-list)
        (and not-null (not destructured) 
             (error "subst-markers: marker ~a has not-null=T but destructured=NIL" marker))
          (setf result 
                (funcall (if destructured 'destructuring-maptree 'maptree) 
                         result 
                         (lambda (x) (new-value x marker value :not-null not-null)))))
      result)))


(defparameter *backup-readtable* (copy-readtable))
(defparameter *subst-readtable* (copy-readtable))
(setf *readtable* *subst-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character 
   #\# #\[ 
   #'(lambda (stream sub-char infix)
       (declare (ignorable sub-char infix))
       (let* ((first-char (peek-char t stream nil nil nil))
              (is-destructured (char-equal #\@ first-char))
              (step (when is-destructured (read-char stream)))
              (first-char (and is-destructured (peek-char t stream nil nil nil)))
              (is-nullable (and is-destructured (char-equal #\* first-char)))
              (step (when is-nullable (read-char stream)))         
              (l (read-delimited-list #\] stream nil)))
         (declare (ignorable step))
         (cond
          (is-nullable 
           (append  #1=(append (list 'list) l) (list t t)))
          (is-destructured 
           (append  #1# (list t)))
          (T #1#))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character 
   #\# #\{ 
   #'(lambda (stream sub-char infix) 
    (declare (ignorable sub-char infix))
    (let* 
	((l (append (list 'list) (read-delimited-list #\} stream t)))
	 (subst-form  `( ,@(car (last l))))
	 (subst-list (nbutlast (copy-tree l))))
      (list 'eval (list 'car (list 'cl-fd/src/utilities:subst-markers 
				   subst-list
				   subst-form)))))))
  


(defun %defun-inline (name typed-lambda-list return-type &rest body) 
  (labels 
      ((extra-parameters-type (l &key result)
         (cond
          ((null l) 
           result)
          ((consp (car l))
           (extra-parameters-type (rest l) :result result))
          ((member (car l) '(&optional &key &rest))
           (if result
               (error "Only one of &OPTIONAL, &KEY and &REST can appear in lambda list.")
             (extra-parameters-type (rest l) :result (car l))))
           (T
            (extra-parameters-type (rest l) :result result))))
       (parameters-list (l &key (mode :required) (acc (list)))
         (when (null l) (return-from parameters-list (reverse acc)))
         (cond
          ((and (symbolp #1=(car l))
                (member (symbol-name #1#) 
                        '("&KEY" "&OPTIONAL") :test 'equalp))
           #2=(when (not (eq mode :required)) 
                (error		   
                 "defun-inline: mixing &key, &optional and &rest not allowed."))
           (parameters-list (rest l) :mode :extra :acc acc))
          ((and (symbolp #1#)
                (symbol-name= #1# '&REST))
           #2#
           (parameters-list (rest l) :mode :rest :acc acc))
          (t
           (case mode
             (:required 
              (if (atom #1#) 
                  (push (list mode #1# 't) acc)
                (push (list mode (first #1#) (second #1#)) acc))
              (parameters-list (rest l) :mode mode :acc acc))
             (:extra
              (if (atom #1#)
                  (push (list mode #1# 't) acc)
                (push (list mode (first #1#) (second #1#) (third #1#)) acc))
              (parameters-list (rest l) :mode mode :acc acc))
             (:rest 
              (push (list mode #1#) acc)
              (assert (null (rest l)))
              (reverse acc))))))
       (declare-parameters-list (l)
         (let* ((extra-type (extra-parameters-type l))
               (result
                (remove nil
                        (mapcar
                         (lambda (entry)
                           (when (symbol-name= (car entry) 'REQUIRED)
                             (second (rest entry))))
                         (parameters-list l)))))
           (if (null extra-type) result
             (append result (list extra-type)
                     (remove nil
                             (mapcar
                              (lambda (entry)
                                (when (symbol-name= (car entry) 'EXTRA)
                                  (cond
                                   ((symbol-name= extra-type '&key)
                                    (list 
                                     (intern  
                                      (symbol-name
                                       (first (rest entry)))
                                      (find-package :keyword))
                                     (second (rest entry)))
                                    )
                                   ((symbol-name= extra-type '&optional)
                                    (second (rest entry)))
                                   (T nil))))
                              (parameters-list l)))))))
       (function-lambda-list (l)
         (iter (for a in l) 
           (cond 
            ((atom a) (collect a))
            ((= 3 (length a))(collect (list (first a) (third a))))
            ((= 2 (length a))(collect (first a)))
            (t (error "don't know what to make of parameter ~a" a))))))
    #{
    #[ '%name name ]
    #[ '%return-type return-type ]
    #[ '%declare-parameters-list 
    (declare-parameters-list typed-lambda-list) ]
    #[ '%function-lambda-list
    (function-lambda-list typed-lambda-list) ]
    #[@ '%body body ]
    '(progn 
       (fmakunbound '%name)
       (declaim (ftype 
		   (function %declare-parameters-list
		    %return-type) 
		   %name)
		  (inline %name))
       (defun %name %function-lambda-list
         (declare (optimize speed))
         %body)) 
    } ))

(defmacro defun-inline (name typed-lambda-list return-type &body body)
  `(%defun-inline 
    (quote ,name) 
    ,(when typed-lambda-list
       `(quote ,typed-lambda-list))
    (quote ,return-type)
    ,@(iter (for f in body)
        (collect `(quote ,f)))))

(setf *readtable* *backup-readtable*)

