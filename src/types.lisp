(defpackage :safer-code/src/types
  (:use :cl)
  (:export :safer-code-return :function-parameter-group))


(in-package :safer-code/src/types)


(progn ; SAFER-CODE-RETURN
  (defun @safer-code-raturn-valid(x)
    (and (not (null x)) (listp x) (>= (length x) 2))
    (typep (first x) 'boolean))
  (deftype safer-code-return ()
    `(satisfies @safer-code-raturn-valid)))

(progn ;FUNCTION-PARAMETER-GROUP
  (defun @function-parameter-group-valid (pg)
    (declare (type symbol pg))
    (let ((name (symbol-name pg)))
      (find name '("required" "&optional" "&key" "&rest") :test #'equalp)))
  (deftype function-parameter-group ()
    `(and  symbol (satisfies @function-parameter-group-valid))))
