(defpackage :safer-code
  (:nicknames :sc)
  (:use :cl)
  (:use :safer-code/src/all :safer-code/src/scratch)
  (:export
   :safe-defun :safe-function-success :safe-function-return
   :safe-function-value :safe-function-value-multiple
   :safe-function-extra-values))
