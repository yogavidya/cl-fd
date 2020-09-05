(defpackage :safer-code
  (:nicknames :sc)
  (:use :cl
   :safer-code/src/utilities :safer-code/src/results :safer-code/src/safe-defun)
  (:export
   :*scratch* :macroexpand-to-file
   :safe-defun :safe-function-success :safe-function-return
   :safe-function-value :safe-function-value-multiple
   :safe-function-extra-values))
