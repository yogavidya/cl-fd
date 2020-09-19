(defpackage :cl-fd
  (:use :cl :iterate
   :cl-fd/src/utilities :cl-fd/src/results :cl-fd/src/descriptors/function :cl-fd/src/instantiator)
  (:export
   :*scratch* :macroexpand-to-file
   :make-function-descriptor :fd-instantiate
   :fd-function-success 
   :fd-function-value-multiple 
   :fd-function-value
   :fd-function-extra-values
   :fd-function-explain))
