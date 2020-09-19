(defsystem "cl-fd"
  :description "cl-fd: a tool for automated testing and error management"
  :version "0.99"
  :author "Salvatore Uras yogavidya@gmail.com"
  :licence "Public Domain"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("alexandria"
               "iterate" 
               "cl-fd/src/utilities"
               "cl-fd/src/types"
               "cl-fd/src/conditions"
               "cl-fd/src/results"
               "cl-fd/src/descriptors/parameter"
               "cl-fd/src/descriptors/function"
               "cl-fd/src/instantiator")
  :in-order-to ((test-op (test-op "cl-fd/test")))
  :components ((:file "cl-fd")))

(defsystem "cl-fd/test"
  :description "cl-fd/src/test: test service for cl-fd"
  :version "0.99"
  :author "Salvatore Uras yogavidya@gmail.com"
  :licence "Public Domain"
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("lisp-unit" "iterate" "cl-fd")
  :components ((:module "test"
                :serial T
                :components ((:file "package")
                             (:file "test" :depends-on ("package") 
                              ))))
  :perform (test-op  (o s)
                     (if (find-package :cl-fd/test)
                         (symbol-call :cl-fd/test :test)
                       (error "Can't find test package"))))
