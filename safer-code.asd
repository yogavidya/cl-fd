(defsystem "safer-code"
  :description "safer-code: a tool for automated testing and error management"
  :version "0.99"
  :author "Salvatore Uras yogavidya@gmail.com"
  :licence "Public Domain"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("iterate" 
               "safer-code/src/utilities"
               "safer-code/src/types"
               "safer-code/src/conditions"
               "safer-code/src/results"
               "safer-code/src/function-parameters"
               "safer-code/src/lambda-list-parser"
               "safer-code/src/safe-defun")
  :in-order-to ((test-op (test-op "safer-code/test")))
  :components ((:file "safer-code")))

(defsystem "safer-code/test"
  :description "safer-code/src/test: test service for safer-code"
  :version "0.99"
  :author "Salvatore Uras yogavidya@gmail.com"
  :licence "Public Domain"
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("lisp-unit" "iterate" "safer-code")
  :components ((:module "test"
                :serial T
                :components ((:file "package")
                             (:file "test" :depends-on ("package") 
                              ))))
  :perform (test-op  (o s)
                     (if (find-package :safer-code/test)
                         (symbol-call :safer-code/test :test)
                       (error "Can't find test package"))))
