(defsystem "safer-code"
  :description "safer-code: a tool for automated testing and error management"
  :version "0.99"
  :author "Salvatore Uras yogavidya@gmail.com"
  :licence "Public Domain"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("lisp-unit" "iterate" "safer-code/safer-code")
  :components ((:file "safer-code")))
