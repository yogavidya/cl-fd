(defpackage :cl-fd/src/types
  (:use :cl :cl-fd/src/utilities)
  (:export :fd-return :mismatched-argument :invalid-argument
   :mismatched-argument-name :mismatched-argument-type :mismatched-argument-value
   :invalid-argument-name :invalid-argument-op :invalid-argument-value))


(in-package :cl-fd/src/types)

;; TYPE: FD-RETURN
;; The format for return value in functions which are 
;; a) described by a function-descriptor, and 
;; b) defined by fd-instantiate.
;; It's a list R of two elements: (FIRST R) is a boolean
;; value, meaning the function executed normally (T)
;; or it had to handle a condition (NIL);
;; (SECOND R) is the actual returned value if
;; (FIRST R) is true, or an object with subtype 
;; CONDITION if (FIRST R) is false.
(defun-inline fd-return-valid (x) boolean
  (and (not (null x)) (listp x) (>= (length x) 2))
  (typep (first x) 'boolean))
(deftype fd-return ()
  `(satisfies fd-return-valid))


;; TYPE: MISMATCHED-ARGUMENT
;; Used internally by fd-instantiate as a descriptor
;; of a mismatched runtime argument to a function.
;; It's a list M of three elements: (FIRST M)
;; is the symbol of the formal parameter in the 
;; lambda-list as stored in the function-descriptor;
;; (SECOND M) is a symbol referring to a type for
;; said formal parameter; (THIRD M) is the value
;; found for the actual parameter, which should not be
;; a subtype of (SECOND M).

; helpers
(defun-inline mismatched-argument-name ((e cons)) T
  (first e))
(defun-inline mismatched-argument-type ((e cons)) T
  (second e))
(defun-inline mismatched-argument-value ((e cons)) T
  (third e))

; type validator
(defun-inline check-mismatched-argument (e) boolean
  (and (listp e) 
   (= (length e) 3)
   (symbolp (mismatched-argument-name e))
   (symbolp #1=(mismatched-argument-type e))
   (not 
    (subtypep 
     (type-of (mismatched-argument-value e)) 
     #1#))))

(deftype mismatched-argument ()
  `(and
    (cons)
    (satisfies check-mismatched-argument)))



;; TYPE: INVALID-ARGUMENT
;; Used internally by fd-instantiate as a descriptor
;; of an invalid runtime argument to a function.
;; It's a list I of three elements: (FIRST I)
;; is the symbol of the formal parameter in the 
;; lambda-list as stored in the function-descriptor;
;; (SECOND I) is a boolean function of one argument
;; used to check actual parameter; (THIRD I) is the value
;; found for the actual parameter.

; helpers
(defun-inline invalid-argument-name ((e cons)) symbol
  (first e))
(defun-inline invalid-argument-op ((e cons)) T
  (second e))
(defun-inline invalid-argument-value ((e cons)) T
  (third e))

; type validator
(defun-inline check-invalid-argument (e) T
  (and (consp 3) 
   (= (length e) 3)
   (symbolp (invalid-argument-name e))
   (consp #1=(invalid-argument-op e))
   (member (length #1#) '(2 3) :test '=)
   (not (null (first #1#)))))

(deftype invalid-argument ()
  `(and
    (cons)
    (satisfies check-invalid-argument)))

