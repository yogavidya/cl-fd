(defpackage :safer-code/src/results
	(:use :cl)
	(:export :safe-function-success :safe-function-value-multiple
		:safe-function-value :safe-function-extra-values
		:safe-function-return))

(in-package :safer-code/src/results)

(declaim (inline safe-function-success))
(defun safe-function-success (result)
  (car result))

(declaim (inline safe-function-value-multiple))
(defun safe-function-value-multiple (result)
  (consp (second result)))

(declaim (inline safe-function-value))
(defun safe-function-value (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (first (second result))
    (second result)))

(declaim (inline safe-function-extra-values))
(defun safe-function-extra-values (result)
  (if (and (safe-function-success result)
           (safe-function-value-multiple result))
      (rest (second result))
    (error "No multiple values")))

(declaim (inline safe-function-return))
(defun safe-function-return (success result)
  (list success result))

