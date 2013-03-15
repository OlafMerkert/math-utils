(defpackage :math-variables
  (:nicknames :vars)
  (:use :cl :ol :iterate )
  (:export
   #:var<
   #:var=))

(in-package :math-variables)

;;; some helper functions that deal with symbolic variables and
;;; canonical ordering on them

(defun var= (v w)
  (eql v w))

(defun var< (v w)
  (string< (symbol-name v) (symbol-name w)))

