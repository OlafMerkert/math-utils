(defpackage :polynomials
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :generic-math))

(in-package :polynomials)

(defclass polynomial ()
  ((coefficients :initform (vector 0)
                 :initarg :coefficients
                 :reader coefficients)
   #|(var :initform 'x
   :accessor var)|#)
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS."))

;; TODO unify polynomial interface with power series interface
