(defpackage :power-series
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :generic-math))

(in-package :power-series)

(defclass power-series ()
  ((degree :initform 0
           :initarg :degree
           :reader degree)
   (coefficients :initform (la% 0 1)
                 :initarg :coefficients
                 :reader coefficients)
   #|(var :initform 'x
        :accessor var)|#)
  (:documentation "Model a laurent series in VAR^-1 with the first
  coefficient being for VAR^DEGREE."))

(defun series-normalised-p (series)
  "Test whether the first coefficient is indeed not 0, so the degree is
  meaningful."
  (not (gm:= 0 (lazy-aref (coefficients series) 0))))

(defun nth-coefficient% (series n)
  "Return the nth element of the coefficients pipe--or zero, if the
pipe ends before."
  (unless (>= n 0)
    (error "Non negative index ~A in NTH-COEFFICIENT%." n))
  (lazy-aref (coefficients series) n))

(defun nth-coefficient (series n)
  "Return the coefficient of X^n"
  (if (<= n (degree series))
      (nth-coefficient% series (- (degree series) n))
      0))

