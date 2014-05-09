(defpackage :heights
  (:use :cl :ol :iterate)
  (:export
   #:weyl-height
   #:weyl-height/log))

(in-package :heights)

(defgeneric weyl-height (number)
  (:documentation "The absolute Weyl height."))

(defun weyl-height/log (number)
  "The absolute logarithmic Weyl height."
  (log (weyl-height number)))

(defmethod weyl-height ((rational rational))
  (max (abs (numerator rational)) (abs (denominator rational))))

(defmethod weyl-height ((integer integer))
  (abs integer))

(defmethod weyl-height ((fraction fractions:fraction))
  (error "Height not yet implemented for general fractions."))

(defmethod weyl-height ((poly polynomials:polynomial))
  (reduce #'max (polynomials:coefficients poly) :key #'weyl-height))
