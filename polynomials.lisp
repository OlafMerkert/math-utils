(defpackage :polynomials
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :generic-math)
  (:export
   :degree
   :nth-coefficient%
   :nth-coefficient
   :polynomial
   :coefficients))

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
(defmethod degree ((polynomial polynomial))
  (1- (length (coefficients polynomial))))

(defmethod nth-coefficient% ((polynomial polynomial) n)
  (aref (coefficients polynomial) n))

(defmethod nth-coefficient ((polynomial polynomial) n)
  (let ((d (degree polynomial)))
    (nth-coefficient% polynomial (- d n))))

(defun constant-coefficient (polynomial)
  #|(unless (zerop (degree series))
    (error "CONSTANT-COEFFICIENT only works with 0 DEGREE."))|#
  (nth-coefficient% polynomial 0))

(defmethod simplified-p ((polynomial polynomial))
  (or (zerop (degree polynomial))
      (not (zero-p
            (nth-coefficient% polynomial 0)))))

(defun make-polynomial (lk &rest coefficients)
  (make-instance 'polynomial :coefficients (list->array (list* lk coefficients))))

(defmethod zero ((number polynomial))
  (make-polynomial 0))

(defmethod zero ((number (eql 'polynomial)))
  (make-polynomial 0))

(defmethod one ((number polynomial))
  (make-polynomial 1))

(defmethod one ((number (eql 'polynomial)))
  (make-polynomial 1))

(defmethod simplify ((polynomial polynomial) &key)
  "Remove all leading zeros from the coefficients.  If all
  coefficients are zero, keep the last zero."
  (with-slots (coefficients) polynomial
   (let ((nz (position-if-not #'zero-p coefficients :end (1- (length coefficients)))))
     (setf coefficients
           (subseq coefficients nz))
     (values polynomial nz))))
