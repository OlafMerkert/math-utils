(defpackage :multivariate-polynomials
  (:nicknames :mpoly)
  (:shadowing-import-from :fractions :numerator :denominator)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :iterate
        :generic-math
        :fractions
        :math-variables
        :polynomials)
  (:export
   #:mpolynomial
   #:mpoly-p
   #:mpoly-cases
   #:mdegree))

(in-package :multivariate-polynomials)

(defclass mpolynomial (polynomial)
  ()
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS. Here coefficients only
  contains variables of lower order."))

(defun mpoly-p (mp)
  (typep mp 'mpolynomial))

(defmacro! mpoly-cases ((o!mpoly o!var) > = < &optional (not-mpoly >))
  "A convenience macro for working with multivariate polynomials,
respecting ordering of variables.

< means coefficients may contain expressions in VAR, = means we have a
polynomial in VAR, and > means coefficients cannot contain VAR, which
is also the default for anything that is not a multivariate
polynomial."
  `(let ((,g!var% (var ,g!mpoly)))
    (cond ((not (mpoly-p ,g!mpoly)) ,not-mpoly)
          ((var= ,g!var% ,g!var) ,=)
          ((var< ,g!var% ,g!var) ,<)
          (t ,>))))


;; TODO what about degree of zero poly?
(defmethod mdegree ((mp mpolynomial) var)
  (mpoly-cases (mp var)
               0
               (1- (length (coefficients mp)))
               (maximise (coefficients mp) :key (lambda (x) (mdegree x var)))))

(defmethod generic-+ ((a mpolynomial) (b mpolynomial))
  (mpoly-cases (a (var b))
               (poly+constant a b 'mpolynomial)
               (poly+poly a b 'mpolynomial)
               (poly+constant b a 'mpolynomial)))



(defmethod generic-* ((a mpolynomial) (b mpolynomial))
  (mpoly-cases (a (var b))
               (poly*constant a b 'mpolynomial)
               (poly*poly a b 'mpolynomial)
               (poly*constant b a 'mpolynomial)))

(defmethod generic-* ((poly-b mpolynomial) (number rational))
  (generic-* number poly-b))

(defmethod generic-* ((number rational) (poly-b mpolynomial))
  (poly*constant poly-b number 'mpolynomial))

(defmethod generic-/ ((poly mpolynomial) (number rational))
  (generic-* (/ number) poly))

;;; rational functions built from polynomials

;;; in theory, this construction should be completely generic.

(defmethod mdegree ((fraction fraction) var)
   (- (mdegree (numerator fraction) var) (mdegree (denominator fraction) var)))

(defmethod ggt ((a mpolynomial) (b mpolynomial))
  ;; TODO implement ggt for polynomials properly
  1)
