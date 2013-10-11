(defpackage :multivariate-polynomials
  (:nicknames :mpoly)
  (:shadowing-import-from :fractions :numerator :denominator)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :ol :^ :_)
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
   #:mdegree
   #:make-mpolynomial
   #:make-monomial+))

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

(defmethod generic-/ ((poly-numer mpolynomial) (poly-denom mpolynomial))
  ;; TODO test for divisibility
  (fractions:frac poly-numer poly-denom))

;;; rational functions built from polynomials

;;; in theory, this construction should be completely generic.

(defmethod mdegree ((fraction fraction) var)
   (- (mdegree (numerator fraction) var) (mdegree (denominator fraction) var)))

(defmethod ggt ((a mpolynomial) (b mpolynomial))
  ;; TODO implement ggt for polynomials properly
  1)

(defun make-mpolynomial (var lk &rest coefficients)
  (if coefficients
      ;; TODO consider checking for proper ordering of variables
      (let ((poly (make-instance 'mpolynomial :var var
                                 :coefficients (list->array (list* lk coefficients)))))
        ;; TODO do we need different downgrading for mpolys??
        (simplify-poly poly nil)
        poly)
      ;; if only one coefficient is given, don't bother so much
      lk))

(defun make-monomial+ (vars degrees coefficient)
  (make-monomial-helper coefficient
                        (sort (mapcar #'cons vars degrees)
                              #'var< :key #'car)))

(defun make-monomial-helper (coefficient vars+degs)
  (if (null vars+degs)
      coefficient
      (make-monomial-helper
       (make-monomial (cdr vars+degs) coefficient (car vars+degs) 'mpolynomial)
       (rest vars+degs))))
