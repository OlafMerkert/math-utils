(defpackage :multivariate-polynomials
  (:nicknames :mpoly)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :iterate
        :generic-math
        :math-variables
        :polynomials)
  (:export))

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
(defun mdegree (mp var)
  (mpoly-cases (mp var)
               0
               (1- (length (coefficients mp)))
               (maximise (coefficients mp) :key (lambda (x) (mdegree x var)))))

(defun poly+constant (poly constant)
  (make-instance 'mpolynomial :var (var poly)
                 :coefficients (aprog1 (copy-seq (coefficients poly))
                                 (setf (alast it) (gm:+ (alast it) constant)))))

(defmethod generic-+ ((a mpolynomial) (b mpolynomial))
  (mpoly-cases (a (var b))
               (poly+constant a b)
               (poly+poly a b 'mpolynomial)
               (poly+constant b a)))

