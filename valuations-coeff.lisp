(defpackage :valuations-coeff
  (:nicknames :vc)
  (:shadow #:valuate-exp #:val)
  (:use :cl :ol :iterate
        :infinite-math
        :valuations)
  (:import-from :polynomials  #:polynomial #:coefficients #:degree)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:export
   #:valuate-exp
   #:val
   #:polynomial-values
   #:power-series-values))

(in-package :valuations-coeff)

(defgeneric valuate-exp (valuation object))

(defun val (valuation)
  (lambda (x) (valuate-exp valuation x)))

;;; fall back to ordinary valuation in case of elementary expressions
;;; like rationals
(defmethod valuate-exp (valuation (rational rational))
  (vv:valuate-exp valuation rational))

;;; for polynomials and power series, we just map over the
;;; coefficients.
(defmethod valuate-exp (valuation (polynomial polynomial))
  (make-instance 'polynomial-values
                 :coefficients (map 'vector (val valuation)
                                    (coefficients polynomial))))

(defmethod valuate-exp (valuation (power-series power-series))
  (make-instance 'power-series-values
                 :degree (degree power-series)
                 :coefficients (lazy-array-map (val valuation)
                                               (coefficients power-series)
                                               infinity+)))

(defclass polynomial-values (polynomial)
  ())

(defclass power-series-values (power-series)
  ())


(defmethod print-object ((polynomial-values polynomial-values) stream)
  (let ((pspr:*current-printer* 'pspr:string-printer))
    (princ "V[" stream)
    (pspr:format-polynomial/all polynomial-values)
    (princ "]" stream)))

(defmethod print-object ((power-series-values power-series-values) stream)
  (let ((pspr:*current-printer* 'pspr:string-printer))
    (princ "V[" stream)
    (pspr:format-power-series/all power-series-values)
    (princ "]" stream)))

(defmethod gm:simplify ((polynomial-values polynomial-values) &key)
  (with-slots (coefficients) polynomial-values
    (when (length=0 coefficients)
      (setf coefficients (vector infinity+))))
  polynomial-values)

(defmethod gm:simplify ((power-series-values power-series-values) &key)
  power-series-values)

;;; todo improve display of this stuff
