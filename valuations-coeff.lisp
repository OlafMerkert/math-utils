(defpackage :valuations-coeff
  (:nicknames :vc)
  (:shadow #:valuate-exp #:val)
  (:use :cl :ol :iterate
        :valuations)
  (:import-from :polynomials  #:polynomial #:coefficients #:degree)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:export
   #:valuate-exp
   #:val))

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
  (make-instance 'polynomial
                 :coefficients (map 'vector (lambda (x) (valuate-exp valuation x))
                                    (coefficients polynomial))))

(defmethod valuate-exp (valuation (power-series power-series))
  (make-instance 'power-series
                 :degree (degree power-series)
                 :coefficients (make-lazy-array
                                   (:finite (lazy-array-finite (coefficients power-series))
                                            :default-value +infinity+
                                            :index-var i)
                                 (valuate-exp valuation (lazy-aref (coefficients power-series) i)))))
