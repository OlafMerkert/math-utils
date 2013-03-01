(defpackage :valuations
  (:nicknames :vv)
  (:use :cl :ol :iterate )
  (:import-from :polynomials  #:polynomial #:coefficients)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:export))

(in-package :valuations)

;;; todo figure out how to treat infinity and zero objects

;;; start off with exponential valuations for simplicity

(defgeneric valuate-exp (valuation object))

;;; for polynomials, we can just minimise the valuation on the
;;; coefficients
(defmethod valuate-exp (valuation (polynomial polynomial))
  (iter (for coeff in-vector (coefficients polynomial))
        (minimise (valuate-exp valuation coeff))))

;;; for power-series, we need to get a bit smarter -- unless the
;;; series is constant
(defmethod valuate-exp (valuation (constant-series constant-series))
  (valuate-exp valuation (constant-coefficient constant-series)))

;;; otherwise, we try to guess whether the power series is bounded
(defparameter bounded-count 10
  "The number of subsequent coefficients that need to be lower than
  the current bound before we hazard the guess that the series is
  bounded.")

(defparameter bounded-search-limit 50
  "The maximum number of coefficients we want to look at when
  searching for a bound of the coefficients of the power series.")

(defmethod valuate-exp (valuation (power-series power-series))
  ;; first consider the case of finite series
  (let ((coeff (coefficients power-series)))
    (aif (lazy-array-finite coeff)
         (iter (for c in-vector (lazy-array-take coeff it nil))
               (minimise (valuate-exp valuation c)))
         ;; in the other case, we have to guess
         (valuate-exp/power-series-infinite valuation coeff))))

(defun valuate-exp/power-series-infinite (valuation coeffs)
  (do* ((index 0 (+ 1 index))
        (val #2=(valuate-exp valuation (lazy-aref coeffs index)) #2#)
        (bound val)
        (bound-index 0))
       ;; terminate if the last coeffs did not require increasing the bound
       ((cond ((<= bounded-count (- index bound-index))
               (values bound bound-index)) ; todo take degree into account
              ;; or we reached the end of our curiosity
              ((> index bounded-search-limit)
               (values bound :unbounded))))
    (when (< val bound)
      (setf bound val
            bound-index index))))
