(defpackage :valuations
  (:nicknames :vv)
  (:use :cl :ol :iterate )
  (:import-from :polynomials  #:polynomial #:coefficients #:degree)
  (:import-from :power-series #:power-series #:constant-series #:constant-coefficient)
  (:export
   #:valuate-exp
   #:bounded-search-limit
   #:bounded-count
   #:v-minimise
   #:v<
   #:infinity-p
   #:+infinity+))

(in-package :valuations)

;;; todo figure out how to treat infinity and zero objects

;;; start off with exponential valuations for simplicity
(defconstant +infinity+ :infinity)

(declaim (inline infinity-p))
(defun infinity-p (x)
  (eq x +infinity+))

(defun v< (a b)
  (cond ((infinity-p a) nil)
        ((infinity-p b) t)
        ((cl:< a b))))

(defun v-minimise (values &optional key)
  (reduce (lambda (a b) (if (v< b a) b a))
          values :key key))

(declaim (inline val))
(defun val (valuation)
  (lambda (x) (valuate-exp valuation x)))

(defgeneric valuate-exp (valuation object))

;;; for polynomials, we can just minimise the valuation on the
;;; coefficients
(defmethod valuate-exp (valuation (polynomial polynomial))
  (v-minimise (coefficients polynomial)
              (val valuation)))

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
         (values (v-minimise (lazy-array-take coeff it nil)
                             (val valuation))
                 :finite) 
         ;; in the other case, we have to guess
         (valuate-exp/power-series-infinite valuation coeff (degree power-series)))))

(defun valuate-exp/power-series-infinite (valuation coeffs &optional (degree 0))
  (do* ((index 0 (+ 1 index))
        (val #2=(valuate-exp valuation (lazy-aref coeffs index)) #2#)
        (bound val)
        (bound-index 0))
       ;; terminate if the last coeffs did not require increasing the bound
       ((or #1=(<= bounded-count (- index bound-index))
            ;; or we reached the end of our curiosity
            (> index bounded-search-limit))
        
        (values bound (if #1# bound-index :unbounded)))
    (when (v< val bound)
      (setf bound val
            bound-index index))))

;;; p-adic valuations on rationals
(defmethod valuate-exp ((p integer) (rational rational))
  ;; todo check that p is prime
  (if (zerop rational)
      +infinity+
      (nt:ord-p p rational)))

