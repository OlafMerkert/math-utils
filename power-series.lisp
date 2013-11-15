(defpackage :power-series
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :ol :^ :_)
  (:import-from :generic-math :gm-summing)
  (:use :cl :ol :generic-math
        :iterate
        :polynomials
        :infinite-math
        :infinite-sequence)
  (:export
   #:confidence
   #:nth-coefficient%
   #:nth-coefficient
   #:default-series-simplification-depth
   #:series-truncate
   #:series-remainder
   #:power-series
   #:constant-series
   #:make-constant-series
   #:constant-coefficient
   #:degree
   #:coefficients
   #:make-power-series
   #:make-power-series/inf
   #:leading-coefficient))

(in-package :power-series)

(declaim (inline finite-coefficients))
(defun finite-coefficients (&rest coefficients)
  (finseq (apply #'vector coefficients) 0))


(defclass power-series (generic-math-object)
  ((degree :initform 0
           :initarg :degree
           :reader degree)
   (coefficients :initform (finite-coefficients)
                 :initarg :coefficients
                 :reader coefficients)
   (var :initform 'x
        :accessor var))
  (:documentation "Model a laurent series in VAR^-1 with the first
  coefficient being for VAR^DEGREE."))

(defclass constant-series (power-series)
  ()
  (:documentation "optimisation of constant series, with only a
  coefficient in VAR^0."))

(defun make-constant-series (constant)
  (make-instance 'constant-series :coefficients (finite-coefficients constant)))

;; TODO add support for different variables (also when working
;; together with polynomials).

(defmethod simplified-p ((series power-series))
  "Test whether the first coefficient is indeed not 0, so the degree is
  meaningful."
  (not (zero-p (sref (coefficients series) :start))))

(defmethod simplified-p ((series constant-series))
  t)

(defmethod leading-coefficient ((power-series power-series))
  (let ((lead-coeff (nth-coefficient power-series :start)))
    (if (not (zero-p lead-coeff))
        lead-coeff
        (error 'unsupported-operation-on-unsimplified))))

(defmethod leading-coefficient ((series constant-series))
  (nth-coefficient series 0))

(defmethod nth-coefficient% ((series power-series) n)
  "Return the nth element of the coefficients pipe--or zero, if the
pipe ends before."
  (error 'unsupported-operation))

(defmethod nth-coefficient ((series power-series) n)
  "Return the coefficient of X^n"
  (sref (coefficients series) n))

(defmethod -> ((target-type (eql 'power-series)) (polynomial polynomial) &key)
  (if (zerop (degree polynomial))
      (make-constant-series (constant-coefficient polynomial))
      (make-instance 'power-series
                     :degree (degree polynomial)
                     :coefficients (finseq (coefficients polynomial) 0))))

(defmethod -> ((target-type power-series) (polynomial polynomial) &key)
  (-> 'power-series polynomial))

(create-binary->-wrappers power-series polynomial (:left :right)
  generic-+
  generic--
  generic-*
  generic-/
  generic-=)

(defun make-power-series (degree leading-coefficient &rest coefficients)
  "Create a new power-series with finitely many non-zero COEFFICIENTS.
The LEADING-COEFFICIENT must be non-zero.  If DEGREE is nil, we assume
you want to define a polynomial, thus the DEGREE is just the number of
COEFFICIENTS."
  (when (zero-p leading-coefficient)
    (error "Cannot define a power series [~A~{ ~A~}] with zero leading coefficient."
           leading-coefficient coefficients))
  (make-instance 'power-series
                 :degree (or degree (length coefficients))
                 :coefficients (apply #'finite-coefficients leading-coefficient coefficients)))

(defmacro make-power-series/inf (degree formula)
  "Create a new power-series with given DEGREE and coefficients given
by FORMULA where INDEX is anaphorically bound."
  `(make-instance 'power-series
                  :degree ,degree
                  :coefficients (inf-seq nil (index) ,formula)))

(defparameter default-series-simplification-depth 100)

(defmethod simplify ((series power-series) &key (depth default-series-simplification-depth))
  "Remove zeroes from the start of SERIES and adjust the degree
  accordingly.  In order to avoid infinite loops, at most
  NORMALISATION-DEPTH entries are removed.  Thus the result need not
  satisfy SERIES-NORMALISED-P.  This is indicated by the negative sign
  of the second value, describing the necessary reduction in degree.
  Additionally, when the series is marked finite, and
  NORMALISATION-DEPTH is reached, we will assume the SERIES is 0."
  (with-slots (degree coefficients) series
    (mvbind (coeff stripped) (strip-if #'zero-p coefficients :from :end)
      (setf coefficients coeff
            degree (- degree stripped))))
  (let* ((coeff (coefficients series))
         (non-zero (iter (for i from (degree series) above (- (degree series) depth))
                         (finding i such-that (not (zero-p (sref coeff i)))
                                  on-failure (- (degree series) depth)))))
    (if (and (lazy-array-finite coeff) ;; todo new indices should
             ;; simplify this, moreover we have better tests for
             ;; finite stuff
             (= non-zero depth))
        ;; for finite series, treat reaching normalisation-depth as
        ;; having found the 0 series.
        0
        (values
         (make-instance 'power-series
                        :degree (- (degree series) non-zero)
                        :coefficients (subsequence coeff infinity- non-zero))
         (if (= non-zero depth)
             (- non-zero)
             non-zero)))))

(defmethod simplify ((series constant-series) &key)
  (values series 0))

(defmethod generic-* ((series-a power-series) (series-b power-series))
  ;; todo adjust index arithmetic
  (let ((array-a (coefficients series-a))
        (array-b (coefficients series-b)))
    (make-instance 'power-series
                   :degree (+ (degree series-a)
                              (degree series-b))
                   :coefficients
                   (make-lazy-array
                    (:index-var n
                                :default-value 0
                                :finite (la-finite-test (array-a array-b)
                                                        (+ array-a array-b)))
                    (gm-summing (i 0 n)
                                (gm:* (sref array-a i)
                                      (sref array-b (- n i))))))))

(defmethod generic-* ((series-a constant-series) (series-b constant-series))
  (make-constant-series (generic-* (constant-coefficient series-a)
                                   (constant-coefficient series-b))))

;;; todo maybe figure out a better system for dealing with constants.
;;; (first take care of polynomials, perhaps?

(defmethod generic-* ((series-a power-series) (series-b constant-series))
  (generic-* series-b series-a))

(defmethod generic-* ((series-a constant-series) (series-b power-series))
  "Multiply the SERIES-B with the scalar NUMBER."
  (let ((number (constant-coefficient series-a)))
    (make-instance 'power-series
                   :degree (degree series-b)
                   :coefficients
                   (map-sequence (lambda (x) (gm:* number x)) (coefficients series-b)))))

(defmethod generic-/ ((series-numer power-series) (series-denom power-series))
  (unless (simplified-p series-denom)
    (error "Cannot divide by the SERIES-DENOM ~A unless it is
    normalised, i.e. the first coefficient is non-zero." series-denom))
  ;; todo adjust indices
  (let ((a0 (leading-coefficient series-denom))
        (an (coefficients series-denom))
        (cn (coefficients series-numer)))
    (make-instance 'power-series
                   :degree (- (degree series-numer)
                              (degree series-denom))
                   :coefficients (make-lazy-array (:start ((gm:/ (sref cn 0) a0))
                                                          :index-var n
                                                          :default-value 0)
                                   (gm:/ (gm:- (sref cn n)
                                               (gm-summing (i 1 n)
                                                        (gm:* (sref an i)
                                                              (aref this (- n i)))))
                                         a0)))))

(defmethod generic-/ ((series-numer constant-series) (series-denom constant-series))
  (make-constant-series (generic-/ (constant-coefficient series-numer)
                                   (constant-coefficient series-denom))))

(defmethod generic-/ ((series-numer power-series) (series-denom constant-series))
  (generic-* (make-constant-series (gm:/ (constant-coefficient series-denom)))
             series-numer))

(defmethod generic-/ ((series-numer constant-series) (series-denom power-series))
  "Calculate the inverse series of the given Laurentseries."
  (unless (simplified-p series-denom)
    (error "Cannot invert the SERIES ~A unless it is properly
    normalised, i.e. first coefficient is non-zero." series-denom))
  (let ((a0 (leading-coefficient series-denom))
        (an (coefficients series-denom)))
    ;; todo adjust indices
    (make-instance 'power-series
                   :degree (- (degree series-denom))
                   :coefficients
                   (make-lazy-array (:start ((gm:/ (constant-coefficient series-numer) a0))
                                            :index-var n
                                            :default-value 0)
                     (gm:/ (gm:- (gm-summing (i 1 n)
                                          (gm:* (sref an i)
                                                (aref this (- n i)))))
                           a0)))))

;; TODO perhaps consider additional simplification for units

(defmethod generic-+ ((series-a power-series) (series-b power-series))
  "Add two series together.  Careful: This might destroy
  normalisation."
  (let ((coeff-a (coefficients series-a))
        (coeff-b (coefficients series-b)))
    (make-instance 'power-series
                   :degree (max (degree series-a) (degree series-b))
                   :coefficients
                   (map-sequences/or #'gm:+ coeff-a coeff-b))))

(defmethod generic-- ((series-a power-series) (series-b power-series))
  "Add two series together.  Careful: This might destroy
  normalisation."
  (let ((coeff-a (coefficients series-a))
        (coeff-b (coefficients series-b)))
    (make-instance 'power-series
                   :degree (max (degree series-a) (degree series-b))
                   :coefficients
                   (map-sequences/or #'gm:- coeff-a coeff-b))))

(defmethod generic-+ ((series-a constant-series) (series-b constant-series))
  (make-constant-series (generic-+ (constant-coefficient series-a)
                                   (constant-coefficient series-b))))

(defmethod gm:sqrt ((series power-series))
  "Calculate a square root of this series--as long as the degree is
  even."
  (unless (and (simplified-p series)
               (evenp (degree series)))
    (error "Cannot take the root of SERIES ~A unless the degree is known to be even!" series))
  ;; now we essentially reduce to the case degree = 0
  (let ((a0 (gm:sqrt (nth-coefficient% series 0)))
        (b  (coefficients series)))
    (make-instance 'power-series
                   :degree (/ (degree series) 2)
                   ;; todo adjust indices
     :coefficients (make-lazy-array (:start (a0)
                                            :index-var n
                                            :default-value 0)
                     (gm:/ (gm:- (sref b n)
                                 (gm-summing (i 1 n t) (gm:* (aref this i)
                                                          (aref this (- n i)))))
                           a0 2)))))

(defmethod gm:sqrt ((series constant-series))
  (multiple-value-bind (root nice) (gm:sqrt (constant-coefficient series))
   (values (make-constant-series root) nice)))

(defmethod gm:sqrt ((polynomial polynomial))
  "With power-series available, we can also take the square roots of
polynomials."
  (let* ((root (gm:sqrt (-> 'power-series polynomial)))
         (root-poly (series-truncate root)))
    (setf (var root-poly) (var polynomial))
    ;; check whether the root is a polynomial.
    (if (gm:= (gm:expt root-poly 2) polynomial)
        (values root-poly t)
        (values root nil))))

(defparameter confidence 40
  "How many coefficient of a power series should be compared in order to say they are equal.")

(defmethod generic-= ((series-1 power-series) (series-2 power-series))
  "Compare the first CONFIDENCE coefficients of the series.  If they
match, consider the series equal."
  (when (= (degree series-1)
           (degree series-2))
    (let ((co-1 (coefficients series-1))
          (co-2 (coefficients series-2)))
      (when
          (iter (for i from 0 to confidence)
                (always (gm:= (sref co-1 i)
                              (sref co-2 i))))
        confidence))))
;; TODO fix problems with possibly not yet simplified series (for
;; instance a series representing 0, but without "knowing" it)

(defmethod generic-= ((series-1 constant-series) (series-2 constant-series))
  (= (constant-coefficient series-1) (constant-coefficient series-2)))

;; extracting and removing polynomial part of the laurent series
(defmethod series-truncate ((series power-series))
  "Take the polynomial part of the given SERIES."
  ;; Evaluate (all) the coefficients of polynomial parts
  (let ((d (degree series)))
   (if (minusp d)
       0
       (make-instance 'polynomial
                      ;; :degree d
                      ;; todo check the array direction is correct
                      :coefficients (seq->array (subsequence (coefficients series)
                                                  0 infinity+))))))

(defmethod series-truncate ((series constant-series))
  (make-polynomial (constant-coefficient series)))

(defmethod series-remainder ((series power-series))
  "Remove the polynomial part from the given SERIES -- thus equivalent
 to SERIES - (series-truncate SERIES).  Careful, the result is
 possibly not yet simplified."
  (let ((d (degree series)))
    (if (< d 0)
        series ; no polynomial part -> nothing to do
        (make-instance 'power-series
                       :degree -1 
                       :coefficients (subsequence (coefficients series)
                                                  infinity- -1)))))

(defmethod series-remainder ((series constant-series))
  (zero series))

;;; reducing mod p
(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (power-series power-series) &key (mod 2))
  (simplify
   (make-instance 'power-series
                  :degree (degree power-series)
                  :coefficients
                  (map-sequence
                   (lambda (x) (-> 'finite-fields:integer-mod x :mod mod))
                   (coefficients power-series)))))

(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (constant-series constant-series) &key (mod 2))
  (make-constant-series (-> 'finite-fields:integer-mod
                            (constant-coefficient constant-series) :mod mod)))


;;; in case of finite power series, the precision should be explicit

;;; compatibility with constant coefficients
(defmethod -> ((target-type1589 (eql (quote power-series))) (rational rational) &key)
  (make-constant-series rational))

(defmethod -> ((target-type1589 (eql (quote constant-series))) (rational rational) &key)
  (make-constant-series rational))

(defmethod -> ((power-series power-series) (rational rational) &key)
  (make-constant-series rational))

(default-simple-type-conversion rational power-series)
