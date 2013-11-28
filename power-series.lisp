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
   #:leading-coefficient
   #:make-power-series%
   #:exponential-series))

(in-package :power-series)

;; develop power-series backed by infinite sequences. In order to
;; simplify the code, we always use sequences starting from 0, going
;; up to infinity+. As a first step, implement it using
;; infinite+-sequence with general starting bound, but perhaps we
;; shall move to a slightly simpler datastructure for efficiency
;; reasons (there might be a number of redundant operations
;; happening). But first we want functionality.

(declaim (inline finite-coefficients))
(defun finite-coefficients (&rest coefficients)
  ;; todo maybe the reverse operation should go into an iseq function??
  (seq->iseq/sv coefficients :start 0 :end (- (length coefficients) 1) :standard-value 0))


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
  (not (zero-p (sref (coefficients series) 0))))

(defmethod simplified-p ((series constant-series))
  t)

(defmethod leading-coefficient ((power-series power-series))
  (let ((lead-coeff (sref (coefficients  power-series) 0)))
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
  (sref (coefficients series) (- (degree series) n)))

(defmethod -> ((target-type (eql 'power-series)) (polynomial polynomial) &key)
  (if (zerop (degree polynomial))
      (make-constant-series (constant-coefficient polynomial))
      (make-instance 'power-series
                     :degree (degree polynomial)
                     ;; todo check the right ordering of coefficients
                     :coefficients (seq->iseq/sv (coefficients polynomial) :start 0 :end (degree polynomial) :standard-value 0))))

(defmethod -> ((target-type power-series) (polynomial polynomial) &key)
  (-> 'power-series polynomial))

(create-binary->-wrappers power-series polynomial (:left :right)
  generic-+
  generic--
  generic-*
  generic-/
  generic-=)

(defun make-power-series% (degree leading-coefficient &rest coefficients)
  "As `make-power-series', but don't check whether the
`leading-coefficient' is zero."
  (make-instance 'power-series
                 :degree (or degree (- (length coefficients) 1))
                 :coefficients (apply #'finite-coefficients leading-coefficient coefficients)))

(defun make-power-series (degree leading-coefficient &rest coefficients)
  "Create a new power-series with finitely many non-zero `coefficients'.
The `leading-coefficient' must be non-zero. If DEGREE is nil, we
assume you want to define a polynomial, thus the `degree' is just the
number of `coefficients'."
  (if (zero-p leading-coefficient)
      (error "Cannot define a power series [~A~{ ~A~}] with zero leading coefficient."
             leading-coefficient coefficients)
      (apply #'make-power-series% degree leading-coefficient coefficients)))


(defmacro! make-power-series/inf (o!degree formula)
  "Create a new power-series with given DEGREE and coefficients given
by FORMULA where INDEX is anaphorically bound."
  `(make-instance 'power-series
                  :degree ,g!degree
                  :coefficients (inf+seq nil (,g!index)
                                  (let ((index (- ,g!degree ,g!index)))
                                    (declare (ignorable index))
                                    ,formula))))

(defparameter default-series-simplification-depth 100)

(defmethod simplify ((series power-series) &key (depth default-series-simplification-depth))
  "Remove zeroes from the start of SERIES and adjust the degree
  accordingly.  In order to avoid infinite loops, at most
  NORMALISATION-DEPTH entries are removed.  Thus the result need not
  satisfy SERIES-NORMALISED-P.  This is indicated by the negative sign
  of the second value, describing the necessary reduction in degree.
  Additionally, when the series is marked finite, and
  NORMALISATION-DEPTH is reached, we will assume the SERIES is 0."
  ;; todo treat series with finite data differently (do complete
  ;; simplification there)
  (with-slots (degree coefficients) series
    (mvbind (coeff stripped) (strip-if #'zero-p coefficients :from :start :limit depth)
      ;; todo perhaps integrate shifting into strip-if??
      (setf coefficients (shift coeff (- stripped))
            degree (- degree stripped))
      (values series (if (>= stripped depth) (- stripped) stripped)))))

(defmethod simplify ((series constant-series) &key)
  (values series 0))

;; todo how about series with finitely many entries
(defmethod generic-* ((series-a power-series) (series-b power-series))
  (let* ((seq-a (coefficients series-a))
         (seq-b (coefficients series-b)))
    (make-instance
     'power-series
     :degree (+ (degree series-a) (degree series-b))
     ;; todo special case for finite sequences
     :coefficients (inf+seq nil (n)
                     (gm-summing (i 0 n)
                                 (gm:* (sref seq-a i) (sref seq-b (- n i))))))))

(defmethod generic-* ((series-a constant-series) (series-b constant-series))
  (make-constant-series (generic-* (constant-coefficient series-a)
                                   (constant-coefficient series-b))))

;;; todo maybe figure out a better system for dealing with constants.
;;; (first take care of polynomials, perhaps?

;; todo use a macro
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
  (let ((a0 (leading-coefficient series-denom))
        (an (coefficients series-denom))
        (cn (coefficients series-numer)))
    (make-instance
     'power-series
     :degree  (- (degree series-numer) (degree series-denom))
     :coefficients
     (inf+seq (vector (gm:/ (sref cn 0) a0))
         (n) (gm:/ (gm:- (sref cn n)
                         (gm-summing (i 0 n t)
                                     (gm:* (sref an (- n i))
                                           (this i))))
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
  (let* ((a0 (leading-coefficient series-denom))
         (a0m (gm:- a0))
         (an (coefficients series-denom)))
    (make-instance
     'power-series
     :degree (- (degree series-denom))
     :coefficients
     (inf+seq (vector (gm:/ (constant-coefficient series-numer) a0))
         (n)
       (gm:/ (gm-summing (i 0 n t)
                         (gm:* (sref an (- n i))
                               (this i)))
             a0m)))))

;; TODO perhaps consider additional simplification for units

(bind-multi ((generic-+ generic-+ generic--)
             (gm:+ gm:+ gm:-)
             (+uncalculated+ +uncalculated+ 0))
 (defmethod generic-+ ((series-a power-series) (series-b power-series))
  ;; Careful: This might destroy normalisation.
  (let ((coeff-a (coefficients series-a))
        (coeff-b (coefficients series-b))
        (deg-a (degree series-a))
        (deg-b (degree series-b)))
    ;; in case the degrees are different, we need to offset one of the series
    (cond ((< deg-a deg-b)
           (setf coeff-a (shift coeff-a (- deg-b deg-a))))
          ((< deg-b deg-a)
           (setf coeff-b (shift coeff-b (- deg-a deg-b)))))
    (make-instance 'power-series
                   :degree (max (degree series-a) (degree series-b))
                   :coefficients
                   (map-sequences/or #'gm:+ +uncalculated+ coeff-a coeff-b))))

 (defmethod generic-+ ((series-a constant-series) (series-b constant-series))
   (make-constant-series (generic-+ (constant-coefficient series-a)
                                    (constant-coefficient series-b)))))

(defmethod gm:sqrt ((series power-series))
  "Calculate a square root of this series--as long as the degree is
  even."
  (unless (and (simplified-p series)
               (evenp (degree series)))
    (error "Cannot take the root of SERIES ~A unless the degree is known to be even!" series))
  ;; now we essentially reduce to the case degree = 0
  (let* ((a0 (gm:sqrt (leading-coefficient series)))
         (a0d (gm:* 2 a0))
         (b  (coefficients series))
         (degree (/ (degree series) 2)))
    (make-instance
     'power-series
     :degree degree
     ;; todo adjust indices
     :coefficients
     (inf+seq  (vector a0)
         (n)
       (gm:/ (gm:- (sref b n)
                   (gm-summing (i 1 n t) (gm:* (this i)
                                               (this (- n i)))))
             a0d)))))

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
  "Compare the first `confidence' coefficients of the two series. If
they match, consider the series equal."
  (when (= (degree series-1)
           (degree series-2))
    (let ((co-1 (coefficients series-1))
          (co-2 (coefficients series-2)))
      (when (iter (for i from 0 below confidence)
                  (always (gm:= (sref co-1 i) (sref co-2 i))))
        confidence))))
;; TODO fix problems with possibly not yet simplified series (for
;; instance a series representing 0, but without "knowing" it)

(defmethod generic-= ((series-1 constant-series) (series-2 constant-series))
  (gm:= (constant-coefficient series-1) (constant-coefficient series-2)))

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
                                                              0 d))))))

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
                                                  (+ 1 d) infinity+)))))

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

;;; some special series
(defun exponential-series (number)
  "Generate the exponential power series."
  (make-power-series/inf
   0
   (let ((n (- index)))
     ;; todo make use of previous coefficient for more efficient calculation
     (gm:/ (gm:^ number n)
           (combi:factorial n)))))
