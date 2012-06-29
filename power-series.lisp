(defpackage :power-series
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :generic-math)
  (:export
   :confidence
   :nth-coefficient%
   :nth-coefficient
   :default-series-simplification-depth
   :series-truncate
   :series-remainder
   :power-series
   :constant-series
   :make-constant-series
   :constant-coefficient))

(in-package :power-series)

(defclass power-series ()
  ((degree :initform 0
           :initarg :degree
           :reader degree)
   (coefficients :initform (la% 0)
                 :initarg :coefficients
                 :reader coefficients)
   #|(var :initform 'x
        :accessor var)|#)
  (:documentation "Model a laurent series in VAR^-1 with the first
  coefficient being for VAR^DEGREE."))

(defclass constant-series (power-series)
  ()
  (:documentation "optimisation of constant series, with only a
  coefficient in VAR^0."))

(defun make-constant-series (constant)
  (make-instance 'constant-series :coefficients (la% 0 constant)))

(defun constant-coefficient (series)
  #|(unless (zerop (degree series))
    (error "CONSTANT-COEFFICIENT only works with 0 DEGREE."))|#
  (nth-coefficient% series 0))

;; TODO add support for different variables.

(defmethod simplified-p ((series power-series))
  "Test whether the first coefficient is indeed not 0, so the degree is
  meaningful."
  (not (zero-p (lazy-aref (coefficients series) 0))))

(defmethod simplified-p ((series constant-series))
  t)

(defmethod nth-coefficient% ((series power-series) n)
  "Return the nth element of the coefficients pipe--or zero, if the
pipe ends before."
  (unless (>= n 0)
    (error "Non negative index ~A in NTH-COEFFICIENT%." n))
  (lazy-aref (coefficients series) n))

(defmethod nth-coefficient ((series power-series) n)
  "Return the coefficient of X^n"
  (if (<= n (degree series))
      (nth-coefficient% series (- (degree series) n))
      0))

(defun make-power-series/polynomial (leading-coefficient &rest coefficients)
  (when (zerop leading-coefficient)
    (error "Cannot define a power series [~A~{ ~A~}] with zero leading coefficient."
           leading-coefficient coefficients))
  (make-instance 'power-series
                 :degree (length coefficients)
                 :coefficients (apply #'la% 0 leading-coefficient coefficients)))

(defmethod zero ((number power-series))
  (make-constant-series 0))

(defmethod one ((number power-series))
  (make-constant-series 1))

;; TODO visualising polynomials and power series

(defparameter default-series-simplification-depth 100)

(defmethod simplify ((series power-series) &key (depth default-series-simplification-depth))
  "Remove zeroes from the start of SERIES and adjust the degree
  accordingly.  In order to avoid infinite loops, at most
  NORMALISATION-DEPTH entries are removed.  Thus the result need not
  satisfy SERIES-NORMALISED-P.  This is indicated by the negative sign
  of the second value, describing the necessary reduction in degree.
  Additionally, when the series is marked finite, and
  NORMALISATION-DEPTH is reached, we will assume the SERIES is 0."
  (let* ((coeff (coefficients series))
         (non-zero (loop
                      for i from 0 below depth
                      for zp = (zero-p (lazy-aref coeff i))
                      while zp
                      finally (return (if zp (+ i 1) i)))))
    (if (and (lazy-array-finite coeff)
             (= non-zero depth))
        ;; for finite series, treat reaching normalisation-depth as
        ;; having found the 0 series.
        (zero series)
        (values
         (make-instance 'power-series
                        :degree (- (degree series) non-zero)
                        :coefficients (lazy-array-drop coeff non-zero))
         (if (= non-zero depth)
             (- non-zero)
             non-zero)))))

(defmethod simplify ((series constant-series) &key)
  (values series 0))

(defmethod generic-* ((series-a power-series) (series-b power-series))
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
                     (loop for i from 0 to n
                        for pr = (gm:* (lazy-aref array-a i)
                                       (lazy-aref array-b (- n i)))
                        for sum = pr then (gm:+ sum pr)
                        finally (return sum))))))

(defmethod generic-* ((series-a constant-series) (series-b constant-series))
  (make-constant-series (generic-* (constant-coefficient series-a)
                                   (constant-coefficient series-b))))

(defmethod generic-* ((series-a power-series) (series-b constant-series))
  (generic-* series-b series-a))

(defmethod generic-* ((series-a constant-series) (series-b power-series))
  "Multiply the SERIES-B with the scalar NUMBER."
  (let ((number (constant-coefficient series-a)))
    (make-instance 'power-series
                   :degree (degree series-b)
                   :coefficients
                   (make-lazy-array (:index-var n :default-value 0
                                                :finite (lazy-array-finite (coefficients series-b)))
                                   (* number (lazy-aref (coefficients series-b) n))))))


(defmethod gm:expt ((base power-series) (power (eql 2)))
  (generic-* base base))

(defmethod generic-/ ((series-numer power-series) (series-denom power-series))
  (unless (simplified-p series-denom)
    (error "Cannot dive by the SERIES-DENOM ~A unless it is
    normalised, i.e. the first coefficient is non-zero." series-denom))
  (let ((a0 (nth-coefficient% series-denom 0))
        (an (coefficients series-denom))
        (cn (coefficients series-numer)))
    (make-instance 'power-series
                   :degree (- (degree series-numer)
                              (degree series-denom))
                   :coefficients (make-lazy-array (:start ((gm:/ (lazy-aref cn 0) a0))
                                                          :index-var n
                                                          :default-value 0)
                                   (gm:/ (- (lazy-aref cn n)
                                            (loop for i from 1 to n
                                               for pr = (gm:* (lazy-aref an i)
                                                              (aref this (- n i)))
                                               for sum = pr then (gm:+ sum pr)
                                               finally (return sum)))
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
  (let ((b0 (gm:/ (nth-coefficient% series-denom 0) (constant-coefficient series-numer)))
        (an (coefficients series-denom)))
    (make-instance 'power-series
                   :degree (- (degree series-denom))
                   :coefficients (make-lazy-array (:start (b0)
                                                          :index-var n)
                                   (gm:* -1 b0
                                         (loop for i from 1 to n
                                            for pr = (gm:* (lazy-aref an i)
                                                           (aref this (- n i)))
                                            for sum = pr then (gm:+ sum pr)
                                            finally (return sum)))))))

;; TODO perhaps consider additional simplification for units

(defmethod generic-+ ((series-a power-series) (series-b power-series))
  "Add two series together.  Careful: This might destroy
  normalisation."
  (if (> (degree series-a) (degree series-b))
      (generic-+ series-b series-a)
      ;; now series-b has the higher degree
      (let ((coeff-a (coefficients series-a))
            (coeff-b (coefficients series-b))
            (d (- (degree series-b) (degree series-a))))
        (make-instance 'power-series
                       :degree (degree series-b)
                       :coefficients
                       (make-lazy-array (:index-var n :default-value 0
                                                    :finite
                                                    (la-finite-test (coeff-a coeff-b)
                                                      (max coeff-a coeff-b)))
                         (if (< n d)
                             (lazy-aref coeff-b n)
                             (gm:+ (lazy-aref coeff-b n)
                                   (lazy-aref coeff-a (- n d)))))))))

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
     :degree (gm:/ (degree series) 2)
     :coefficients (make-lazy-array (:start (a0)
                                            :index-var n)
                     (gm:/ (gm:- (lazy-aref b n)
                                 (loop for i from 1 below n
                                    for pr = (gm:* (aref this i)
                                                   (aref this (- n i)))
                                    for sum = pr then (gm:+ sum pr)
                                    finally (return sum)))
                           a0 2)))))

(defmethod gm:sqrt ((series constant-series))
  (multiple-value-bind (root nice) (gm:sqrt (constant-coefficient series))
   (values (make-constant-series root) nice)))

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
          (loop for i from 0 to confidence
             always (gm:= (lazy-aref co-1 i)
                          (lazy-aref co-2 i)))
        confidence))))

(defmethod generic-= ((series-1 constant-series) (series-2 constant-series))
  (generic-= (constant-coefficient series-1) (constant-coefficient series-2)))

;; extracting and removing polynomial part of the laurent series
(defmethod series-truncate ((series power-series))
  "Take the polynomial part of the given SERIES."
  ;; Evaluate (all) the coefficients of polynomial parts
  (let ((d (degree series)))
   (if (< d 0)
       (zero series)
       (make-instance 'power-series
                      :degree d
                      :coefficients (lazy-array-take (coefficients series) (+ d 1))))))

(defmethod series-truncate ((series constant-series))
  series)

(defmethod series-remainder ((series power-series))
  "Remove the polynomial part from the given SERIES -- thus equivalent
 to SERIES - (series-truncate SERIES).  Careful, the result is
 possibly not yet simplified."
  (let ((d (degree series)))
    (if (< d 0)
        series ; no polynomial part -> nothing to do
        (make-instance 'power-series
                       :degree -1 
                       :coefficients (lazy-array-drop (coefficients series) (+ d 1))))))

(defmethod series-remainder ((series constant-series))
  (zero series))