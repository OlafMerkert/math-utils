(defpackage :polynomials
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :generic-math
        :iterate)
  (:export
   :degree
   :nth-coefficient%
   :nth-coefficient
   :polynomial
   :coefficients
   :make-polynomial
   :constant-coefficient
   :format-monomial/tex))

(in-package :polynomials)

(defclass polynomial ()
  ((coefficients :initform (vector 0)
                 :initarg :coefficients
                 :reader coefficients)
   #|(var :initform 'x
   :accessor var)|#)
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS."))

;; TODO unify polynomial interface with power series interface
(defmethod degree ((polynomial polynomial))
  (1- (length (coefficients polynomial))))

(defmethod nth-coefficient% ((polynomial polynomial) n)
  (aref (coefficients polynomial) n))

(defmethod nth-coefficient ((polynomial polynomial) n)
  (let ((d (degree polynomial)))
    (nth-coefficient% polynomial (- d n))))

(defun constant-coefficient (polynomial)
  "This is just an abbreviation for (nth-coefficient p 0)"
  (nth-coefficient polynomial 0))

(defmethod simplified-p ((polynomial polynomial))
  (or (zerop (degree polynomial))
      (not (zero-p
            (nth-coefficient% polynomial 0)))))

(defun make-polynomial (lk &rest coefficients)
  (make-instance 'polynomial :coefficients (list->array (list* lk coefficients))))

(defmethod zero ((number polynomial))
  (make-polynomial 0))

(defmethod zero ((number (eql 'polynomial)))
  (make-polynomial 0))

(defmethod one ((number polynomial))
  (make-polynomial 1))

(defmethod one ((number (eql 'polynomial)))
  (make-polynomial 1))

(defmethod simplify ((polynomial polynomial) &key)
  "Remove all leading zeros from the coefficients.  If all
  coefficients are zero, keep the last zero."
  (with-slots (coefficients) polynomial
    (let* ((deg (- (length coefficients) 1))
           (nz (or (position-if-not #'zero-p coefficients
                                    :end deg)
                   deg)))
      (setf coefficients
            (subseq coefficients nz))
      (values polynomial nz))))

;;; arithmetic of polynomials
(defmethod generic-* ((poly-a polynomial) (poly-b polynomial))
  "Multiply two polynomials."
  (let ((array-a (coefficients poly-a))
        (array-b (coefficients poly-b))
        (deg-a   (degree poly-a))
        (deg-b   (degree poly-b)))
    (make-instance 'polynomial
                   :coefficients
                   (make-nlazy-array
                       (:index-var n
                                   :default-value 0
                                   :finite (+ deg-a deg-b 1))
                     (summing (i (max 0 (- n deg-b))
                                 (min n deg-a))
                              (gm:* (aref array-a i)
                                    (aref array-b (- n i))))))))

(defmethod generic-* ((poly-b polynomial) (int integer))
  (generic-* int poly-b))

(defmethod generic-* ((int integer) (poly-b polynomial))
  (make-instance 'polynomial
                 :coefficients
                 (map 'vector (lambda (x) (gm:* int x)) (coefficients poly-b))))

(defmethod generic-+ ((poly-a polynomial) (poly-b polynomial))
  "Add two polynomials together.  Implicitly simplify."
  (if (> (degree poly-a) (degree poly-b))
      (generic-+ poly-b poly-a)
      ;; now poly-b has the higher degree
      (let ((coeff-a (coefficients poly-a))
            (coeff-b (coefficients poly-b))
            (d (- (degree poly-b) (degree poly-a))))
        (simplify
         (make-instance 'polynomial
                        :coefficients
                        (make-nlazy-array (:index-var n :default-value 0
                                                      :finite (+ (degree poly-b) 1))
                          (if (< n d)
                              (aref coeff-b n)
                              (gm:+ (aref coeff-b n)
                                    (aref coeff-a (- n d))))))))))

(defmethod generic-- ((poly-a polynomial) (poly-b polynomial))
  (generic-+ poly-a (generic-* -1 poly-b)))

(defmethod generic-/ ((poly-numer polynomial) (poly-denom polynomial))
  "This actually implements polynomial division with a remainder.
Keep this in mind when using."
  (unless (simplified-p poly-denom)
    (error "Cannot divide by the POLY-DENOM ~A unless it is
    normalised, i.e. the first coefficient is non-zero." poly-denom))
  (when (zero-p poly-denom)
    (error "Cannot divide by ZERO."))
  (let ((a0 (nth-coefficient% poly-denom 0))
        (an (coefficients poly-denom))
        (cn (coefficients poly-numer))
        (deg-a (degree poly-denom)) 
        (deg (- (degree poly-numer)
                (degree poly-denom))))
    (if (minusp deg)
        (values (zero 'polynomial) poly-numer)
        (let ((result
               (make-instance 'polynomial
                              :coefficients
                              (make-nlazy-array (:start ((gm:/ (aref cn 0) a0))
                                                        :index-var n
                                                        :finite (+ deg 1)
                                                        :default-value 0)
                                (gm:/ (gm:- (aref cn n)
                                            (summing (i 1 (min n deg-a))
                                                     (gm:* (aref an i)
                                                           (aref this (- n i)))))
                                      a0)))))
          (values result ;(gm:- poly-numer (gm:* poly-denom result))
                  )))))

;;; comparison
(defmethod generic-= ((poly-a polynomial) (poly-b polynomial))
  "Compare two polynomials for equality, assuming both are already
  simplified."
  (let ((d (degree poly-a)))
    (and (= d (degree poly-b))
         (iter (for a in-vector (coefficients poly-a))
               (for b in-vector (coefficients poly-b) )
               (always (gm:= a b))))))

;;; output of polynomials
(defmethod print-object ((polynomial polynomial) stream)
  (princ #\[ stream)
  (iter (for i from 0 to  (degree polynomial))
        (unless (zerop i)
          (format stream " + ")
          (format stream "~A X^~A"
                  (nth-coefficient% polynomial i)
                  (- (degree polynomial) i))))
  (princ #\] stream)
  #|(terpri)|#)

(defun format-monomial/tex (stream coefficient exponent)
  "Format a monomial expression nicely, such that nothing like X^0 =
1, X^1 = X or 1 \, X^n = X^n appears.  Something like 0 \, X^n will
not be handled here, however."
  (if (one-p coefficient)
      (case exponent
        ((0) (print-object/tex coefficient stream))
        ((1) (princ "X" stream))
        (t   (format stream "X^{~A}" exponent)))
      (progn
        (print-object/tex coefficient stream)
        (case exponent
          ((0))
          ((1) (princ " \\, X" stream))
          (t   (format stream " \\, X^{~A}" exponent))))))

(defmethod print-object/tex ((polynomial polynomial) stream)
  (iter (for i from 0 to (degree polynomial))
        (for coefficient = (nth-coefficient% polynomial i))
        (for exponent    = (- (degree polynomial) i))
        (for zero-p      = (zero-p coefficient))
        (unless (or (zerop i) zero-p)
          (format stream " + "))
        (unless zero-p 
          (format-monomial/tex stream coefficient exponent))))
