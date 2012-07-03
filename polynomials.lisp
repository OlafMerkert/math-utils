(defpackage :polynomials
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :generic-math)
  (:export
   :degree
   :nth-coefficient%
   :nth-coefficient
   :polynomial
   :coefficients
   :make-polynomial
   :constant-coefficient))

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
                                   :finite (nla-finite-test (array-a array-b)
                                             (+ array-a array-b)))
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
  "Add two polynomials together."
  (if (> (degree poly-a) (degree poly-b))
      (generic-+ poly-b poly-a)
      ;; now poly-b has the higher degree
      (let ((coeff-a (coefficients poly-a))
            (coeff-b (coefficients poly-b))
            (deg-a   (degree poly-a))
            (deg-b   (degree poly-b)))
        (simplify ; automatically simplify--this is more convenient here.
         (make-instance 'polynomial
                        :coefficients
                        (make-nlazy-array
                            (:index-var n
                                        :default-value 0
                                        :finite deg-b)
                          (if (<= n deg-a)
                              (gm:+ (aref coeff-a n)
                                    (aref coeff-b n))
                              (aref coeff-b n))))))))

(defmethod generic-- ((poly-a polynomial) (poly-b polynomial))
  (generic-+ poly-a (generic-* -1 poly-b)))

;;; comparison
(defmethod generic-= ((poly-a polynomial) (poly-b polynomial))
  "Compare two polynomials for equality, assuming both are already
  simplified."
  (let ((d (degree poly-a)))
    (and (= d (degree poly-b))
         (let ((coeff-a (coefficients poly-a))
               (coeff-b (coefficients poly-b)))
          (loop for i from 0 to d
             always (gm:= (aref coeff-a i)
                          (aref coeff-b i)))))))

;;; output of polynomials
(defmethod print-object ((polynomial polynomial) stream)
  (loop
     for i from 0 upto  (degree polynomial)
     unless (zerop i)
     do (format stream " + ")
     do (format stream "~A X^~A"
                (nth-coefficient% polynomial i)
                (- (degree polynomial) i)))
  (terpri))
