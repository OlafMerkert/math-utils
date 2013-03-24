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
   :leading-coefficient
   :derivative
   :make-monomial))

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

(defmethod leading-coefficient ((polynomial polynomial))
  (if (simplified-p polynomial)
      (nth-coefficient% polynomial 0)
      (error "Trying to take leading-coefficient of non-simplified polynomial.")))

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

(defun make-monomial (degree coefficient)
  (let ((coeff (make-array (+ 1 degree) :initial-element 0)))
    (setf (aref coeff 0) coefficient)
    (make-instance 'polynomial :coefficients coeff)))


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
  (let* ((b0 (gm:/ (nth-coefficient% poly-denom 0)))
         (bn (coefficients poly-denom))
         (an (copy 'vector (coefficients poly-numer)))
         (m (degree poly-numer))
         (n (degree poly-denom))
         (m-n (- m n)))
    (if (minusp m-n)
        (values (zero 'polynomial) poly-numer)
        (let ((qn (make-array (+ m-n 1) :initial-element 0)))
          (iter (for k from 0 to m-n)
                (setf (aref qn k) (gm:* (aref an k) b0))
                (iter (for j from 0 to n)
                      (for jj from k to (+ k n))
                      (setf (aref an jj) (gm:- (aref an jj)
                                              (gm:* (aref qn k) (aref bn j))))))
          (values (simplify (make-instance 'polynomial :coefficients qn))
                  (simplify (make-instance 'polynomial :coefficients (subseq an m-n))))))))

;; TODO provide condition when division has remainder

;;; comparison
(defmethod generic-= ((poly-a polynomial) (poly-b polynomial))
  "Compare two polynomials for equality, assuming both are already
  simplified."
  (let ((d (degree poly-a)))
    (and (cl:= d (degree poly-b))
         (iter (for a in-vector (coefficients poly-a))
               (for b in-vector (coefficients poly-b) )
               (always (gm:= a b))))))

;;; reducing mod p
(defmethod -> ((target-type (eql 'finite-fields:integer-mod)) (polynomial polynomial) &key (mod 2))
  (simplify
   (make-instance 'polynomial
                  :coefficients (map 'vector
                                     (lambda (x) (-> 'finite-fields:integer-mod x :mod mod))
                                     (coefficients polynomial)))))

(defmethod derivative ((polynomial polynomial))
  "Calculate the usual derivative of a polynomial."
  (simplify
   (make-instance 'polynomial
                  :coefficients (map 'vector #'gm:*
                                     (subseq (coefficients polynomial)
                                             0 (degree polynomial))
                                     (mrange (degree polynomial) 1)))))
