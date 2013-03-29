(defpackage :polynomials
  (:shadowing-import-from :fractions :numerator :denominator)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :generic-math
        :iterate :fractions)
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
   :var
   :simplify-poly
   :poly+poly
   :poly*poly
   :poly*constant
   :poly+constant
   :make-monomial))

(in-package :polynomials)

(defclass polynomial ()
  ((coefficients :initform (vector 0)
                 :initarg :coefficients
                 :reader coefficients)
   (var :initform 'X
        :initarg :var
        :accessor var))
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS."))

;; unify polynomial interface with power series interface
(defmethod degree ((polynomial polynomial))
  (1- (length (coefficients polynomial))))

(defmethod degree ((fraction fraction))
  (- (degree (numerator fraction)) (degree (denominator fraction))))

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

(defmethod zero-p ((polynomial polynomial))
  ;; assume poly is simplified for now
  (assert (simplified-p polynomial))
  (and (zerop (degree polynomial))
       (zero-p (constant-coefficient polynomial))))

(defmethod one-p ((polynomial polynomial))
  (assert (simplified-p polynomial))
  (and (zerop (degree polynomial))
       (one-p (constant-coefficient polynomial))))

(defun simplify-poly (polynomial)
  (with-slots (coefficients) polynomial
    (let* ((deg (- (length coefficients) 1))
           (nz (or (position-if-not #'zero-p coefficients
                                    :end deg)
                   deg)))
      (setf coefficients
            (subseq coefficients nz))
      (values polynomial nz))))

(defmethod simplify ((polynomial polynomial) &key)
  "Remove all leading zeros from the coefficients.  If all
  coefficients are zero, keep the last zero."
  (simplify-poly polynomial))

;;; arithmetic of polynomials
(defun poly*poly (poly-a poly-b &optional (type 'polynomial))
  (let ((array-a (coefficients poly-a))
        (array-b (coefficients poly-b))
        (deg-a   (degree poly-a))
        (deg-b   (degree poly-b)))
    (make-instance type
                   :var (var poly-a)
                   :coefficients
                   (make-nlazy-array
                       (:index-var n
                                   :default-value 0
                                   :finite (+ deg-a deg-b 1))
                     (summing (i (max 0 (- n deg-b))
                                 (min n deg-a))
                              (gm:* (aref array-a i)
                                    (aref array-b (- n i))))))))

(defun poly*constant (poly constant &optional (type 'polynomial))
  (make-instance type :var (var poly)
                 :coefficients (map 'vector (lambda (x) (gm:* constant x))
                                    (coefficients poly))))

(defmethod generic-* ((poly-a polynomial) (poly-b polynomial))
  "Multiply two polynomials."
  (poly*poly poly-a poly-b))

(defmethod generic-* ((poly-b polynomial) (number rational))
  (generic-* number poly-b))

(defmethod generic-* ((number rational) (poly-b polynomial))
  (poly*constant poly-b number))

(defun poly+constant (poly constant &optional (type 'polynomial))
  (make-instance type :var (var poly)
                 :coefficients (aprog1 (copy-seq (coefficients poly))
                                 (setf (alast it) (gm:+ (alast it) constant)))))

(defun poly+poly (poly-a poly-b &optional (type 'polynomial))
  (if (> (degree poly-a) (degree poly-b))
      (poly+poly poly-b poly-a type)
      ;; now poly-b has the higher degree
      (let ((coeff-a (coefficients poly-a))
            (coeff-b (coefficients poly-b))
            (d (- (degree poly-b) (degree poly-a))))
        (simplify-poly
         (make-instance type
                        :var (var poly-a)
                        :coefficients
                        (make-nlazy-array (:index-var n :default-value 0
                                                      :finite (+ (degree poly-b) 1))
                          (if (< n d)
                              (aref coeff-b n)
                              (gm:+ (aref coeff-b n)
                                    (aref coeff-a (- n d)))))))))
  )
(defmethod generic-+ ((poly-a polynomial) (poly-b polynomial))
  "Add two polynomials together.  Implicitly simplify."
  (poly+poly poly-a poly-b))

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
        (values (zero poly-numer) poly-numer)
        (let ((qn (make-array (+ m-n 1) :initial-element 0)))
          (iter (for k from 0 to m-n)
                (setf (aref qn k) (gm:* (aref an k) b0))
                (iter (for j from 0 to n)
                      (for jj from k to (+ k n))
                      (setf (aref an jj) (gm:- (aref an jj)
                                              (gm:* (aref qn k) (aref bn j))))))
          (values (simplify (make-instance 'polynomial :var (var poly-numer)
                                           :coefficients qn))
                  (simplify (make-instance 'polynomial :var (var poly-numer)
                                           :coefficients (subseq an m-n))))))))

;; TODO provide condition when division has remainder

(defun poly-divisible-p (divisor polynomial)
  (let ((remainder (nth-value 1 (generic-/ polynomial divisor))))
    (or (zero-p remainder)
        (values nil remainder))))

(defmethod ggt ((a polynomial) (b polynomial))
  (if (zero-p b) a
      (ggt b (nth-value 1 (generic-/ a b)))))

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
                  :var (var polynomial)
                  :coefficients (map 'vector
                                     (lambda (x) (-> 'finite-fields:integer-mod x :mod mod))
                                     (coefficients polynomial)))))

(defmethod derivative (number &key (var 'X))
  (zero number))

(defmethod derivative ((polynomial polynomial) &key (var 'X))
  "Calculate the usual derivative of a polynomial."
  (if (eql var (var polynomial))
      (simplify
       (make-instance 'polynomial
                      :var (var polynomial)
                      :coefficients (map 'vector #'gm:*
                                         (subseq (coefficients polynomial)
                                                 0 (degree polynomial))
                                         (mrange (degree polynomial) 1))))
      (zero polynomial)))

;;; compatibility with constant coefficients
(define->-method (polynomial rational (:var var 'X))
    :coefficients (vector rational))

(create-binary->-wrappers polynomial rational (:left :right)
  generic-+
  generic--
  generic-*
  generic-/)
