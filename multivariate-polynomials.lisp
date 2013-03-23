(defpackage :multivariate-polynomials
  (:nicknames :mpoly)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :iterate
        :generic-math
        :math-variables
        :polynomials)
  (:export
   #:mpolynomial
   #:mpoly-p
   #:mpoly-cases
   #:mdegree))

(in-package :multivariate-polynomials)

(defclass mpolynomial (polynomial)
  ()
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS. Here coefficients only
  contains variables of lower order."))

(defun mpoly-p (mp)
  (typep mp 'mpolynomial))

(defmacro! mpoly-cases ((o!mpoly o!var) > = < &optional (not-mpoly >))
  "A convenience macro for working with multivariate polynomials,
respecting ordering of variables.

< means coefficients may contain expressions in VAR, = means we have a
polynomial in VAR, and > means coefficients cannot contain VAR, which
is also the default for anything that is not a multivariate
polynomial."
  `(let ((,g!var% (var ,g!mpoly)))
    (cond ((not (mpoly-p ,g!mpoly)) ,not-mpoly)
          ((var= ,g!var% ,g!var) ,=)
          ((var< ,g!var% ,g!var) ,<)
          (t ,>))))


;; TODO what about degree of zero poly?
(defun mdegree (mp var)
  (mpoly-cases (mp var)
               0
               (1- (length (coefficients mp)))
               (maximise (coefficients mp) :key (lambda (x) (mdegree x var)))))

(defmethod generic-+ ((a mpolynomial) (b mpolynomial))
  (mpoly-cases (a (var b))
               (poly+constant a b 'mpolynomial)
               (poly+poly a b 'mpolynomial)
               (poly+constant b a 'mpolynomial)))



(defmethod generic-* ((a mpolynomial) (b mpolynomial))
  (mpoly-cases (a (var b))
               (poly*constant a b)
               (poly*poly a b 'mpolynomial)
               (poly*constant b a)))

(defmethod generic-* ((poly-b mpolynomial) (int integer))
  (poly*constant poly-b int 'mpolynomial))

(defmethod generic-* ((int integer) (poly-b mpolynomial))
  (poly*constant poly-b int 'mpolynomial))

;;; rational functions built from polynomials
(defclass fraction ()
  ((numer :initarg :numer
         :initform 0
         :accessor numer)
   (denom :initarg :denom
         :initform 1
         :accessor denom))
  (:documentation "A fraction can hold any objects, and describes the
  formal quotient. The domain, over which we consider fraction, should
  not matter much, but it must provide some canonical canceling
  strategry, like the euclidean algorithm."))

;;; in theory, this construction should be completely generic.

(defmethod degree ((fraction fraction))
  (- (degree numer) (degree denom)))

(defmethod mdegree ((fraction fraction) var)
   (- (mdegree numer var) (mdegree denom var)))

(defmethod generic-= ((a fraction) (b fraction))
  (gm:= (gm:* (numer a) (denom b))
        (gm:* (numer b) (denom a))))

(defmethod zero-p ((a fraction))
  (zero-p (numer a)))

(defmethod one-p ((a fraction))
  (gm:= (numer a) (denom a)))


(defmethod generic-+ ((a fraction) (b fraction))
  (simplify
   (make-instance 'fraction
                  :numer (gm:+ (gm:* (numer a) (denom b)) (gm:* (numer b) (denom a)))
                  :denom (gm:* (denom a) (denom b)))))

(defmethod generic-- ((a fraction) (b fraction))
  (simplify
   (if (zero-p a)
       (make-instance 'fraction
                      :numer (gm:- (numer b))
                      :denom (denom b))
       (make-instance 'fraction
                      :numer (gm:- (gm:* (numer a) (denom b)) (gm:* (numer b) (denom a)))
                      :denom (gm:* (denom a) (denom b))))))


(defmethod generic-*  ((a fraction) (b fraction))
  (simplify
   (make-instance 'fraction
                  :numer (gm:* (numer a) (numer b))
                  :denom (gm:* (denom a) (denom b)))))

(defmethod simplify ((a fraction) &key)
  (with-slots ((n numer) (d denom)) a
    ;; first simplify both parts
    (simplify (numer a))
    (simplify (denom a))
    ;; then divide by gcd
    (let ((g (gcd n d)))                ; use a custom gcd function name
      (setf n (gm:/ n g)
            d (gm:/ d g))))
  a)

(defmethod generic-/ ((a fraction) (b fraction))
  (when (zero-p b)
    (error "Cannot divide by 0."))
  (simplify 
   (make-instance 'fraction
                  :numer (gm:* (numer a) (denom b))
                  :denom (gm:* (numer b) (denom a)))))
