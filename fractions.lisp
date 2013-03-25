(defpackage :fractions
  (:shadowing-import-from :generic-math :summing  :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :iterate :generic-math)
  (:export
   #:ggt
   #:fraction
   #:numer
   #:denom))

(in-package :fractions)

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

(defmethod generic-/ ((a fraction) (b fraction))
  (when (zero-p b)
    (error "Cannot divide by 0."))
  (simplify 
   (make-instance 'fraction
                  :numer (gm:* (numer a) (denom b))
                  :denom (gm:* (numer b) (denom a)))))

(defmethod print-object ((fraction fraction) stream)
  (format stream "[~A / ~A]" (numer fraction) (denom fraction)))

;;; use german abbrevation ggt for the greatest common divisor, so we
;;; don't have to bother with name collisions
(defgeneric ggt (a b)
  (:documentation "Compute the greatest common divisor for A, B in
  some factorial (or better euclidean) ring."))

(defmethod ggt (a (b (eql 1)))
  1)

(defmethod ggt ((b (eql 1)) a)
  1)

(defmethod ggt ((a integer) (b integer))
  (gcd a b))

;;; with ggt, we can simplify fractions
(defmethod simplify ((a fraction) &key)
  (with-slots ((n numer) (d denom)) a
    ;; first simplify both parts
    (setf n (simplify n)
          d (simplify d))
    ;; then divide by gcd
    (let ((g (ggt n d)))
      (unless (one-p g)
        (setf n (gm:/ n g)
              d (gm:/ d g)))))
  a)

;;; TODO perhaps allow moving units in fractions (how in canonical way?)
