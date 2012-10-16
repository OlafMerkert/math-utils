(defpackage :localisations
  (:shadow :numerator :denominator)
  (:shadowing-import-from :cl :+ :- :* :/ :expt := :sqrt)
  (:use :cl :ol
        :generic-math
        :finite-fields)
  (:export
   :numerator
   :denominator
   :modulus
   :integer-loc))

(in-package :localisations)

(defclass integer-loc ()
  ((numerator   :initarg :numer
                :reader   numerator)
   (denominator :initarg :denom
                :initform 1
                :reader   denominator)
   (modulus     :initarg :mod
                :reader   modulus))
  (:documentation "An element of the localisation of the integers at a
  modulus p."))

(define-modify-macro divf (&rest args) cl:/)

(defmethod initialize-instance :after ((object integer-loc) &key)
  (with-slots (numerator denominator modulus) object
    ;; check for invalid denominator
    (when (number-theory/primes:divides-p modulus denominator)
      (error "Inversion error with denominator ~A at modulus ~A"
             denominator modulus))
    ;; simplify
    (let ((d (cl:gcd numerator denominator)))
      (unless (cl:= d 1)
        (divf numerator d)
        (divf denominator d)))))

(defmethod print-object ((object integer-loc) stream)
  (with-slots (numerator denominator modulus) object
    (if (cl:= denominator 1)
        (format stream "[~A at ~A]" numerator modulus)
        (format stream "[~A/~A at ~A]" numerator denominator modulus))))

(defmethod print-object/tex ((object integer-loc) stream)
  (with-slots (numerator denominator) object
    (if (cl:= denominator 1)
        (format stream "~A" numerator)
        (format stream "\\frac{~A}{~A}" numerator denominator))))

(defun loc% (n d m)
  (make-instance 'integer-loc :numer n :denom d :mod m))

(defmacro at-modulus ((p) &body body)
  "Consider all verbatim integers and rationals as being at p."
  ;; TODO  whether p is actually prime
  `(progn ,@(map-tree-if
             #'numberp
             (lambda (number)
               (typecase number
                 (integer `(loc% ,number 1 ,p))
                 (rational `(loc% ,(cl:numerator number)
                                  ,(cl:denominator number)
                                  ,p))
                 (t number)))
             body)))

(defmethod simplify ((a integer-loc) &key)
  ;; simplification should occur at init time
  a)

(defmethod generic-= ((a integer-loc) (b integer-loc))
  (and (= (modulus a) (modulus b))
       (= (* (numerator a) (denominator b))
          (* (numerator b) (denominator a)))))

(defmethod generic-+ ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numer (+ (* (numerator a) (denominator b))
                             (* (numerator b) (denominator a)))
                   :denom (* (denominator a) (denominator b))
                   :mod p)))

(defmethod generic-- ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numer (- (* (numerator a) (denominator b))
                             (* (numerator b) (denominator a)))
                   :denom (* (denominator a) (denominator b))
                   :mod p)))

(defmethod generic-* ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numer (* (numerator a)   (numerator b))
                   :denom (* (denominator a) (denominator b))
                   :mod p)))

(defmethod generic-/ ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numer (* (numerator a) (denominator b))
                   :denom (* (numerator b) (denominator a))
                   :mod p)))

(defmethod zero ((a integer-loc))
  (make-instance 'integer-loc :numer 0 :mod (modulus a)))

(defmethod one ((a integer-loc))
  (make-instance 'integer-loc :numer 1 :mod (modulus a)))

(defmethod -> ((target-type (eql 'integer-loc)) (number rational) &key (mod 2))
  (make-instance 'integer-loc :numer (cl:numerator number) :denom (cl:denominator number) :mod mod))

(defmethod -> ((target-type integer-loc) (number rational) &key)
  (-> 'integer-loc number :mod (modulus target-type)))

(defmethod -> ((target-type (eql 'integer-mod)) (number integer-loc) &key)
  (generic-/
   (make-instance 'integer-mod :rem (numerator number)   :mod (modulus number))
   (make-instance 'integer-mod :rem (denominator number) :mod (modulus number))))

(defmethod -> ((target-type integer-mod) (number integer-loc) &key)
  (assert-same-modulus p (target-type number)
    (-> 'integer-mod number)))
