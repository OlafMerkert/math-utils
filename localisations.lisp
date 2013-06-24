(defpackage :localisations
  (:shadowing-import-from :fractions :numerator :denominator)
  (:shadowing-import-from :cl :+ :- :* :/ :expt := :sqrt)
  (:shadowing-import-from :ol :^ :_)
  (:use :cl :ol
        :generic-math
        :finite-fields
        :fractions)
  (:export
   :numerator
   :denominator
   :modulus
   :integer-loc
   :loc%
   :at-modulus
   :assert-same-modulus))

(in-package :localisations)

(defclass integer-loc (fraction)
  ((modulus     :initarg :mod
                :reader   modulus))
  (:documentation "An element of the localisation of the integers at a
  modulus p."))

(define-modify-macro divf (&rest args) /)

(defmethod initialize-instance :after ((object integer-loc) &key)
  (with-slots (numerator denominator modulus) object
    ;; check for invalid denominator
    (when (nt:divides-p modulus denominator)
      (error "Inversion error with denominator ~A at modulus ~A"
             denominator modulus))
    ;; simplify
    (let ((d (gcd numerator denominator)))
      (unless (= d 1)
        (divf numerator d)
        (divf denominator d)))))

(defmethod print-object ((object integer-loc) stream)
  (with-slots (numerator denominator modulus) object
    (if (= denominator 1)
        (format stream "[~A at ~A]" numerator modulus)
        (format stream "[~A/~A at ~A]" numerator denominator modulus))))

(defmethod print-object/tex ((object integer-loc) stream)
  (with-slots (numerator denominator) object
    (if (= denominator 1)
        (format stream "~A" numerator)
        (format stream "\\frac{~A}{~A}" numerator denominator))))

(defun loc% (n d m)
  (make-instance 'integer-loc :numerator n :denominator d :mod m))

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
                   :numerator (+ (* (numerator a) (denominator b))
                             (* (numerator b) (denominator a)))
                   :denominator (* (denominator a) (denominator b))
                   :mod p)))

(defmethod generic-- ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numerator (- (* (numerator a) (denominator b))
                             (* (numerator b) (denominator a)))
                   :denominator (* (denominator a) (denominator b))
                   :mod p)))

(Defmethod generic-* ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numerator (* (numerator a)   (numerator b))
                   :denominator (* (denominator a) (denominator b))
                   :mod p)))

(defmethod generic-/ ((a integer-loc) (b integer-loc))
  (assert-same-modulus p (a b)
    (make-instance 'integer-loc
                   :numerator (* (numerator a) (denominator b))
                   :denominator (* (numerator b) (denominator a))
                   :mod p)))

(defmethod -> ((target-type (eql 'integer-loc)) (number rational) &key (mod 2))
  (make-instance 'integer-loc :numerator (cl:numerator number) :denominator (cl:denominator number) :mod mod))

(defmethod -> ((target-type integer-loc) (number rational) &key)
  (-> 'integer-loc number :mod (modulus target-type)))

(defmethod -> ((target-type (eql 'integer-mod)) (number integer-loc) &key)
  (generic-/
   (make-instance 'integer-mod :rem (numerator number)   :mod (modulus number))
   (make-instance 'integer-mod :rem (denominator number) :mod (modulus number))))

(defmethod -> ((target-type integer-mod) (number integer-loc) &key)
  (assert-same-modulus p (target-type number)
    (-> 'integer-mod number)))
