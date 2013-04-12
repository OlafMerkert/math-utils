(defpackage :finite-fields
  (:shadowing-import-from :cl :+ :- :* :/ :expt := :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :generic-math
        :iterate)
  (:export
   :with-modulus
   :int%
   :integer-mod
   :remainder
   :modulus
   :assert-same-modulus))

(in-package :finite-fields)

;;; calculate mod p in the integer numbers
(defclass integer-mod ()
  ((remainder :initarg :rem
              :reader   remainder)
   (modulus   :initarg :mod
              :reader   modulus))
  (:documentation "A number"))

(defmethod initialize-instance :after ((object integer-mod) &key)
  (with-slots (remainder modulus) object
    (setf remainder (mod remainder modulus))))

(defmethod print-object ((object integer-mod) stream)
  (with-slots (remainder modulus) object
    (format stream "[~A mod ~A]" remainder modulus)))

(defmethod print-object/tex ((object integer-mod) stream)
  (with-slots (remainder modulus) object
    ;; modulus has to be tracked manually
    (format stream "~A" remainder)))

(defun int% (r m)
  (make-instance 'integer-mod :rem r :mod m))

(defmacro with-modulus ((p) &body body)
  "Consider all verbatim integers appearing in body as being mod p."
  ;; TODO test whether p is actually prime
  `(progn ,@(map-tree-if #'integerp #`(int% ,a1 ,p) body)))

(defmacro assert-same-modulus (modulus numbers &body body)
  `(let ((,modulus (modulus ,(first numbers))))
     (unless (= ,modulus ,@(mapcar #`(modulus ,a1) (rest numbers)))
       (error "Expected elements of ~A to have same modulus." (list ,@numbers)))
     ,@body))

;; choose a unique representation
(defmethod simplify ((a integer-mod) &key)
  ;; simplification happens at init time
  a)

(defmethod simplified-p ((integer-mod integer-mod)) t)

(defmethod generic-= ((a integer-mod) (b integer-mod))
  (and (= (modulus a) (modulus b))
       (= (remainder a) (remainder b))))

(defmethod zero-p ((a integer-mod))
  (zerop (remainder a)))

(defmethod one-p ((a integer-mod))
  (cl:= 1 (remainder a)))

(defmethod generic-+ ((a integer-mod) (b integer-mod))
  (assert-same-modulus p (a b)
    (make-instance 'integer-mod
                   :rem (mod (+ (remainder a)
                                (remainder b))
                             p)
                   :mod p)))

(defmethod generic-- ((a integer-mod) (b integer-mod))
  (assert-same-modulus p (a b)
    (make-instance 'integer-mod
                   :rem (mod (- (remainder a)
                                (remainder b))
                             p)
                   :mod p)))

(defmethod generic-* ((a integer-mod) (b integer-mod))
  (assert-same-modulus p (a b)
    (make-instance 'integer-mod
                   :rem (mod (* (remainder a)
                                (remainder b))
                             p)
                   :mod p)))

(defmethod generic-/ ((a integer-mod) (b integer-mod))
  (assert-same-modulus p (a b)
    ;; first remove the gcd from A and B, so this may work in case p
    ;; is not prime
    (let* ((a (remainder a))
           (b (remainder b))
           (d0 (gcd a b)))
      (setf a (cl:/ a d0)
            b (cl:/ b d0))
      (if (cl:= b 1)
          (make-instance 'integer-mod :rem a :mod p)
          (multiple-value-bind (d u v) (nt:xgcd b p)
            (declare (ignore v))
            (when (/= d 1)
              (error "Cannot invert ~A." b))
            (make-instance 'integer-mod
                           :rem (mod (* a u) p)
                           :mod p))))))

;; TODO Quadratwurzeln in endlichen Körpern // besserer Algorithmus
(defmethod gm:sqrt ((a integer-mod))
  (with-slots ((r remainder) (p modulus)) a
    ;; for now, just do brute force.  if a root exists, its square can
    ;; be assumed to be less than p^2
    (or (int% (iter (for i from 0 below p)
                    (finding i such-that (zerop (mod (- (* i i) r) p))))
              p)
        (error "~A has no square root mod ~A" r p))))

(define->-method (integer-mod integer (:mod modulus 2))
    :rem integer)

(default-simple-type-conversion integer integer-mod)

(defmethod gm:expt ((base integer-mod) (power integer))
  (make-instance 'integer-mod
                 :rem (cl-utilities:expt-mod (remainder base)
                                             power
                                             (modulus base))
                 :mod (modulus base)))
