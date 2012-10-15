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

(defmethod generic-= ((a integer-mod) (b integer-mod))
  (and (= (modulus a) (modulus b))
       (= (remainder a) (remainder b))))


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
    (multiple-value-bind (d u v) (nt:xgcd (remainder b) p)
      (declare (ignore v))
      (when (/= d 1)
        (error "Cannot invert ~A." b))
      (make-instance 'integer-mod
                    :rem (mod (* (remainder a)
                                 u)
                              p)
                    :mod p))))

(defmethod zero ((a integer-mod))
  (make-instance 'integer-mod :rem 0 :mod (modulus a)))

(defmethod one ((a integer-mod))
  (make-instance 'integer-mod :rem 1 :mod (modulus a)))

(defmethod -> ((target-type (eql 'integer-mod)) (number integer) &key (mod 2))
  (make-instance 'integer-mod :rem number :mod mod))

(defmethod -> ((target-type integer-mod) (number integer) &key)
  (-> 'integer-mod number :mod (modulus target-type)))

;; TODO Quadratwurzeln in endlichen Körpern // besserer Algorithmus
(defmethod gm:sqrt ((a integer-mod))
  (with-slots ((r remainder) (p modulus)) a
    ;; for now, just do brute force.  if a root exists, its square can
    ;; be assumed to be less than p^2
    (or (int% (iter (for i from 0 below p)
                    (finding i such-that (zerop (mod (- (* i i) r) p))))
              p)
        (error "~A has no square root mod ~A" r p))))

(create-binary->-wrappers integer-mod integer
    (:mod (modulus integer-mod)) (:left :right)
  generic-+
  generic--
  generic-*
  generic-/)

(defmethod gm:expt ((base integer-mod) (power integer))
  (make-instance 'integer-mod
                 :rem (cl-utilities:expt-mod (remainder base)
                                             power
                                             (modulus base))
                 :mod (modulus base)))
