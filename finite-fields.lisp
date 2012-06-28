(defpackage :finite-fields
  (:shadowing-import-from :cl :+ :- :* :/ :expt :=)
  (:use :cl :ol :generic-math))

(in-package :finite-fields)

;;; calculate mod p in the integer numbers
(defclass integer-mod ()
  ((remainder :initarg :rem
              :reader  remainder)
   (modulus   :initarg :mod
              :reader  modulus))
  (:documentation "A number"))

(create-standard-print-object integer-mod (remainder "mod" modulus))

(defun int% (r m)
  (make-instance 'integer-mod :rem r :mod m))

(defmacro assert-same-modulus (modulus numbers &body body)
  `(let ((,modulus (modulus ,(first numbers))))
     (unless (= ,modulus ,@(mapcar #`(modulus ,a1) (rest numbers)))
       (error "Expected elements of ~A to have same modulus." (list ,@numbers)))
     ,@body))

;; choose a unique representation
(defmethod simplify ((a integer-mod))
  (with-slots (remainder modulus) a
    (setf remainder (mod remainder modulus))
    a))

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

(defmethod +-unit ((a integer-mod))
  (make-instance 'integer-mod :rem 0 :mod (modulus a)))

(defmethod *-unit ((a integer-mod))
  (make-instance 'integer-mod :rem 1 :mod (modulus a)))
