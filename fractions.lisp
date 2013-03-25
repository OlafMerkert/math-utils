(defpackage :fractions
  (:shadow :numerator :denominator)
  (:shadowing-import-from :generic-math :summing  :+ :- :* :/ := :expt :sqrt)
  (:use :cl :ol :iterate :generic-math)
  (:export
   #:ggt
   #:fraction
   #:numerator
   #:denominator
   #:frac))

(in-package :fractions)

(defclass fraction ()
  ((numerator :initarg :numerator
              :initform 0
              :accessor numerator)
   (denominator :initarg :denominator
                :initform 1
                :accessor denominator))
  (:documentation "A fraction can hold any objects, and describes the
  formal quotient. The domain, over which we consider fraction, should
  not matter much, but it must provide some canonical canceling
  strategy, like the euclidean algorithm."))

(defun frac (numerator &optional (denominator 1))
  "Constructor abbreviation for general fractions."
  (make-instance 'fraction :numerator numerator :denominator denominator))

(defmethod generic-= ((a fraction) (b fraction))
  (gm:= (gm:* (numerator a) (denominator b))
        (gm:* (numerator b) (denominator a))))

(defmethod zero-p ((a fraction))
  (zero-p (numerator a)))

(defmethod one-p ((a fraction))
  (gm:= (numerator a) (denominator a)))

(defmethod generic-+ ((a fraction) (b fraction))
  (simplify
   (make-instance 'fraction
                  :numerator (gm:+ (gm:* (numerator a) (denominator b)) (gm:* (numerator b) (denominator a)))
                  :denominator (gm:* (denominator a) (denominator b)))))

(defmethod generic-- ((a fraction) (b fraction))
  (simplify
   (if (zero-p a)
       (make-instance 'fraction
                      :numerator (gm:- (numerator b))
                      :denominator (denominator b))
       (make-instance 'fraction
                      :numerator (gm:- (gm:* (numerator a) (denominator b)) (gm:* (numerator b) (denominator a)))
                      :denominator (gm:* (denominator a) (denominator b))))))


(defmethod generic-*  ((a fraction) (b fraction))
  (simplify
   (make-instance 'fraction
                  :numerator (gm:* (numerator a) (numerator b))
                  :denominator (gm:* (denominator a) (denominator b)))))

(defmethod generic-/ ((a fraction) (b fraction))
  (when (zero-p b)
    (error "Cannot divide by 0."))
  (simplify 
   (make-instance 'fraction
                  :numerator (gm:* (numerator a) (denominator b))
                  :denominator (gm:* (numerator b) (denominator a)))))

(defmethod print-object ((fraction fraction) stream)
  (format stream "[~A / ~A]" (numerator fraction) (denominator fraction)))

;;; use german abbrevation ggt for the greatest common divisor, so we
;;; don't have to bother with name collisions
(defgeneric ggt (a b)
  (:documentation "Compute the greatest common divisor for A, B in
  some unique factorisation domain (or better euclidean domain)."))

(defmethod ggt (a (b (eql 1)))
  1)

(defmethod ggt ((b (eql 1)) a)
  1)

(defmethod ggt ((a integer) (b integer))
  (gcd a b))

;;; with ggt, we can simplify fractions
(defmethod simplify ((a fraction) &key)
  (with-slots ((n numerator) (d denominator)) a
    ;; first simplify both parts
    (setf n (simplify n)
          d (simplify d))
    ;; then divide by gcd
    (let ((g (ggt n d)))
      (unless (one-p g)
        (setf n (gm:/ n g)
              d (gm:/ d g)))))
  a)


(defmethod simplified-p ((a fraction))
  ;; TODO do we want to test something here?
  t)

;;; TODO perhaps allow moving units in fractions (how in canonical way?)

;;; more important functions from generic-math
(defmethod gm:sqrt ((fraction fraction))
  (multiple-value-bind (num-r num-nice) (sqrt (numerator fraction))
    (multiple-value-bind (den-r den-nice) (sqrt (denominator fraction))
      (values (frac num-r den-r)
              (and num-nice den-nice)))))

;;; coerce rationals into fractions
(defmethod -> ((target-type (eql 'fraction)) (rational rational) &key)
  (frac (cl:numerator rational) (cl:denominator rational)))

(defmethod -> ((target-type fraction) (rational rational) &key)
  (frac (cl:numerator rational) (cl:denominator rational)))

;; TODO create a helper macro that automatically takes care of
;; generating both -> methods (including parameters), and similar
;; stuff for one and zero methods

(create-binary->-wrappers fraction rational
    () (:left :right)
  generic-+
  generic--
  generic-*
  generic-/)

