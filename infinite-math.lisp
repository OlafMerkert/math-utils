(defpackage :infinite-math
  (:nicknames :im)
  (:shadowing-import-from :cl :+ :- :* :/ :expt :sqrt)
  (:shadowing-import-from :ol :^ :_)
  (:shadowing-import-from :gm :=)
  (:use :cl :ol :iterate :generic-math)
  (:export
   #:infinity+
   #:infinity-
   #:infinite-p
   #:i<
   #:i<=
   #:i>=
   #:i>
   #:imax
   #:imin))

(in-package :infinite-math)

;;; infinite (real) numbers
(defconstant infinity+ :infinity+)
(defconstant infinity- :infinity-)

(declaim (inline infinite-p))
(defun infinite-p (x)
  (or (eq x infinity+)
      (eq x infinity-)))

(defun i< (a b)
  "comparing numbers and infinite numbers."
  (cond ((eq a infinity+) nil)
        ((eq b infinity-) nil)
        ((eq a infinity-) (not (eq b infinity-)))
        ((eq b infinity+) (not (eq a infinity+)))
        (t (cl:< a b))))

(defun i<= (a b)
  "comparing numbers and infinite numbers."
  (cond ((eq a infinity+) (eq b infinity+))
        ((eq b infinity-) (eq a infinity-))
        ((eq a infinity-) t)
        ((eq b infinity+) t)
        (t (cl:<= a b))))

(declaim (inline i> i>=))

(defun i> (a b) (i< b a))
(defun i>= (a b) (i<= b a))

(defun imin (a b)
  "minimal value between `a' and `b'."
  (cond ((or (eq a infinity-)
             (eq b infinity-)) infinity-)
        ((eq a infinity+) b)
        ((eq b infinity+) a)
        (t (min a b))))

(defun imax (a b)
  "maximal value between `a' and `b'."
  (cond ((or (eq a infinity+)
             (eq b infinity+)) infinity+)
        ((eq a infinity-) b)
        ((eq b infinity-) a)
        (t (max a b))))


(defmethod one-p ((number (eql infinity+)))
  nil)

(defmethod one-p ((number (eql infinity-)))
  nil)

(defmethod zero-p ((number (eql infinity+)))
  nil)

(defmethod zero-p ((number (eql infinity-)))
  nil)

(defmethod minus-p ((number (eql infinity+)))
  nil)

(defmethod minus-p ((number (eql infinity-)))
  t)

(defmethod generic-= ((a (eql infinity+)) (b (eql infinity+)))
  t)

(defmethod generic-= ((a (eql infinity-)) (b (eql infinity-)))
  t)

;;; todo perhaps some arithmetic? might however be too dangerous

;; polynomial formatting requires additive inverses (because we return
;; t in (minus-p infinity-)
(defmethod generic-- (a (b (eql infinity+)))
  (unless (zero-p a)
    (error "substracting infinity from anything but 0 is not well defined."))
  infinity-)

(defmethod generic-- (a (b (eql infinity-)))
  (unless (zero-p a)
    (error "substracting infinity from anything but 0 is not well defined."))
  infinity+)
