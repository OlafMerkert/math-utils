(defpackage :infinite-math
  (:nicknames :im)
  (:shadowing-import-from :cl :+ :- :* :/ :expt :sqrt)
  (:shadowing-import-from :gm := :summing)
  (:use :cl :ol :iterate :generic-math)
  (:export :infinity+
   :infinity-
   :infinite-p
   :i<))

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
        ((eq a infinity-) t)
        ((eq b infinity+) t)
        (t (cl:< a b))))

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

(defmethod zero ((a (eql infinity+)))
  0)

(defmethod zero ((a (eql infinity-)))
  0)

(defmethod one ((a (eql infinity+)))
  1)

(defmethod one ((a (eql infinity-)))
  1)
