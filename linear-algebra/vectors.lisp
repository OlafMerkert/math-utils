(defpackage :linear-algebra/vectors
  (:nicknames :vectors)
  (:shadow :vector)
  (:use :cl :ol )
  (:export
   :coefficients
   :vector
   :dimensions
   :mref))

(in-package :linear-algebra/vectors)

(defclass vector ()
  ((coefficients :initarg :coefficients
                 :accessor coefficients))
  (:documentation "doc"))

(defun dimensions (vector)
  (array-dimensions (coefficients vector)))

(defun mref (vector-or-matrix &rest indices)
  "doc"
  (apply #'aref (coefficients vector-or-matrix) indices))

(defun set-mref (vector-or-matrix &rest indices+value)
  "doc"
  (multiple-value-bind (indices value) (split-last indices+value)
    (setf (apply #'aref (coefficients vector-or-matrix) indices)
          value)))

(defun split-last (list)
  "destructively split the last entry from the list. return (values
list last)"
  (if (cdr list)
      ;; first deal with lists of more than 1 element
      (let* ((l (last list 2))
             (e (second l)))
        (setf (cdr l) nil)
        (values list e))
      ;; then the special case of just one or none element.
      (values nil (first list))))

(defsetf mref set-mref)
