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

(defun make-vector% (dimensions fill-function)
  "create a new array with given dimensions and use fill-function to
  calculate the entries."
  (declare (inline fill-array))
  (let ((coeff (make-array dimensions :initial-element nil)))
    (fill-array coeff fill-function dimensions)))

(defun fill-array (array fill-function &optional (positions nil positions?))
  "fill the ARRAY with the FILL-FUNCTION in the places given by
POSITIONS. POSITIONS is a list of index ranges, where an index range
is either a tuple (start end) with inclusive start and exclusive end,
or simply an integer end, equivalent to (0 end). FILL-FUNCTION will be
called in the same way as aref--first argument is the array, the
remaining are the indices. "
  (unless positions?
    (setf positions (array-dimensions array)))
  ;; just reverse the positions once here, so we don't have to reverse
  ;; the index-lists all the time.
  (setf positions (reverse positions))
  (labels ((index-range (position)
             ;; normalise the range information
             (if (listp position)
                 (values (first position) (second position))
                 (values 0 position)))
           (rec (positions indices)
             (if positions
                 ;; more ranges to iterate over
                 (multiple-value-bind (start end) (index-range (first positions))
                   (dotimes+ (i start end)
                       ((rest (rest positions)))
                     (rec rest
                          (cons i indices))))
                 ;; all index information available
                 (setf (apply #'aref        array indices)
                       (apply fill-function array indices)))))
    (rec positions nil)
    ;; return the now filled array
    array))

