(defpackage :linear-algebra/elementary-matrices
  (:shadowing-import-from :linear-algebra/vectors #:vector #:fill-array)
  (:import-from :generic-math :->)
  (:use :cl :ol :iterate :linear-algebra/vectors)
  (:export))

(in-package :linear-algebra/elementary-matrices)

(defclass elementary-matrix ()
  ((dimension :initarg :dimension
              :initform :automatic
              :accessor dimension))
  (:documentation "Model a square matrix representing particular
generators of GL(2,k)."))

(defmethod generic-* ((a elementary-matrix) (b elementary-matrix))
  (generic-* a (-> 'matrix b)))

;;; it might be convenient not to specify the size of the matrix.
(defmethod dimensions ((matrix elementary-matrix))
  (with-slots (dimension) matrix
    (list dimension dimension)))

(defclass transposition-matrix (elementary-matrix)
  ((i :initarg :i
      :initform 0
      :accessor i)
   (j :initarg :j
      :initform 1
      :accessor j))
  (:documentation "A matrix that interchanges rows (or columns) upon
  multiplication. Internal indexing starts with 0, but an additional
  parameter HUMAN to make-instance converts from indexing starting
  with 1."))

(defmethod initialize-instance :after ((matrix transposition-matrix) &key human)
  (when human
    (decf (i matrix))
    (decf (j matrix))))

;;; render the transposition matrix
(gm:define->-method/custom (matrix transposition-matrix)
  (with-slots (i j) transposition-matrix
    ;; TODO alert if dimension is automatic
    (make-matrix (:list (dimensions transposition-matrix))
        (r s)
      (cond ((or (= i r s) (= j r s)) 0)
            ((= r s) 1)
            ((and (= i r) (= j s)) 1)
            ((and (= i s) (= j r)) 1)
            (t 0)))))

(defmethod gm:generic-* ((transposition-matrix transposition-matrix) (matrix matrix))
  (with-vector-type (matrix)
    (rotaterow matrix (i transposition-matrix) (j transposition-matrix))))

(defmethod gm:generic-* ((matrix matrix) (transposition-matrix transposition-matrix))
  (with-vector-type (matrix)
    (rotatecol matrix (i transposition-matrix) (j transposition-matrix))))

(defclass single-diagonal-matrix (elementary-matrix)
  ((i :initarg :i
      :initform 0
      :accessor i)
   (factor :initarg :factor
           :initform 1
           :accessor factor))
  (:documentation "A square matrix with 1 on the diagonal, except at
  index I, where we have FACTOR. Indexing from 0, unless :HUMAN T is
  given to make-instance."))

(defmethod initialize-instance :after ((matrix single-diagonal-matrix) &key human)
  (when human
    (decf (i matrix))))

(gm:define->-method/custom (matrix single-diagonal-matrix)
  (with-slots (i factor) single-diagonal-matrix
    ;; TODO alert if dimension is automatic
    (make-matrix (:list (dimensions single-diagonal-matrix))
        (r s)
      (cond ((= i r s) factor)
            ((= r s) 1)
            (t 0)))))

(defmethod gm:generic-* ((single-diagonal-matrix single-diagonal-matrix) (matrix matrix))
  "Multiply row I with factor."
  (let ((entries (entries matrix)))
    (with-slots (i factor) single-diagonal-matrix
     (make-matrix (:list (dimensions matrix))
         (&rest indices)
       (if (= (first indices) i)
           (gm:* factor (apply #'aref entries indices))
           (apply #'aref entries indices))))))

(defmethod gm:generic-* ((matrix matrix) (single-diagonal-matrix single-diagonal-matrix))
  "Multiply col I with factor."
  (let ((entries (entries matrix)))
    (with-slots (i factor) single-diagonal-matrix
     (make-matrix (:list (dimensions matrix))
         (&rest indices)
       (if (= (last1 indices) i)
           (gm:* factor (apply #'aref entries indices))
           (apply #'aref entries indices))))))


;;; TODO unimodular matrix
