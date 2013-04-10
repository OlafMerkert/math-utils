(defpackage :linear-algebra/elementary-matrices
  (:shadowing-import-from :linear-algebra/vectors #:vector #:fill-array)
  (:import-from :generic-math :->)
  (:use :cl :ol :iterate :linear-algebra/vectors)
  (:export
   #:elementary-matrix
   #:determinant
   #:dimension
   #:transposition-matrix
   #:i
   #:j
   #:single-diagonal-matrix
   #:factor
   #:add-row/col-matrix
   #:make-transposition-matrix
   #:make-single-diagonal-matrix
   #:make-add-row/col-matrix
   #:inverse))

(in-package :linear-algebra/elementary-matrices)

(defgeneric determinant (matrix)
  (:documentation "compute the determinant of the given matrix, to
  check whether it is regular."))

(defgeneric inverse (matrix)
  (:documentation "compute the inverse of the given matrix."))

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
    (decf (j matrix)))
  (when (= (i matrix) (j matrix))
    (error "For a proper transposition matrix, you must use two
    different indices.")))

(defun make-transposition-matrix (i j &optional (dimension :automatic) human)
  (make-instance 'transposition-matrix :dimension dimension :i i :j j :human human))

(defmethod determinant ((matrix transposition-matrix)) -1)

(defmethod inverse ((matrix transposition-matrix))
  "A transposition matrix has order 2."
  matrix)

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

(defun make-single-diagonal-matrix (i factor &optional (dimension :automatic) human)
  (make-instance 'single-diagonal-matrix
                 :dimension dimension :i i :factor factor :human human))

(defmethod determinant ((matrix single-diagonal-matrix))
  (factor matrix))

(defmethod inverse ((matrix single-diagonal-matrix))
  (with-slots (dimension i factor) matrix
    (if (gm:zero-p factor)
        (error "Cannot invert diagonal matrix with 0 on diagonal ")
        (make-single-diagonal-matrix i (gm:/ factor) dimension))))


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

(defclass add-row/col-matrix (elementary-matrix)
  ((factor :initarg :factor
           :initform 0
           :accessor factor)
   (i :initarg :i
      :initform 0
      :accessor i)
   (j :initarg :j
      :initform 1
      :accessor j))
  (:documentation "A square matrix representing: Add FACTOR times
  row/col J to row/col I. If I = J, an error is signalled."))

(defmethod initialize-instance :after ((matrix add-row/col-matrix) &key human)
  (when human
    (decf (i matrix))
    (decf (j matrix)))
  (when (= (i matrix) (j matrix))
    (error "The factor of an add-row/col-matrix must not lie on the
    diagonal. Use the single-diagonal-matrix instead.")))

(defun make-add-row/col-matrix (i j factor &optional (dimension :automatic) human)
  (make-instance 'add-row/col-matrix :dimension dimension :i i :j j :factor factor :human human))

(defmethod determinant ((matrix add-row/col-matrix)) 1)

(defmethod inverse ((matrix add-row/col-matrix))
  (with-slots (dimension i j factor) matrix
    (make-add-row/col-matrix i j (gm:- factor) dimension)))

(gm:define->-method/custom (matrix add-row/col-matrix)
  (with-slots (i j factor) add-row/col-matrix
    ;; TODO alert if dimension is automatic
    (make-matrix (:list (dimensions add-row/col-matrix))
        (r s)
      (cond ((= r s) 1)
            ((and (= r i) (= s j)) factor)
            (t 0)))))

(defmethod gm:generic-* ((add-row-matrix add-row/col-matrix) (matrix matrix))
  (let ((entries (entries matrix)))
    (with-slots (i j factor) add-row-matrix
      (make-matrix (:list (dimensions matrix))
          (r &rest indices)
        (if (= r i)
            (gm:+ (apply #'aref entries r indices)
                  (gm:* factor (apply #'aref entries j indices)))
            (apply #'aref entries r indices))))))

(defmethod gm:generic-* ((matrix matrix) (add-col-matrix add-row/col-matrix))
  (let ((entries (entries matrix)))
    (with-slots (i j factor) add-col-matrix
      (make-matrix (:list (dimensions matrix))
          (&rest indices)
        (multiple-value-bind (indices% s) (split-last indices)
         (if (= s j)
             (gm:+ (apply #'aref entries indices)
                   (gm:* factor (apply #'aref entries (append1 indices% i))))
             (apply #'aref entries indices)))))))

;;; TODO unimodular matrix
;;; TODO think about a sophisticated conversion system for matrices
;;; (could be very useful)
;;; TODO checks whether row/col operations can work out

;;; TODO what about mref -- should share code with ->matrix
