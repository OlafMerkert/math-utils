(defpackage :linear-algebra/vectors
  (:nicknames :vectors)
  (:shadow :vector :fill-array)
  (:use :cl :ol :iterate)
  (:export
   :entries
   :vector
   :dimensions
   :mref
   :mref/human
   :indices
   :this
   :define-index-transform
   :with-indices
   :transpose
   :subrow
   :subcol
   :droprow
   :dropcol
   :droprowcol
   :rotaterow
   :rotatecol
   :matrix
   :*create-matrix*
   :make-vector
   :with-vector-type
   :make-matrix
   :make-matrix/human
   :make-matrix%))

(in-package :linear-algebra/vectors)

(defclass vector ()
  ((entries :initarg :entries
                 :accessor entries))
  (:documentation "Model a vector representated by a multi-dimensional array."))

(defgeneric dimensions (vector))

(defmethod dimensions ((vector vector))
  (array-dimensions (entries vector)))

(defun mref (vector-or-matrix &rest indices)
  "access vector or matrix entreis, where indexing start with 0."
  (apply #'aref (entries vector-or-matrix) indices))

(defun set-mref (vector-or-matrix &rest indices+value)
  "setf method for MREF."
  (multiple-value-bind (indices value) (split-last indices+value)
    (setf (apply #'aref (entries vector-or-matrix) indices)
          value)))

(defsetf mref set-mref)

(defun mref/human (vector-or-matrix &rest indices)
  "access vector or matrix entries, where indexing starts with 1."
  (apply #'aref (entries vector-or-matrix)
         (mapcar #'1- indices)))

(defun set-mref/human (vector-or-matrix &rest indices+value)
  "setf method for MMREF."
  (multiple-value-bind (indices value) (split-last indices+value)
    (setf (apply #'aref (entries vector-or-matrix)
                 (mapcar #'1- indices))
          value)))

(defsetf mref/human set-mref/human)

(defsymconstant +unfilled+
    "a symbolic constant to mark unfilled entries in vectors.")

(defun matrix-p (matrix)
  (typep matrix 'matrix))

(defparameter *create-matrix* nil)

(defmacro with-vector-type ((type) &body body)
  `(let ((*create-matrix* ',type))
     ,@body))

(defmacro! define-matrix-variant (fn-vector fn-matrix)
  "Define an alias for a function where MAKE-VECTOR produces a matrix
instead of a vector."
  `(defun ,fn-matrix (&rest ,g!args)
     (with-vector-type (matrix)
       (apply #',fn-vector ,g!args))))

(defun make-vector% (dimensions fill-function &optional vector-type)
  "create a new array with given dimensions and use fill-function to
  calculate the entries."
  (declare (inline fill-array))
  (let ((coeff (make-array dimensions :initial-element +unfilled+)))
    (fill-array coeff fill-function dimensions)
    (make-instance (or vector-type *create-matrix* 'vector) :entries coeff)))

(defun make-matrix% (dimensions fill-function)
  "as make-vector%, but return a matrix instead."
  (make-vector% dimensions fill-function 'matrix))

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

(ew
  (defun make-vector/general (human dimensions index-vars fill-form &optional vector-type)
    "Convenience macro for building vectors, indexing starts with 0 if
human is nil, otherwise with 1. This affects autogenerated index-vars.
The array to be filled can be referenced by this in fill form, but do
not rely on filling order. Autogeneration of index-vars happens for
nil, then you get i0, i1, i2 etc and for t, then you get &rest
indices."
    (unless index-vars
      (setf index-vars (mapcar (lambda (x) (symb 'i (if human (+ 1 x) x)))
                               (lrange dimensions))))
    (when (eq index-vars t)
      (setf index-vars '(&rest indices)))
    (unless (or (member '&rest index-vars)
                (member :list  dimensions)
                (= (length dimensions) (length index-vars)))
      (error "Mismatching dimensions and indices ~A!" index-vars))
    `(make-vector% ,(if (eq (first dimensions) :list)
                        (second dimensions)
                        `(list ,@dimensions))
                   (lambda (this ,@index-vars)
                     (declare (ignorable this ,@(remove '&rest index-vars)))
                     ,@(when human (mapcar #`(incf ,a1) index-vars))
                     ,@fill-form)
                   ',vector-type)))

(defmacro make-vector (dimensions index-vars &body fill-form)
  "see make-vector/general for doc (with human = nil)"
  (make-vector/general nil dimensions index-vars fill-form))

(defmacro make-vector/human (dimensions index-vars &body fill-form)
  "see make-vector/general for doc (with human = t)"
  (make-vector/general t dimensions index-vars fill-form))

(defmacro make-matrix (dimensions index-vars &body fill-form)
  "as make-vector, but return a matrix."
  (make-vector/general nil dimensions index-vars fill-form 'matrix))

(defmacro make-matrix/human (dimensions index-vars &body fill-form)
  "as make-vector/human, but return a matrix."
  (make-vector/general t dimensions index-vars fill-form 'matrix))

(defun dimensions-compatible-p (&rest vectors)
  "test whether the given vectors have the same dimensions, so we can
elementwise operations."
  (case (length vectors)
    ((0) (error "no vector given for dimensions test."))
    ((1) (dimensions (first vectors)))
    (t   (let ((dimensions-of-vectors
                (mapcar #'dimensions vectors)))
           (when (apply #'equal dimensions-of-vectors)
             (first dimensions-of-vectors))))))

(defmacro elementwise-operation (vectors &body fill-form)
  "create a new vector with same dimensions as vectors (a list of
  symbols referencing actual vectors), where every field is filled
  with fill-form, where every vector symbol stands for the
  corresponding field."
  (let ((coeffs (list->gensyms :entries vectors))
        (vectors-ev (list->gensyms :vectors vectors)))
    ;; first evaluate all the vectors
    `(let ,(mapcar #'list vectors-ev vectors)
       ;; determine whether they are all matrices - in that case,
       ;; return a matrix
       (let ((*create-matrix* (if (and ,@(mapcar #`(matrix-p ,a1) vectors-ev))
                                  'matrix *create-matrix*)))
        (let ,(mapcar #2`(,a1 (entries ,a2)) coeffs vectors-ev)
          (make-vector (:list (dimensions-compatible-p ,@vectors-ev)) t
            (symbol-macrolet
                ,(mapcar #2`(,a2 (apply #'aref ,a1 indices)) coeffs vectors)
              ,@fill-form)))))))

;;; generic operations for vectors

(defmethod gm:generic-+ ((vector-a vector) (vector-b vector))
  (elementwise-operation (vector-a vector-b)
    (gm:+ vector-a vector-b)))

(defmethod gm:generic-- ((vector-a vector) (vector-b vector))
  (elementwise-operation (vector-a vector-b)
    (gm:- vector-a vector-b)))

(defmethod gm:generic-* ((vector-a vector) (vector-b vector))
  (elementwise-operation (vector-a vector-b)
    (gm:* vector-a vector-b)))

(defmethod gm:generic-/ ((vector-a vector) (vector-b vector))
  (elementwise-operation (vector-a vector-b)
    (gm:/ vector-a vector-b)))

(defmacro! define-index-transform (name (&rest args) dim-form &rest index-forms)
  "Define a transformation of a multidim vector V by supplying a
  DIM-FORM to calculate the new dimensions, with the anaphoric
  DIMENSIONS of V; and using INDEX-FORMS to calculate the indices in V
  from the anaphoric INDICES in the new vector.  Use the helper macro WITH-INDICES to destructure INDICES."
  `(defun ,name (,g!vector ,@args)
     (macrolet ((with-indices (dest &body body)
                  `(destructuring-bind ,dest indices ,@body)))
       (let ((dimensions (dimensions ,g!vector))
             (,g!coeffs (entries ,g!vector)))
         (make-vector% ,dim-form
                       (lambda (,g!this indices)
                         (declare (ignore ,g!this))
                         (apply #'aref ,g!coeffs ,@index-forms))
                       (if (matrix-p ,g!vector)
                           'matrix nil))))))

;; Transposition der Matrix
(define-index-transform transpose ()
  (reverse dimensions)
  (reverse indices))

;; Extrahiere Zeile i
(define-index-transform subrow (i)
  (cdr dimensions)
  i indices)

;; Extrahiere Spalte j
(define-index-transform subcol (j)
  (droplast dimensions)
  (append1 indices j))

(defun skip (i j)
  "Entferne j aus der Folge der natuerlichen Zahlen."
  (if (< i j) i (1+ i)))

;; Entferne Zeile i.
(define-index-transform droprow (i)
  (aprog1 (copy-list dimensions)
    (decf (first it)))
  (progn
    (setf (first indices) (skip (first indices) i))
    indices))

;; Entferne Spalte j.
(define-index-transform dropcol (j)
  (aprog1 (copy-list dimensions)
    (decf (last1 it))
    dimensions)
  (progn
    (setf (last1 indices) (skip (last1 indices) j))
    indices))

;; Entferne Zeile i und Spalte j.
(define-index-transform droprowcol (i j)
  (aprog1 (copy-list dimensions)
    (when (length=1 it)
      (error "droprowcol only works with vectors that have at least two dimensions!"))
    (decf (first it))
    (incf (last1 it)))
  (progn
    (setf (first indices) (skip (first indices) i)
          (last1 indices) (skip (last1 indices) j))
    indices))

(defun swap (x i j)
  "Vertausche i und j in der Folge der natuerlichen Zahlen."
  (cond ((= x i) j)
        ((= x j) i)
        (t x)))

;; Tausche Zeilen i und j.
(define-index-transform rotaterow (i j)
  dimensions
  (swap (first indices) i j) (cdr indices))

;; Tausche Spalten i und j.
(define-index-transform rotatecol (i j)
  dimensions
  (progn
    (setf (last1 indices) (swap (last1 indices) i j))
    indices))

;;; Matrixmultiplikation
(defclass matrix (vector)
  ()
  (:documentation "extend the vector class with matrix multiplication."))

(defmethod gm:generic-* ((matrix-a matrix) (matrix-b matrix))
  (multiple-value-bind (dims-a m-a) (split-last (copy-list (dimensions matrix-a)) )
    (destructuring-bind (m-b dims-b) (dimensions matrix-b)
      (unless (= m-a m-b)
        (error "Dimensions of ~A and ~A are not compatible for matrix multiplication." matrix-a matrix-b))
      (let ((split (length dims-a))
            (entries-a (entries matrix-a))
            (entries-b (entries matrix-b)))
        (make-matrix%
         (append dims-a dims-b)
         (ilambda (this &rest indices)
           (let ((ind-a (subseq indices 0 split))
                 (ind-b (subseq indices split)))
             (gm:summing (i 0 m-a t)
                         (gm:+ (apply #'aref entries-a (append1 ind-a i))
                               (apply #'aref entries-b i ind-b))))))))))

;;; TODO sparse matrices
;;; TODO matrix construction utilities
;;; TODO vector -> matrix casting
;;; TODO vector * matrix multiplication

(defun make-diagonal-vector (number &rest dimensions)
  (make-vector (:list dimensions) t
               (if (apply #'= indices) number 0)))

(define-matrix-variant make-diagonal-vector make-diagonal-matrix)

(defun make-full-vector (number &rest dimensions)
  (make-vector (:list dimensions) t number))

(define-matrix-variant make-full-vector make-full-matrix)

(defun zero-vector (&rest dimensions)
  "Create a vector with 0 entries."
  (apply #'make-full-vector 0 dimensions))

(defun zero-matrix (dimension)
  "Create a square matrix with 0 entries."
  (make-full-matrix 0 dimension dimension))

(defun one-vector (&rest dimensions)
  "Create a vector with 1 entries."
  (apply #'make-full-vector 1 dimensions))

(defun one-matrix (dimension)
  "Create a square identity matrix"
  (make-diagonal-matrix 1 dimension dimension))

;; upgrade numbers to vectors or matrices
(gm:define->-method/custom (vector number (dimensions))
  (apply #'make-full-vector number dimensions))

(gm:define->-method/custom (matrix number (dimensions))
  (apply #'make-diagonal-matrix number dimensions))

;; switch between matrices and vectors
(gm:define->-method (matrix vector)
    :entries (entries vector))

(gm:define->-method (vector matrix)
    :entries (entries matrix))

(gm:define->-method/identity vector)
(gm:define->-method/identity matrix)

(defmacro as-matrices  (matrices &body body)
  "ensure that everything in the vars MATRICES are actually matrices and not vectors."
  `(let ,@(mapcar #`(,a1 (gm:-> 'matrix ,a1)) matrices)
     ,@body))

(defmacro as-vectors  (vectors &body body)
  "ensure that everything in the vars VECTORS are actually vectors and not matrices."
  `(let ,@(mapcar #`(,a1 (gm:-> 'vector ,a1)) vectors)
     ,@body))
