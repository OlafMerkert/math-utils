(defpackage :linear-algebra/vectors
  (:nicknames :vectors)
  (:shadow :vector)
  (:use :cl :ol :iterate)
  (:export
   #:entries
   #:vector
   #:dimensions
   #:mref
   #:mref/human
   #:indices
   #:this
   #:define-index-transform
   #:with-indices
   #:transpose
   #:subrow
   #:subcol
   #:droprow
   #:dropcol
   #:droprowcol
   #:rotaterow
   #:rotatecol
   #:matrix
   #:*create-matrix*
   #:make-vector
   #:with-vector-type
   #:make-matrix
   #:make-matrix/human
   #:make-matrix%
   #:identity-matrix
   #:make-vector-from-rows
   #:vect
   #:matr
   #:droprows-from
   #:make-matrix-from-rows
   #:elementwise-operation
   #:multi-dim-dotimes+
   #:make-diagonal-vector
   #:make-diagonal-matrix))

(in-package :linear-algebra/vectors)

(defclass vector (gm:generic-math-object)
  ((entries :initarg :entries
                 :accessor entries))
  (:documentation "Model a vector representated by a multi-dimensional array."))

(defclass matrix (vector)
  ()
  (:documentation "extend the vector class with matrix multiplication."))

(defgeneric dimensions (vector))

(defmethod dimensions ((vector vector))
  (array-dimensions (entries vector)))

(defun mref (vector-or-matrix &rest indices)
  "access vector or matrix entries, where indexing start with 0."
  (apply #'aref (entries vector-or-matrix) indices))

(defun set-mref (vector-or-matrix &rest indices+value)
  "setf method for MREF."
  (multiple-value-bind (indices value) (split-last indices+value)
    (setf (apply #'aref (entries vector-or-matrix) indices)
          value)))

(defsetf mref set-mref)

(defun mref/human (vector-or-matrix &rest indices)
  "access vector or matrix entries, where indexing starts with 1."
  (apply #'mref vector-or-matrix
         (mapcar #'1- indices)))

(defun nreverse-with-tail (list &optional tail)
  "Reverse `list' reusing cons cells, but use `tail' as the end of the
  result."
  (if (null list) tail
      (nreverse-with-tail (cdr list)
                          (progn (setf (cdr list) tail)
                                 list))))

(defun map-butlast (function list)
  "Map `function' inplace over `list', except at the last element."
  (check-type function function)
  (check-type list list)
  (labels ((rec (list acc)
             (if (and (consp list)
                      (consp (cdr list)))
                 (rec (cdr list)
                      (cons (funcall function (car list)) acc))
                 (nreverse-with-tail acc list))))
    (rec list nil)))

(defun set-mref/human (vector-or-matrix &rest indices+value)
  "setf method for MMREF."
  (apply #'set-mref vector-or-matrix
         (map-butlast #'1- indices+value)))

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

(ew
  (defun make-vector/general (human dimensions index-vars fill-form &optional vector-type)
    "Convenience macro for building vectors.

* If `human' is t, start indexing from 1 instead of 0. This also
  affects `index-vars'.

* `dimensions' should be either a list of expressions evaluating to
  integers, or have the shape `(:list dims)' where `dims' should
  evaluate to a list.

* `index-vars' should be a lambda-parameter form, which should take as many
  parameters as there are `dimensions'. Special values for this are t,
  which autogenerates `(&rest indices)', and nil, which
  autogenerates `(i0 i1 ...)' (or `(i1 i2 ...)' if `human' is t), with
  the appropriate number taken from dimensions. Note the latter is
  incompatible with the `(:list dims)' form for `dimensions'.

* `fill-form' should be some expression which uses the `index-vars'
  and produces numbers to put in the appropriate vector/matrix entry.

* With `vector-type', one can control whether to create a `vector' or
  a `matrix'. This essentially gets passed to `make-vector%', so
  instead of using the argument, one can also use the
  `*create-matrix*' special variable."
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
  "see make-vector/general for doc (with `human' = nil)"
  (make-vector/general nil dimensions index-vars fill-form))

(defmacro make-vector/human (dimensions index-vars &body fill-form)
  "see make-vector/general for doc (with `human' = t)"
  (make-vector/general t dimensions index-vars fill-form))

(defmacro make-matrix (dimensions index-vars &body fill-form)
  "as make-vector, but return a `matrix'."
  (make-vector/general nil dimensions index-vars fill-form 'matrix))

(defmacro make-matrix/human (dimensions index-vars &body fill-form)
  "as make-vector/human, but return a `matrix'."
  (make-vector/general t dimensions index-vars fill-form 'matrix))

(defun dimensions-compatible-p (&rest vectors)
  "Test whether the given `vectors' have the same `dimensions', so we
can perform elementwise operations. On success, returns the list of
dimensions of the `vectors'."
  (case (length vectors)
    ((0) (error "no vector given for dimensions test."))
    ((1) (dimensions (first vectors)))
    (t   (let ((dimensions-of-vectors
                (mapcar #'dimensions vectors)))
           (when (apply #'equal dimensions-of-vectors)
             (first dimensions-of-vectors))))))

(defmacro elementwise-operation (vectors &body fill-form)
  "Create a new vector with same `dimensions' as `vectors' (a list of
  symbols referencing actual vectors), where every field is filled
  with `fill-form', where every vector symbol stands for the
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

;;; don't do division, it's nasty
#|(defmethod gm:generic-/ ((vector-a vector) (vector-b vector))
  (elementwise-operation (vector-a vector-b)
    (gm:/ vector-a vector-b)))|#

;;; operations with scalars
(bind-multi ((scalar rational finite-fields:integer-mod))
  (defmethod gm:generic-+ ((scalar scalar) (vector vector))
    (elementwise-operation (vector)
      (gm:+ scalar vector)))

  (defmethod gm:generic-* ((scalar scalar) (vector vector))
    (elementwise-operation (vector)
      (gm:* scalar vector)))

  (gm:declare-commutative scalar vector
    gm:generic-+
    gm:generic-*)

  (defmethod gm:generic-- ((scalar scalar) (vector vector))
    (elementwise-operation (vector)
      (gm:- scalar vector)))

  (defmethod gm:generic-- ((vector vector) (scalar scalar))
    (gm:generic-+ (gm:- scalar) vector))

  (defmethod gm:generic-/ ((vector vector) (scalar scalar))
    (gm:generic-* (gm:/ scalar) vector)))

(defmacro! define-index-transform (name (&rest args) dim-form &rest index-forms)
  "Define a transformation of a multidim vector `v' by supplying a
`dim-form' to calculate the new `dimensions', with the anaphoric
`dimensions' of `v'; and using `index-forms' to calculate the indices
in `v' from the anaphoric `indices' in the new vector. Use the helper
macro `with-indices' to destructure `indices'."
  `(defun ,name (,g!vector ,@args)
     (macrolet ((with-indices (dest &body body)
                  `(destructuring-bind ,dest indices ,@body)))
       (let ((dimensions (dimensions ,g!vector))
             (,g!coeffs (entries ,g!vector)))
         (make-vector% ,dim-form
                       (lambda (,g!this &rest indices)
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

;;; entferne alle Zeilen ab i.
(define-index-transform droprows-from% (i)
  (cons i (cdr dimensions))
  indices)

(defun droprows-from (vector i)
  "Remove all rows with index starting from i"
  ;; if i coincides with the total number of rows ...
  (if (= i (first (dimensions vector)))
      vector
      (droprows-from% vector i)))

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
(defun dot-product (matrix-a matrix-b &optional (return-type 'matrix))
  (multiple-value-bind (dims-a m-a) (split-last (copy-list (dimensions matrix-a)) )
    (destructuring-bind (m-b . dims-b) (dimensions matrix-b)
      (unless (= m-a m-b)
        (error "Dimensions of ~A and ~A are not compatible for matrix multiplication." matrix-a matrix-b))
      (let ((split (length dims-a))
            (entries-a (entries matrix-a))
            (entries-b (entries matrix-b))
            (dims (append dims-a dims-b)))
        (if dims
            (make-vector% dims
                          (ilambda (this &rest indices)
                            (let ((ind-a (subseq indices 0 split))
                                  (ind-b (subseq indices split)))
                              (gm:gm-summing (i 0 m-a t)
                                          (gm:* (apply #'aref entries-a (append1 ind-a i))
                                                (apply #'aref entries-b i ind-b)))))
                          return-type)
            ;; special case if dims is empty list (i.e. dot-product
            ;; of two simple vectors)
            (gm:gm-summing (i 0 m-a t)
                        (gm:* (aref entries-a i) (aref entries-b i))))))))

(defmethod gm:generic-* ((matrix-a matrix) (matrix-b matrix))
  (dot-product matrix-a matrix-b 'matrix))

(defmethod gm:generic-* ((vector-a vector) (matrix-b matrix))
  (dot-product vector-a matrix-b 'vector))

(defmethod gm:generic-* ((matrix-a matrix) (vector-b vector))
  (dot-product matrix-a vector-b 'vector))

;;; TODO scalar multiplication and addition
;;; TODO sparse matrices
;;; TODO matrix construction utilities

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

(defun identity-matrix (dimension)
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

;;; more matrix creation functions
(defun array-dimensions+ (array-or-other)
  "If something is not an array, it has dimensions NIL."
  (if (arrayp array-or-other)
      (array-dimensions array-or-other)))

(defun all-equal (list &key key (test 'equal))
  "For a given equivalence relation, ensure that all elements of list
  are the same."
  (if (or (null list) (null (cdr list)))
      t
      (let ((first (funcall key (first list))))
        (every (lambda (other) (funcall test first other))
               (mapcar key (rest list))))))

(defun same-dimensions-p/array (arrays)
  "Check that all given arrays have same dimensions."
  (all-equal arrays :key #'array-dimensions+))

(defun same-dimensions-p (vectors)
  "Check that all given vectors have same dimensions."
  (all-equal vectors :key (compose #'array-dimensions #'entries)))

;;; building matrices
(defun make-array-from-rows (rows)
  (let ((arrays (mapcar #'make-array-from-row rows)))
    (if (same-dimensions-p/array arrays)
        (let* ((dim (array-dimensions+ (first arrays)))
              (array (make-array (cons (length arrays) dim)
                                 :initial-element +unfilled+)))
          (if dim
              (fill-array array
                       (ilambda (this i &rest indices)
                         (apply #'aref (nth i arrays) indices)))
              (fill-array array
                          (ilambda (this i)
                            (nth i arrays))))
          array)
        (error "Incompatible dimensions in given rows."))))

(defun make-array-from-row (row)
  (typecase row
    (array row)
    (list (make-array-from-rows row))
    (vector (entries row))
    (t row)))

(defun make-vector-from-rows (rows)
  "Create a vector from the given rows, which may be lists, arrays or
  vectors."
  (make-instance (or *create-matrix* 'vector) :entries (make-array-from-rows rows)))

(define-matrix-variant make-vector-from-rows make-matrix-from-rows)

;;; abbreviations for creating vectors and matrices
(defmacro vect (&rest rows)
  `(make-vector-from-rows ',rows))

(defmacro matr (&rest rows)
  `(make-matrix-from-rows ',rows))

;;; TODO destructive operations.

;;; printing of vector and matrix
(defun vector2->list (vector2)
  (destructuring-bind (m n) (array-dimensions vector2)
    (iter (for i from 0 below m)
          (collect (iter (for j from 0 below n)
                         (collect (aref vector2 i j)))))))

(defun print-vector (stream array)
  (case (length (array-dimensions array))
    (1 (format stream "~{~A~^ ~}" (coerce array 'list)))
    (2 (format stream "~&~{~{~,4T~A~}~%~}" (vector2->list  array)))
    (t (format stream "~A" array))))


(defmethod print-object ((vector vector) stream)
  (print-unreadable-object (vector stream :type t)
    (print-vector stream (entries vector)))
  vector)
