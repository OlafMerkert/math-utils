(defpackage :linear-algebra/linear-solve
  (:nicknames :linsolve)
  (:shadowing-import-from :linear-algebra/vectors #:vector #:fill-array)
  (:use :cl :ol :iterate
        :linear-algebra/vectors
        :linear-algebra/elementary-matrices)
  (:export
   #:lu-decomposition
   #:nullspace
   #:solve-upper-triangular
   #:compute-l-p
   #:compute-l-inverse
   #:compute-l))

(in-package :linear-algebra/linear-solve)


;;; ----------------------------------------------------------------------
;;; LU decomposition for 2d matrices

(defun find-pivot-helper (find-pivot matrix row column)
  "Pivot search always happens on a `column' of `matrix', where we
start from `row'. The found pivot is returned as absolute column
number."
  (aif (funcall find-pivot
                (subseq (entries (subcol matrix column)) row))
       (+ row it)))

(defun find-pivot (seq)
  "find a row below ROW with non-zero entry in COLUMN"
  (position-if-not #'gm:zero-p seq))

(defun position-maximum (seq &key (key #'identity) (compare #'<))
  "Find the index in `seq' where the image under `key' is maximal
w.r.t. `compare', where we favor the second argument of `compare'.
Returns as second value this maximal value."
  (let* ((seq2 (map 'cl:vector key seq))
         (max (elt seq2 0))
         (max-index 0))
    (iter (for i from 1 below (length seq2))
          (for m = (svref seq2 i))
          (when (funcall compare max m)
            (setf max m
                  max-index i)))
    (values max-index max)))

(defun find-pivot/reals (seq)
  "Compare by absolute value."
  (position-maximum seq :key #'abs))

(defun find-pivot/rational (seq)
  "Compare by height"
  (position-maximum seq :key #'height :compare #'<))

(defun height (rational)
  (max (abs (numerator rational))
       (abs (denominator rational))))

(defun find-pivot/p^k (p)
  "when solving linear systems mod P^k, we choose the pivot as the one
with the smallest order in P."
  (lambda (seq)
    (position-maximum seq :key (lambda (x) (nt:ord-p p (finite-fields:remainder x)))
                      :compare #'>)))

;;; TODO perhaps choose maximal entry
;;; for this there are probably different strategies

;;; fix the problem with discrepancy in rank and number of step-cols
;;; fix problem with output matrix not being triangular
(defun lu-decomposition (matrix &optional normalise-pivot (find-pivot #'find-pivot))
  "Decompose A = L U where U is upper triangular, and L is a product
of elementary row operations. Returns (values U L P RANK STEP-COLS
OTHER-COLS) where L and P are list of row-ops you may process with
COMPUTE-L, COMPUTE-L-INVERSE and COMPUTE-L-P, as well as APPLY-L and
APPLY-L-INVERSE. RANK is the rank of the matrix (equal for A and U).
STEP-COLS lists the columns where the next row starts, OTHER-COLS is
the complement."
  ;; TODO what about normalising stuff at the pivot?
  (let (row-ops
        pivot-transpositions
        (current-row 0)
        step-cols
        other-cols)
    (destructuring-bind (m n) (dimensions matrix)
      ;; clearout all columns below diagonal
      (dotimes (current-column n)
        (aif (and (< current-row m)
                  (find-pivot-helper find-pivot matrix current-row current-column))
             (progn
               (push current-column step-cols)
               ;; interchange rows
               (unless (= current-row it)
                 (let ((trans (make-transposition-matrix current-row it m)))
                   (push trans row-ops)
                   (push trans pivot-transpositions)
                   (setf matrix (gm:generic-* trans matrix))))
               ;; with settled pivot, kill every below current-row
               (let ((pivot-entry (mref matrix current-row current-column)))
                 ;; if desired, normalise the pivot entry to 1
                 (if (and normalise-pivot (not (gm:one-p pivot-entry )))
                     (let ((normaliser (make-single-diagonal-matrix
                                        current-row (gm:/ pivot-entry) m)))
                       (push normaliser row-ops)
                       (setf matrix (gm:generic-* normaliser matrix)
                             pivot-entry -1))
                     ;; otherwise just swap the sign
                     (setf pivot-entry (gm:- pivot-entry)))
                 ;; then go on clearing the column below the pivot
                 (iter (for i from (+ 1 current-row) below m)
                       (for entry = (mref matrix i current-column))
                       (unless (gm:zero-p entry)
                         (let ((rowop (make-add-row/col-matrix i current-row
                                                               (gm:/ entry pivot-entry)
                                                               m)))
                           (push rowop row-ops)
                           (setf matrix (gm:generic-* rowop matrix)))))))
             (push current-column other-cols))
        ;; now we mounted the ladder
        (incf current-row)
        ;; no pivot found means the column was zero below current-row,
        ;; so try the next one.
        ))
    ;; current-row now holds the rank of the matrix
    (values matrix row-ops pivot-transpositions current-row
            (nreverse step-cols) (nreverse other-cols))))
;;; TODO mark the matrix as triangular.

(defun compute-elementary-matrix-product (matrix-list &key reverse inverse vector)
  (reduce #'gm:generic-*
          (if reverse
              (reverse matrix-list)
              matrix-list)
          :key (if inverse #'inverse nil)
          :from-end t ; want elementary always on the left
          :initial-value (or vector
                             (identity-matrix (dimension (first matrix-list))))))


(defun compute-l (l-list &optional vector)
  ;; we obtained U = L_1 L_2 ... L_k A where L_i are elements of
  ;; L-LIST in order, with L U = A we get L = L_k^-1 ... L_1^-1
  (declare (inline compute-elementary-matrix-product))
  (compute-elementary-matrix-product l-list :reverse t :inverse t :vector vector))

(defun compute-l-inverse (l-list &optional vector)
  ;; we obtained U = L_1 L_2 ... L_k A where L_i are elements of
  ;; L-LIST in order, with L U = A we get L^-1 = L_1 ... L_k
  (declare (inline compute-elementary-matrix-product))
  (compute-elementary-matrix-product l-list :reverse nil :inverse nil :vector vector))

(defun compute-l-p (l-list p-list &optional vector)
  ;; we obtained U = L_1 L_2 ... L_k A where some L_i are P_j, so
  ;; p-list contains some of the P^T = P_1 ... P_r = L_k_1 ... L_k_r with
  ;; k_1 < ... < k_r. Now we want L U = P A, so P^T L U = P^T P A = A,
  ;; thus L = P_1 ... P_r L_k^-1 ... L_1^-1 and P = P_r ... P_1
  (declare (inline compute-elementary-matrix-product))
  (compute-elementary-matrix-product
   p-list :reverse nil :inverse nil
   :vector (compute-elementary-matrix-product
            l-list :reverse t :inverse t
            :vector vector)))

(defun compute-p (p-list &optional vector)
  ;; see above
  (declare (inline compute-elementary-matrix-product))
  (compute-elementary-matrix-product p-list :reverse nil :inverse nil :vector vector))


;;; TODO apply-l and apply-l-inverse

;;; TODO solving triangular systems, calculating nullspace
(defun nullspace (matrix)
  "return vectors spanning the nullspace of the given MATRIX, and the dimension as second value."
  (multiple-value-bind (triangular l p rank step-cols other-cols) (lu-decomposition matrix t)
    (declare (ignore l p))
    ;; remove zero rows
    (setf triangular (droprows-from triangular rank))
    ;; length of step-cols is exactly the rank, and the other cols
    ;; correspond to the generators of the kernel
    (values
     (mapcar (lambda (j) (nullspace-column triangular step-cols j)) other-cols)
     ;; dimension of the kernel is the number of other-cols
     (- (nth 1 (dimensions matrix)) rank))))

(defun nullspace-column (triangular step-cols col-index)
  ;; express `column2' as unique linear combination of step-columns
  (let ((column1 (solve-upper-triangular triangular (gm:- (subcol triangular col-index)) step-cols)))
    ;; as `solve-upper-triangular' only fills `step-cols', we need to
    ;; fill in the missing one:
    (setf (aref (entries column1) col-index) 1)
    column1))

(defun solve-upper-triangular (triangular vector &optional (step-cols (range (second (dimensions triangular)))))
  "Solve a matrix equation with only zeroes below the diagonal. We
assume `triangular' is an upper triangular matrix of full rank, where
`step-cols' gives for each row the first column with non-zero entry."
  (let* ((n (length step-cols))
         ;; start from the end, because it is upper triangular
         (step-cols (reverse step-cols))
         ;; not that only the coefficients for the step-cols will be nonzero
         (result (make-array (second (dimensions triangular)) :initial-element 0))
         (entries (entries triangular)))
    (iter (for i from (- n 1) downto 0)
          (for j in step-cols)
          ;; compute the coefficient, think of `triangular' as a
          ;; quadratic matrix, simply ignore the non-step-columns
          (setf (aref result j)
                (gm:/ (gm:- (mref vector i)
                            (reduce #'gm:+ previous-columns
                                    :key (lambda (c) (gm:* (aref entries i c)
                                                      (aref result c)))))
                      (aref entries i j)))
          (collect j into previous-columns at beginning))
    (make-instance 'vector :entries result)))

(defun linear-solve (matrix vector &key (all-solutions t))
  "Solve the linear system, return one base solution (if it exists),
  and as second value the vectors spanning the affine solution space
  at the base solution. The third value gives the rank of the
  `matrix'."
  (mvbind (triangular l p rank step-cols other-cols) (lu-decomposition matrix t)
    (declare (ignore p))
    ;; first apply `l' and `p' onto `vector', recall that `p' is a
    ;; sublist of `l'
    (let ((vector1 (compute-l-inverse l vector)))
      ;; next, we test that the entries from rank are all 0
      (if (not (every #'gm:zero-p (subseq (entries vector1) rank)))
          ;; no solutions
          nil
          ;; otherwise, get rid of the zero stuff and compute the
          ;; solution
          (progn
            (setf triangular (droprows-from triangular rank)
                  vector1 (droprows-from vector1 rank))
            (values
             ;; we get the base solution
             (solve-upper-triangular triangular vector1 step-cols)
             ;; and the generators of the nullspace, which describe
             ;; all the other solutions
             (cond (all-solutions
                    (mapcar (lambda (j) (nullspace-column triangular step-cols j))
                            other-cols))
                   ((null other-cols) nil)
                   (t :uncomputed))
             rank))))))


;;; TODO what about destructive operations (for efficiency?)
;;; can probably be implemented as additional type
