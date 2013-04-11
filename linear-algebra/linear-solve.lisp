(defpackage :linear-algebra/linear-solve
  (:nicknames :linsolve)
  (:shadowing-import-from :linear-algebra/vectors #:vector #:fill-array)
  (:use :cl :ol :iterate
        :linear-algebra/vectors
        :linear-algebra/elementary-matrices)
  (:export
   #:lu-decomposition))

(in-package :linear-algebra/linear-solve)


;;; ----------------------------------------------------------------------
;;; TODO LU decomposition for 2d matrices

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defun find-pivot-helper (find-pivot matrix row column)
  (aif (funcall find-pivot
                (subseq (entries (subcol matrix column)) row))
       (+ row it)))

(defun find-pivot (seq)
  "find a row below ROW with non-zero entry in COLUMN"
  (position-if-not #'gm:zero-p seq))

(defun position-maximum (seq &key (key #'identity) (compare #'<))
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


;;; TODO perhaps choose maximal entry
;;; for this there are probably different strategies

(defun lu-decomposition (matrix &optional normalise-pivot (find-pivot #'find-pivot))
  "Decompose A = L U where U is upper triangular, and L is a product
of elementary row operations. Returns (values U L P) where L and P are
list of row-ops you may process with COMPUTE-L, COMPUTE-L-INVERSE and
COMPUTE-L-P, as well as APPLY-L and APPLY-L-INVERSE."
  ;; TODO what about normalising stuff at the pivot?
  (let (row-ops
        pivot-transpositions
        (current-row 0))
    (destructuring-bind (m n) (dimensions matrix)
      ;; clearout all columns below diagonal
      (dotimes (current-column n)
        (awhen (find-pivot-helper find-pivot matrix current-row current-column)
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
                      (setf matrix (gm:generic-* rowop matrix))))))
          ;; now we mounted the ladder
          (incf current-row))
        ;; no pivot found means the column was zero below current-row,
        ;; so try the next one.
        ))
    ;; TODO figure out whether to reverse any of the row-ops or
    ;; pivot-transpositions
    (values matrix row-ops pivot-transpositions)))
;;; TODO mark the matrix as triangular.

(defun compute-l (l-list &optional vector)
  ;; we obtained U = L_1 L_2 ... L_k A where L_i are elements of
  ;; L-LIST in order, with L U = A we get L = L_k^-1 ... L_1^-1
  (reduce #'gm:generic-*
          (reverse l-list)
          :key #'inverse
          :from-end t ; want elementary on the left
          :initial-value (or vector
                             (identity-matrix (dimension (first l-list))))))

(defun compute-l-inverse (l-list &optional vector)
  ;; we obtained U = L_1 L_2 ... L_k A where L_i are elements of
  ;; L-LIST in order, with L U = A we get L^1 = L_1 ... L_k
  (reduce #'gm:generic-*
          l-list
          :from-end t ; want elementary on the left
          :initial-value (or vector
                             (identity-matrix (dimension (first l-list))))))

(defun map-reverse (fn lst &optional acc)
  (if (consp lst)
      (map-reverse fn (cdr lst) (cons (funcall fn (car lst)) acc))
      acc))

(defun compute-l-p (l-list p-list)
  ;; we obtained U = L_1 L_2 ... L_k A where some L_i where P_j, so
  ;; p-list contains some of the P^T = P_1 ... P_r = L_k_1 ... L_k_r with
  ;; k_1 < ... < k_r. Now we want L U = P A, so P^T L U = P^T P A = A,
  ;; thus L = P_1 ... P_r L_k^-1 ... L_1^-1 and P = P_r ... P_1
  (values (reduce #'gm:generic-*
                  (append p-list
                          (map-reverse #'inverse l-list))
                  :from-end t
                  :initial-value (identity-matrix (dimension (first l-list))))
          (reduce #'gm:generic-*
                  (reverse p-list)
                  :from-end t
                  :initial-value (identity-matrix (dimension (first p-list))))))

;;; TODO apply-l and apply-l-inverse

;;; TODO solving triangular systems, calculating nullspace

;;; TODO special matrix type for LU decomposition, with transparent
;;; access to lower and upper triangular parts

;;; TODO what about destructive operations (for efficiency?)
;;; can probably be implemented as additional type
