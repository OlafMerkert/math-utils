(defpackage :permutations
  (:use :cl :ol :iterate )
  (:shadow :compose)
  (:export
   #:permutation-p
   #:inverse
   #:transposition
   #:transpose
   #:image
   #:preimage
   #:cycles
   #:extend
   #:identity-permutation
   #:compose))

(in-package :permutations)

;;; view finite vectors as permutations of the number 0,...,l-1 where
;;; l is the length of the vector.

(defun make-bit-vector (l)
  (make-array l :initial-element nil :element-type 'boolean))

(defun permutation-p (vector)
  (when (vectorp vector)
    (let* ((l (length vector))
           (l- l)
           (present (make-bit-vector l)))
      (block perm
        (iter (for j in-vector vector)
              (unless (and (integerp j) (<= 0 j) (< j l))
                (return-from perm nil))
              (unless #1=(aref present j)
                      (setf #1# t)
                      (decf l-)))
        (zerop l-)))))

(defun identity-permutation (n)
  (aprog1 (make-array n :initial-element 0
                      :element-type 'integer)
    (iter (for i from 1 below n)
          (setf (aref it i) i))))

(defun inverse (vector)
  (let ((inverse (make-array (length vector)
                             :initial-element 0
                             :element-type 'integer)))
    (iter (for i from 0)
          (for j in-vector vector)
          (setf (aref inverse j) i))
    inverse))

(defun extend (vector n)
  (let ((perm (make-array n :element-type 'integer
                          :initial-element 0))
        (l (length vector)))
    (iter (for i from 0 below l)
          (setf (aref perm i) (aref vector i)))
    (iter (for i from l below n)
          (setf (aref perm i) i))
    perm))

(defun transposition (i j &optional (n (+ 1 (max i j))))
  (declare (inline transpose))
  (let ((id (identity-permutation n)))
    (transpose id i j)
    id))

(defun transpose (vector i j)
  (rotatef (aref vector i) (aref vector j)))

;; just use `equal' for comparing permutations

(declaim (inline image preimage))
(defun image (permutation i)
  (aref permutation i))

(defun preimage (permutation j)
  (position j permutation :test #'=))

(defun image-set (permutation i j)
  "Alter the `permutation' s.t. the image of I becomes J."
  ;; before the operation: i -> r, s -> j
  ;; after the operation: i -> j, s -> r
  (let ((r (image permutation i))
        (s (preimage permutation j)))
    (setf (aref permutation i) j
          (aref permutation s) r))
  j)

(defsetf image image-set)
(defsetf preimage (permutation j) (i)
  `(progn (image-set ,permutation ,i ,j) ,i))

(defun cycles (permutation)
  "Find the cycles of the given permutation."
  (let* ((l (length permutation))
         (visited (make-bit-vector l))
         cycles)
    (labels ((next-cycle ()
               (awhen (position nil visited)
                 (let ((cycle (list it)))
                   (setf (aref visited it) t)
                   (push cycle cycles)
                   (compute-cycle it cycle))))
             (compute-cycle (start cycle)
               (let ((next (aref permutation (car cycle))))
                 (cond ((eql start next)
                        (next-cycle))
                       (#1=(aref visited next)
                           (error "not a permutation: visited ~A twice." next))
                       (t (let ((n-cycle (list next)))
                            (setf (cdr cycle) n-cycle
                                  (aref visited next) t)
                            (compute-cycle start n-cycle)))))))
      (next-cycle)
      (nreverse cycles))))

(defun compose (perm2 perm1)
  "Composition of permutations as functions, where first `perm1' is
  applied, then `perm2'. Effectively, this means applying `perm1' onto
  `perm2'."
  (let ((l1 (length perm1))
        (l2 (length perm2)))
    (unless (<= l1 l2) (error "cannot apply a permutation on a shorter array."))
    (let ((new-perm (copy-seq perm2)))
      (iter (for i from 0)
            (for j in-vector perm1)
            (setf (aref new-perm i) (aref perm2 j))))))
