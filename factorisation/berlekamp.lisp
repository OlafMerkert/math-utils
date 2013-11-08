(defpackage :factorisation/berlekamp
  (:nicknames :fac-bk)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate 
        :fac-ds
        :generic-math :fractions
        :polynomials :finite-fields)
  (:import-from :linear-algebra/linear-solve #:nullspace)
  (:export))

(in-package :factorisation/berlekamp)


(defun berlekamp-build-matrix (poly &optional (p (modulus poly)) (n (degree poly)))
  ;; we choose the basis 1, x, x^2, ..., x^(n-1) and compute the
  ;; matrix for the map t -> t^p - t
  (let* ((x^p (expt-mod (make-monomial 1 1) p poly))
         (matrix (vectors:make-diagonal-matrix (int% -1 p) n n))
         (entries (vectors:entries matrix)))
    (iter (for b initially (int% 1 p) then (div (* x^p b) poly))
          (for j from 0)
          ;; put the coefficients from the polynomial in the j-th column
          (fill-array entries (lambda (this i jj)
                                (+ (aref this i jj)
                                   (nth-coefficient b i)))
                      ;; so we select a column, and number of rows
                      ;; depends on degree of b
                      (list (list 0 (degree b))
                            (list j (cl:+ j 1)))))
    matrix))

(defun berlekamp (poly)
  (mvbind (vectors rank) (nullspace (berlekamp-build-matrix poly))
    (if (cl:= rank 1)
        (make-factor :base poly)
        ;; otherwise we have more fun. todo
        )))
