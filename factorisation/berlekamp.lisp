(defpackage :factorisation/berlekamp
  (:nicknames :fac-bk)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate 
        :fac-ds
        :generic-math :fractions
        :polynomials :finite-fields)
  (:import-from :linear-algebra/linear-solve #:nullspace)
  (:import-from :linear-algebra/vectors #:entries)
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

(defun berlekamp-find-factor (poly basis-polynomials p)
  ;; choose a random nonzero element
  (let ((a (berlekamp-random-polynomial basis-polynomials p)))
    (aif (non-constant-p (ggt a poly))
         ;; if it is not coprime with `poly', we found a non-trivial
         ;; factor.
         it
         ;; otherwise, we have a^{p-1} = 1 in all the factor in the CRT
         ;; product. So b -1 = 0 for all factors happens with probability
         ;; (1/2)^r, and with luck, we get a non-trivial factor
         (let ((b (expt-mod a (/ (- p 1) 2) poly)))
           (non-constant-p (ggt (- b 1) poly))))))

;; todo might we not get into trouble for characteristic 2 here?
;; todo ensure that we are always getting monic polynomials -- this
;; might not happen everywhere

(defun berlekamp-random-polynomial (basis-polynomials p)
  "Choose a random-element in the nullspace, which is not 0."
  (mvbind (poly zero-p)
      ;; not too hard, we have a F_p basis for the nullspace already.
      (iter (for b in basis-polynomials)
            (for r next (random p))
            (for z first (zerop r) then (and z (zerop r)))
            ;; linear combination
            (for lc first (* b r) then (+ lc (* b r)))
            (finally (return (values lc z))))
    (if zero-p
        (berlekamp-random-polynomial basis-polynomials p)
        poly)))


(defun berlekamp (poly)
  (let ((p (modulus poly)))
    (mvbind (vectors rank) (nullspace (berlekamp-build-matrix poly p))
      (if (cl:= rank 1)
          (values (list (make-factor :base poly)) :irreducible)
          (let* ((basis-polynomials
                  (mapcar (lambda (v) (make-instance 'polynomial
                                                :var (var poly)
                                                :coefficients (reverse (entries v))))
                          vectors))
                 (factor (until-t (berlekamp-find-factor poly basis-polynomials p))))
            (list (make-factor :base factor)
                  (make-factor :base (/ poly factor))))))))
;; todo in theory, it should be avoidable to solve more nullspace
;; problems -- maybe we can reuse the basis of the nullspace??
