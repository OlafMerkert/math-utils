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
  (:export
   #:berlekamp))

(in-package :factorisation/berlekamp)


(defun berlekamp-build-matrix (poly &optional (p (modulus poly)) (n (degree poly)))
  ;; we choose the basis 1, x, x^2, ..., x^(n-1) and compute the
  ;; matrix for the map t -> t^p - t
  (let* ((x^p (expt-mod (make-monomial 1 1) p poly))
         (matrix (vectors:make-diagonal-matrix (int% -1 p) n n))
         (entries (vectors:entries matrix)))
    (iter (for b initially (int% 1 p) then (divr (* x^p b) poly))
          (for j from 0 below n)
          ;; put the coefficients from the polynomial in the j-th column
          (fill-array entries (lambda (this i jj)
                                (+ (aref this i jj)
                                   (nth-coefficient b i)))
                      ;; so we select a column, and number of rows
                      ;; depends on degree of b
                      (list (list 0 (+ 1 (degree b)))
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

;; an alternative to the somewhat probabilistic approach in
;; `berlekamp-find-factor', which moreover might require additional
;; nullspace computations, because we have to start the whole process
;; again.

(defun berlekamp-find-factors (poly basis-polynomials p &optional (dim (length basis-polynomials)))
  ;; use the fact that each basis-polynomial (and also every linear
  ;; combination) satisfies b^p - b =0, so in a factor of the CRT
  ;; product, one of these must vanish.
  (let ((factor-count 1)
        (dim (- dim 1))
        split-factors
        (factors (list poly)))
    (block search-all-factors
      (labels ((split-off-factors (poly basis-poly)
                 (dotimes (k p)
                   ;; every time we find a new factor, put it into `split-factors'
                   (awhen (non-constant-p (ggt (- basis-poly (int% k p)) poly))
                     (incf factor-count)
                     (push it split-factors)
                     (setf poly (/ poly it))
                     (test-all-factors)))
                 ;; the part we couldn't split off must not be forgotten
                 (unless (constant-p poly)
                   (push poly split-factors)))
               (test-all-factors ()
                 ;; short circuit out of our multiloop construction as
                 ;; soon as we have all factors but one -- the one
                 ;; remaining sits in poly.
                 (when (<= dim factor-count)
                   (if (non-constant-p poly)
                       (push poly split-factors))
                   (return-from  search-all-factors
                     (mapcar (clambda (make-factor :base x!)) (nconc split-factors factors))))))
        (iter (for b in basis-polynomials)
              (ol::while% factors
                (split-off-factors (pop factors) b))
              ;; after one basis-polynomial is done, everything moved
              ;; from factors to split-factors -- time to move it back
              (setf factors split-factors
                    split-factors nil))
        ;; if at this point, we haven't found all factors yet,
        ;; something went wrong
        (error "Did not find all factors with the split off method.")))))

(defun berlekamp (poly &optional (p (modulus poly)))
  (mvbind (vectors dim) (nullspace (berlekamp-build-matrix poly p))
    (if (cl:= dim 1)
        (list (make-factor :base poly))
        (let ((basis-polynomials
               (mapcar (lambda (v) (simplify
                               (make-instance 'polynomial
                                              :var (var poly)
                                              :coefficients (reverse (entries v)))))
                       vectors)))
          (berlekamp-find-factors poly basis-polynomials p dim)))))


