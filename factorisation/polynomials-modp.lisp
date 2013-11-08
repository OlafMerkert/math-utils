(defpackage :factorisation-polynomials-modp
  (:nicknames :pfactp)
  (:shadowing-import-from :fractions :numerator :denominator)
  (:shadowing-import-from :generic-math :+ :- :* :/ := :expt :sqrt :summing :^ :_)
  (:use :cl :ol :iterate
        :generic-math
        :polynomials
        :finite-fields
        :fractions)
  (:export
   #:get-prime
   #:merge-factors
   #:factorise
   #:multiply-factors))

(in-package :factorisation-polynomials-modp)

;;; todo verify these defaults
;;; todo move elsewhere perhaps??
(defmethod ggt ((a integer-mod) (b integer-mod)) 1)
(defmethod ggt ((a integer) (b integer-mod)) 1)
(defmethod ggt ((b integer-mod) (a integer)) 1)

(defmethod content ((a integer-mod)) 1)

(defun factorise-squarefree-poly (u)
  ;; use Berlekamp's algorithm
  (let* ((p (get-prime u))
         (n (degree u))
         (q (vectors:make-matrix-from-rows
             ;; counting k down, because our poly coeffs start with
             ;; leading coefficients.
             (iter (for k from (- n 1) downto 0)
                   (collect (pad-vector-front
                             ;; here the order of coefficients does
                             ;; not really matter
                             (coefficients (nth-value
                                            1
                                            (/ (make-monomial (* p k) (int% 1 p))
                                               u)))
                             n))))))
    ;; here we have to tranpose (Knuth has the vector on the left side
    ;; of the matrix)
    (multiple-value-bind (v r) (linsolve:nullspace (- (vectors:transpose q) (vectors:identity-matrix n)))
      (if (cl:= r 1)
          ;; polynomial is irreducible
          u
          ;; otherwise find factors
          (iter (for vcoeffs in v)
                (for factors first (splitting-helper p vcoeffs u)
                     then  (mapcan (lambda (w) (splitting-helper p vcoeffs w)) factors))
                (until (cl:<= r (length factors)))
                (finally (return (mapcar #'make-monic factors))))))))

(defun splitting-helper (p coeff-vector poly-to-split)
  ;; indexing of coefficients can simply be chosen to be consistent
  ;; with how we collect the 'rows' (rather cols) above.
  (let ((poly (simplify (make-instance 'polynomial :coefficients (vectors:entries coeff-vector)) :downgrade nil))
        (factors))
    (dotimes (s p)
      (aif (non-constant-p (ggt (gm:- poly (int% s p)) poly-to-split))
           (push it factors)))
    ;; either we found some (finer grained) factors, or we just keep
    ;; the factor
    (or factors
        (list poly-to-split))))

