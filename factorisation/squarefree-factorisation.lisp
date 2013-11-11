(defpackage :factorisation/squarefree
  (:nicknames :fac-sqf)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :generic-math :fractions
        :polynomials :finite-fields)
  (:export
   #:finite-field-poly-p-root
   #:square-free-factorise))

(in-package :factorisation/squarefree)

(defun finite-field-poly-p-root (poly p)
  "In char `p', rewrite a polynomial `poly' in X^p to a polynomial f
s.t. f^p = poly."
  (let ((n (degree poly)))
    (assert (nt:divides-p p n))
    (make-instance 'polynomial
                   :coefficients
                   (make-array/fill ((cl:/ n p)) (i)
                     (nth-coefficient% poly (cl:* i p))))))

(defun square-free-factorise (poly)
  "produce a factorisation of `poly' into squarefree factors"
  (let ((derivative (derivative poly)))
    (acond ((zero-p derivative)
            ;; TODO careful when going to q = p^e
            (let ((p (modulus (leading-coefficient poly))))
              (map-on-exponents
               (lambda (x) (* x p))
               (square-free-factorise (finite-field-poly-p-root poly p)))))
           ((non-constant-p (ggt poly derivative))
            (merge-factor
             (square-free-factorise it)
             (make-factor :base (/ poly it))))
           (t (list (make-factor :base poly))))))

;; when we reduce mod p, we have to be slightly careful
(defun square-free-p (poly)
  "Test whether the given `poly' does not contain square factors."
  (constant-p (ggt poly (derivative poly))))
