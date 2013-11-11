(defpackage :factorisation-polynomials-rationals
  (:nicknames :pfactrat)
  (:shadowing-import-from :cl :numerator :denominator)
  (:shadowing-import-from :gm :summing)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadow #:factorise-generic-poly)
  (:use :cl :ol :iterate
        :generic-math
        :polynomials
        :factorisation-polynomials-modp
        :fractions)
  (:export))

(in-package :factorisation-polynomials-rationals)

(defun eliminate-denominators-and-leading-coefficient (polynomial)
  "Given a polynomial with rational coefficients, transform it is
  defined over the integers and is monic, factorise that one and
  retransform the results."
  ;; first compute the lcm of the denominators, and give polynomial
  ;; integer coeffs
  (let* ((polynomial (gm:* (lcm-of-coeffs polynomial) polynomial))
         ;; then make it monic
         (lk (leading-coefficient polynomial))
         (1/lk (/ lk))
         (polynomial (make-monic/integer polynomial lk)))
    (map-on-car (lambda (x) (make-monic/integer x 1/lk)) (factorise-generic-poly polynomial))))

