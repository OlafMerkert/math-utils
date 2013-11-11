(defpackage :factorisation/finite-field-polynomials
  (:nicknames :fac-ffp)
  (:use :cl :ol :iterate
        :polynomials
        :fac-ds
        :fac-sqf
        :fac-bk)
  (:export
   #:factorise/poly-over-finite-field))

(in-package :factorisation/finite-field-polynomials)

(defun factorise/poly-over-finite-field (polynomial)
  ;; first step: make the polynomial monic
  (mvbind (polynomial leading-coefficient) (make-monic polynomial)
    ;; then get at the square factors:
    (let* ((sqf-factors (square-free-factorise polynomial))
           (factorisation (map-on-factors #'berlekamp sqf-factors)))
      ;; each of them can be factorised with Berlekamp:
      (if (gm:one-p leading-coefficient)
          factorisation
          (list* (make-factor :base leading-coefficient) factorisation)))))

