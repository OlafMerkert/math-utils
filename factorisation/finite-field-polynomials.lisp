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
           ;; each of them can be factorised with Berlekamp:
           (factorisation (map-on-factors #'berlekamp sqf-factors)))
      ;; finally, we have to account for the leading coefficient
      (if (gm:one-p leading-coefficient)
          factorisation
          (list* (make-factor :base leading-coefficient) factorisation)))))

;; (setf math-utils-format:*print-poly-pretty* t)

(defun example (n p)
  (let ((poly (gm:-> 'finite-fields:integer-mod
                     (gm:- (polynomials:make-monomial n 1)
                           (polynomials:make-monomial 1 1)) :mod p)))
    (dbug "Poly: ~A" poly)
    (factorise/poly-over-finite-field poly)))
