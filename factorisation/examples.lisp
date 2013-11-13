(defpackage :factorisation/poly-examples
  (:nicknames :fac-ex)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :fac-ffp
        :fac-sqf
        :fac-poly
        :generic-math
        :polynomials
        :fractions)
  (:export))

(in-package :factorisation/poly-examples)

(setf math-utils-format:*print-poly-pretty* t)

(defun example-1 (n p)
  (let ((poly (gm:-> 'finite-fields:integer-mod
                     (gm:- (polynomials:make-monomial n 1)
                           (polynomials:make-monomial 1 1)) :mod p)))
    (dbug "Poly: ~A" poly)
    (factorise poly)))

(defun example-2 (n)
  (let ((poly (- (make-monomial n 1) 1)))
    (dbug "Poly: ~A" poly)
    (factorise poly)))

(defun example-3 ()
  (let ((poly (make-polynomial 1 0 -6 -6 12 -36 1)))
    (dbug "Poly: ~A" poly)
    (factorise poly)))

(defparameter ex3 (make-polynomial 1 0 -6 -6 12 -36 1))
