(defpackage :factorisation/poly-examples
  (:nicknames :fac-ex)
  #.gm:+cl-shadow-imports+
  #.gm:+ol-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :fac-ffp
        :fac-sqf
        :fac-poly
        :fac-bk
        :generic-math
        :polynomials
        :finite-fields)
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
  (let ((poly (gm:- (make-monomial n 1) 1)))
    (dbug "Poly: ~A" poly)
    (factorise poly)))

(defun example-3 ()
  (let ((poly (make-polynomial 1 0 -6 -6 12 -36 1)))
    (dbug "Poly: ~A" poly)
    (factorise poly)))

(defparameter ex3 (make-polynomial 1 0 -6 -6 12 -36 1))

;; (setf ex3p (compute-mignotte-bounded-prime ex3))
;; (berlekamp-build-matrix ex3p)
;; (linear-algebra/linear-solve:nullspace *)
;; (berlekamp ex3p)
;; (combine-factors ex3p (mapcar #'factor-base *))
;; (setf primes (nt-p:erastothenes-sieve 80))
;; (map 'vector (lambda (p) (-> 'integer-mod ex3 :mod p)) primes)
;; (map 'vector #'factorise *)
;; (remove-if-not #'length=1 *)

