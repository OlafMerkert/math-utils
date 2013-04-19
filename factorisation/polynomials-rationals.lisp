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

(defun lcm-of-coeffs (polynomial)
  (apply #'lcm (map 'list #'denominator (coefficients polynomial))))

(defun make-monic/integer (polynomial leading-coeff)
  "Transform poly a_0 X^n + ... to (a_0)^(n-1) a_0 (X/a_0)^n + ... to
  make it monic. The inverse of this operation is the same, but called
  with (/ original-leading-coeff)."
  ;; this corresponds to multiplying a_i with a_i^( (n-1) - i )
  (let ((factor (/ leading-coeff)))
    (make-instance 'polynomial :var (var polynomial)
                   ;; careful, here we assume map operates
                   ;; sequentially from the beginning
                   :coefficients (map 'vector (lambda (x) (prog1 (* x factor)
                                                       (setf factor (* factor leading-coeff))))
                                      (coefficients polynomial)))))

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

;;; some test stuff
;; (setf example (make-polynomial 8/9 2/5 0 7/3 1/2))
;; (setf ex2 (gm:* (lcm-of-coeffs example) example))
;; (gm:= ex2 (make-monic/integer (make-monic/integer ex2 80 ) (/ 80)))

(defun factorise-generic-poly (polynomial)
  (let ((derivative (derivative polynomial)))
    (acond
     ;; constant monic polys have no factors
     ((constant-p polynomial) nil)
     ;; if the gcd of poly and derivative is non-constant, we split
     ;; polynomial in two factors already
     ((non-constant-p (ggt polynomial derivative))
      (merge-factors (factorise-generic-poly it)
                     (factorise-generic-poly (/ polynomial it))))
     ;; otherwise, we know the polynomial is SQUAREFREE
     (t (mapcar (lambda (x) (cons x 1))
                (factorise-squarefree-poly polynomial))))))

(defun factorise-squarefree-poly (polynomial))
