(in-package :math-utils-tests)

(def-suite polynomials :in math-utils)

(in-suite polynomials)

(test make-poly
  (let ((p (polynomials:make-polynomial 2 7 1/8)))
    (is (= (polynomials:degree p) 2))
    (is (= (polynomials:leading-coefficient p) 2))
    (is (= (polynomials:constant-coefficient p) 1/8))
    (is (= (polynomials:nth-coefficient p 2) 2))
    (is (= (polynomials:nth-coefficient p 1) 7))
    (is (= (polynomials:nth-coefficient p 0) 1/8))))

(test poly-equal
  (let ((p (polynomials:make-polynomial 8 8 8 8 9))
        (q (polynomials:make-polynomial 8 8 8 8 9))
        (r (polynomials:make-polynomial 8 7 8 8 9))
        (s (polynomials:make-polynomial 10 8 7 8 8 9)))
    (is (gm:= p q))
    (is (not (gm:= p r)))
    (is (not (gm:= r s)))
    (is (not (gm:= p s)))))

(test poly-equal-special
  (let ((one (polynomials:make-polynomial 1))
        (zero (polynomials:make-polynomial 0)))
    (is (gm:= 1 one))
    (is (gm:one-p one))
    (is (gm:= one (gm:one 'polynomials:polynomial)))
    (is (gm:= 0 zero))
    (is (gm:zero-p zero))
    (is (gm:= zero (gm:zero 'polynomials:polynomial)))))

(test poly-simplify
  (let ((p (polynomials:make-polynomial 0 0 0 8 7 1 7))
        (q (polynomials:make-polynomial 8 7 1 7))
        (r (polynomials:make-polynomial 0 0 0 0 0)))
    (is (gm:zero-p r))
    (is (gm:= p q))))

(test poly-add
  (let ((p   (polynomials:make-polynomial   2 3 4 5))
        (q   (polynomials:make-polynomial 2 3 4 5 1))
        (r   (polynomials:make-polynomial   3 4 5 1))
        (p+q (polynomials:make-polynomial 2 5 7 9 6))
        (p+r (polynomials:make-polynomial   5 7 9 6))
        (q+r (polynomials:make-polynomial 2 6 8 10 2)))
    (is (gm:= (gm:+ p q) p+q))
    (is (gm:= (gm:+ p r) p+r))
    (is (gm:= (gm:+ r q) q+r))
    (is (gm:= (gm:- p+q q) p))
    (is (gm:= (gm:- q+r r) q))
    (is (gm:zero-p (gm:- p p)))))

;;; todo test make-monomial

(test poly-mult
  (let ((p (polynomials:make-polynomial 3 0))
        (p^2 (polynomials:make-polynomial 9 0 0))
        (q (polynomials:make-polynomial 1 1))
        (r (polynomials:make-polynomial 1 -1))
        (r*q (polynomials:make-polynomial 1 0 -1))
        (q^2 (polynomials:make-polynomial 1 2 1))
        (q^2*p (polynomials:make-polynomial 3 6 3 0)))
    (is (gm:= p^2 (gm:* p p)))
    (is (gm:= q^2 (gm:* q q)))
    (is (gm:= r*q (gm:* r q) (gm:* q r)))
    (is (gm:= q^2*p (gm:* q q p)))))

;;; todo more tests for poly arithmetic

;;; todo test poly division
