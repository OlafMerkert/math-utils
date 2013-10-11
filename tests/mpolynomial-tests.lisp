(in-package :math-utils-tests)

(def-suite mpolynomials :in math-utils)

(in-suite mpolynomials)

(test make-poly
  (let ((p (mpoly:make-mpolynomial 'Y 2 7 1/8)))
    (is (= (mpoly:mdegree p 'Y) 2))
    (is (= (mpoly:mdegree p 'X) 0))
    (is (= (polynomials:leading-coefficient p) 2))
    (is (= (polynomials:constant-coefficient p) 1/8))
    (is (= (polynomials:nth-coefficient p 2) 2))
    (is (= (polynomials:nth-coefficient p 1) 7))
    (is (= (polynomials:nth-coefficient p 0) 1/8))))

(test poly-equal
  (let ((p (mpoly:make-mpolynomial 'Y 8 8 8 8 9))
        (q (mpoly:make-mpolynomial 'Y 8 8 8 8 9))
        (r (mpoly:make-mpolynomial 'Y 8 7 8 8 9))
        (s (mpoly:make-mpolynomial 'Y 10 8 7 8 8 9)))
    (is (gm:= p q))
    (is (not (gm:= p r)))
    (is (not (gm:= r s)))
    (is (not (gm:= p s)))))

(test poly-equal-special
  (let ((one (mpoly:make-mpolynomial 'Y 1))
        (zero (mpoly:make-mpolynomial 'Y 0)))
    (is (gm:= 1 one))
    (is (gm:one-p one))
    (is (gm:= one (gm:one 'mpoly:mpolynomial)))
    (is (gm:= 0 zero))
    (is (gm:zero-p zero))
    (is (gm:= zero (gm:zero 'mpoly:mpolynomial)))))

(test poly-simplify
  (let ((p (mpoly:make-mpolynomial 'Y 0 0 0 8 7 1 7))
        (q (mpoly:make-mpolynomial 'Y 8 7 1 7))
        (r (mpoly:make-mpolynomial 'Y 0 0 0 0 0)))
    (is (gm:zero-p r))
    (is (gm:= p q))))

(test poly-add
  (let ((p   (mpoly:make-mpolynomial 'Y   2 3 4 5))
        (q   (mpoly:make-mpolynomial 'Y 2 3 4 5 1))
        (r   (mpoly:make-mpolynomial 'Y   3 4 5 1))
        (p+q (mpoly:make-mpolynomial 'Y 2 5 7 9 6))
        (p+r (mpoly:make-mpolynomial 'Y   5 7 9 6))
        (q+r (mpoly:make-mpolynomial 'Y 2 6 8 10 2)))
    (is (gm:= (gm:+ p q) p+q))
    (is (gm:= (gm:+ p r) p+r))
    (is (gm:= (gm:+ r q) q+r))
    (is (gm:= (gm:- p+q q) p))
    (is (gm:= (gm:- q+r r) q))
    (is (gm:zero-p (gm:- p p)))))

;;; todo test make-monomial

(test poly-mult
  (let ((p (mpoly:make-mpolynomial 'Y 3 0))
        (p^2 (mpoly:make-mpolynomial 'Y 9 0 0))
        (q (mpoly:make-mpolynomial 'Y 1 1))
        (r (mpoly:make-mpolynomial 'Y 1 -1))
        (r*q (mpoly:make-mpolynomial 'Y 1 0 -1))
        (q^2 (mpoly:make-mpolynomial 'Y 1 2 1))
        (q^2*p (mpoly:make-mpolynomial 'Y 3 6 3 0)))
    (is (gm:= p^2 (gm:* p p)))
    (is (gm:= q^2 (gm:* q q)))
    (is (gm:= r*q (gm:* r q) (gm:* q r)))
    (is (gm:= q^2*p (gm:* q q p)))))

;;; todo more tests for poly arithmetic

;;; todo test poly division
