(in-package :math-utils-tests)

(def-suite polynomials :in math-utils)

(in-suite number-theory)

(test make-poly
  (let ((p (polynomials:make-polynomial 2 7 1/8)))
    (is (= (polynomials:degree p) 2))
    (is (= (polynomials:leading-coefficient p) 2))
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

