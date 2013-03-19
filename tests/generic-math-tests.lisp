(in-package :math-utils-tests)

(def-suite generic-math :in math-utils)

(in-suite generic-math)

(test arith-ops
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (is (= (+ a b) (gm:+ a b)))
    (is (= (- a b) (gm:- a b)))
    (is (= (* a b) (gm:* a b)))
    (is (= (/ a b) (gm:/ a b)))
    (is (= (expt a b) (gm:expt a b)))))

(test equal
  (is (gm:= 0 0))
  (is (gm:= 1 1))
  (for-all ((a (gen-integer :min 1))
            (b (gen-integer :max -1)))
    (is (gm:= a a))
    (is (gm:= b b))
    (is (not (gm:= a b)))
    (is (not (gm:= a a b)))
    (is (not (gm:= b a a b)))
    (is (not (gm:= b a b b)))))

(test zero
  (is (zero-p 0))
  (is (zero-p (zero 20)))
  (is (gm:= 0 (zero 77))))

(test one
  (is (one-p 1))
  (is (one-p (one 20)))
  (is (gm:= 1 (one 77))))
