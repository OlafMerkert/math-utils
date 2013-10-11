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
    ;; TODO use smaller numbers for this 
    ;; (is (= (expt a b) (gm:expt a b)))
    ))

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
  (is (gm:zero-p 0))
  (is (gm:zero-p (gm:zero 20)))
  (is (gm:= 0 (gm:zero 77))))

(test one
  (is (gm:one-p 1))
  (is (gm:one-p (gm:one 20)))
  (is (gm:= 1 (gm:one 77))))

;;; todo some more tests of the equality test
(test equal
  (is (gm:= 0 0))
  (is (gm:= 1 1))
  (is (gm:= 17 17))
  (is (not (gm:= 13 17)))
  (is (not (gm:= 1 0)))
  (is (not (gm:= 7 0)))
  (is (not (gm:= 1 7)))
  (is (not (gm:= 1 -1)))
  (is (not (gm:= 3 3 5)))
  (is (gm:= 3 3 3)))
