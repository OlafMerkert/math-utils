(in-package :math-utils-tests)

(def-suite number-theory :in math-utils)

(in-suite number-theory)

(test divides-p
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (is (nt:divides-p a (* a b)))))

(test xgcd
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (multiple-value-bind (d u v) (nt:xgcd a b)
      (is (= d (+ (* u a) (* v b))))
      (is (nt:divides-p d a))
      (is (nt:divides-p d b)))))

(test xgcd/rec
  (for-all ((a (gen-integer))
            (b (gen-integer)))
    (multiple-value-bind (d u v) (number-theory/basic:xgcd/rec a b)
      (is (= d (+ (* u a) (* v b))))
      (is (nt:divides-p d a))
      (is (nt:divides-p d b)))))

(test sqrt-int
  (for-all ((a (gen-integer :min 0)))
    (is (= a (gm:sqrt (expt a 2))))))

(test sqrt-int
  (for-all ((a (gen-integer  :min 0))
            (b (gen-integer :min 1)))
    (is (= (gm:sqrt (expt (/ a b) 2))
           (/ a b)))))

(test prime-testing+erastothenes
  (is (every #'nt:prime-p
             (number-theory/primes:erastothenes-sieve
              1000))))

;; TODO examples for fermat test

(test factorise
  (is (equal (nt:factorise 12396578192)
             '((2 . 4) 239 3241783)))
  (is (equal (nt:factorise 5916293695)
             '(5 (13 . 2) 223 31397)))
  (is (equal (nt:factorise 12341275236)
             '((2 . 2) (3 . 3) 114271067))))

;; (run!)

