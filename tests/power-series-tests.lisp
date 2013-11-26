(in-package :math-utils-tests)

(def-suite power-series :in math-utils)

(in-suite power-series)

(test make-series
  (let ((cs (power-series:make-constant-series 9))
        (ps1 (power-series:make-power-series 0  9))
        (ps2 (power-series:make-power-series 3  9 1 2 4 11 3))
        (ps3 (power-series:make-power-series/inf
              3
              (case index
                (3 9) (2 1) (1 2) (0 4) (-1 11) (-2 3)
                (t 0))))
        (ps4 (power-series:make-power-series/inf 9 index)))
    ;; test leading coefficient access
    (is (= 9 (power-series:leading-coefficient cs)))
    (is (= 9 (power-series:leading-coefficient ps1)))
    (is (= 9 (power-series:leading-coefficient ps2)))
    (is (= 9 (power-series:leading-coefficient ps3)))
    (is (= 9 (power-series:leading-coefficient ps4)))
    ;; test general coefficient access
    (is (= 0 (power-series:nth-coefficient cs -1)))
    (is (= 0 (power-series:nth-coefficient ps1 -1)))
    (is (= 1 (power-series:nth-coefficient ps2 2)))
    (is (= 2 (power-series:nth-coefficient ps2 1)))
    (is (= 4 (power-series:nth-coefficient ps2 0)))
    (is (= 1 (power-series:nth-coefficient ps3 2)))
    (is (= 2 (power-series:nth-coefficient ps3 1)))
    (is (= 4 (power-series:nth-coefficient ps3 0)))
    (for-all ((index1 (gen-integer :min (- array-max-index) :max 9))
              (index2 (gen-integer :min (- array-max-index) :max 0))
              (index3 (gen-integer :min (- array-max-index) :max 3)))
      (is (= index1 (power-series:nth-coefficient ps4 index1)))
      ;; comparison of different models
      (is (= (power-series:nth-coefficient cs index2)
             (power-series:nth-coefficient ps1 index2)))
      (is (= (power-series:nth-coefficient ps2 index3)
             (power-series:nth-coefficient ps3 index3))))
    ;; equality tests
    (is (gm:= cs ps1))
    (is (gm:= ps2 ps3))))

(test series-simplify
  (let ((ps1 (power-series:make-power-series% 10  0 0 0 8 9 7))
        (ps2 (power-series:make-power-series/inf 4 (if (>= index -3) 0 4)))
        (ps3 (power-series:make-power-series/inf 6 (/ (+ 1 (^ index 2)))))
        ;; (ps4 (power-series:make-constant-series 0))
        (ps5 (power-series:make-power-series/inf 0 0)))
    (mvbind (simplified leading-zeroes) (gm:simplify ps1)
      (is (= leading-zeroes 3))
      (is (= 7 (power-series:degree simplified)))
      (is (= 8 (power-series:leading-coefficient simplified))))
    (mvbind (simplified leading-zeroes) (gm:simplify ps2)
      (is (= leading-zeroes 8))
      (is (= -4 (power-series:degree simplified)))
      (is (= 4 (power-series:leading-coefficient simplified))))
    (mvbind (simplified leading-zeroes) (gm:simplify ps3)
      (is (= leading-zeroes 0))
      (is (= 6 (power-series:degree simplified)))
      (is (= 1/37 (power-series:leading-coefficient simplified))))
    (mvbind (simplified leading-zeroes) (gm:simplify ps5)
      (is (minusp leading-zeroes))
      ;; (is (= 7 (power-series:degree simplified)))
      (is (gm:zero-p (power-series:nth-coefficient simplified :end))))))

(test series-add)

(test series-mult)

