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
      (is (gm:zero-p (power-series:nth-coefficient simplified (power-series:degree simplified)))))))

(test series-equal
  (let ((ps1 (power-series:make-power-series     10  8 7 6 5 4 3 2 1))
        (ps2 (power-series:make-power-series/inf 10 (let ((a (- index 2)))
                                                      (if (plusp a) a 0))))
        (ps3 (power-series:make-power-series/inf 0 (if (< index -10000)
                                                       0 7)))
        (ps4 (power-series:make-power-series/inf 0 7))
        (ps5 (power-series:make-power-series/inf 0 (random 80))))
    (is (gm:= ps1 ps2))
    (is (gm:= ps1 ps1))
    (is (gm:= ps2 ps2))
    (is (gm:= ps3 ps4))
    (is (gm:= ps5 ps5))))

(test series-add
  (let ((ps1 (power-series:make-power-series 4  1 1 1 1 1))
        (ps2 (power-series:make-power-series/inf 4  1))
        (ps3 (power-series:make-power-series/inf -1  1))
        (ps4 (power-series:make-power-series 4  1 1 1 1 8))
        (ps5 (power-series:make-constant-series 7))
        (ps6 (power-series:make-power-series/inf 4 (if (zerop index) 8 1))))
    (is (gm:= ps2 (gm:+ ps1 ps3)))
    (is (gm:= ps1 (gm:- ps2 ps3)))
    (is (gm:= ps3 (gm:- ps2 ps1)))
    (is (gm:= ps4 (gm:+ ps1 ps5)))
    (is (gm:= ps1 (gm:- ps4 ps5)))
    (is (gm:= ps5 (gm:- ps4 ps1)))
    (is (gm:= ps6 (gm:+ ps2 ps5)))
    (is (gm:= ps2 (gm:- ps6 ps5)))
    (is (gm:= ps5 (gm:- ps6 ps2)))))



(test series-mult
  (let ((ps1 (power-series:make-power-series 3  2 2 2 2))
        (ps2 (power-series:make-constant-series 5))
        (ps3 (power-series:make-power-series 3  10 10 10 10))
        (ps4 (power-series:exponential-series 2))
        (ps5 (power-series:exponential-series 3))
        (ps6 (power-series:exponential-series 5))
        (ps7 (power-series:exponential-series -2)))
    ;; scalar multiplication
    (is (gm:= ps3 (gm:* ps1 ps2)))
    (is (gm:= ps3 (gm:* ps1 5)))
    (is (gm:= ps1 (gm:/ ps3 ps2)))
    (is (gm:= ps1 (gm:/ ps3 5)))
    ;; how about degrees?
    (is (= 6 (power-series:degree (gm:* ps1 ps3))))
    ;; the exponential series is useful for checking the
    ;; multiplication algorithm
    (is (gm:= ps6 (gm:* ps4 ps5)))
    (is (gm:= ps5 (gm:/ ps6 ps4)))
    (is (gm:= ps7 (gm:/ ps4)))))

(test series-sqrt
  (let* ((ps1 (power-series:make-power-series 4  1 8 9 4 2 0 0 7 2 7))
         (ps2 (gm:^ ps1 2))
         (ps3 (power-series:exponential-series 1))
         (ps4 (power-series:exponential-series 2)))
    (is (gm:= ps1 (gm:sqrt ps2)))
    (is (gm:= ps3 (gm:sqrt ps4)))))

(test series-rem-trunc
  (let ((po1 (polynomials:make-polynomial 1 2 3 4 5))
        (ps1 (power-series:make-power-series 4  1 2 3 4 5))
        (ps2 (power-series:make-power-series/inf 4 (- 5 index)))
        (ps3 (power-series:make-power-series/inf -1 (- 5 index))))
    (is (gm:= ps1 (gm:-> 'power-series:power-series po1)))
    (is (gm:= po1 (power-series:series-truncate ps1)))
    (is (gm:= po1 (power-series:series-truncate ps2)))
    (is (gm:= ps3 (power-series:series-remainder ps2)))
    (is (gm:= ps3 (power-series:series-remainder ps3)))
    (is (gm:= 0 (power-series:series-remainder ps1)))
    (is (gm:= 0 (power-series:series-truncate ps3)))))
