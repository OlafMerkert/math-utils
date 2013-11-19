(in-package :math-utils-tests)

(def-suite infinite-sequence :in math-utils)

(in-suite infinite-sequence)

(defparameter array-max-index (expt 2 20))

(test infseq-generation
  (let ((seq1 (ins:inf+seq '(a b c) (n) n))
        (seq2 (ins:inf-seq '(a b c) (n) n)))
    ;; first the special start values
    (is (eq 'a (ins:sref seq1 0)))
    (is (eq 'b (ins:sref seq1 1)))
    (is (eq 'c (ins:sref seq1 2)))
    (is (eq 'a (ins:sref seq2 0)))
    (is (eq 'b (ins:sref seq2 -1)))
    (is (eq 'c (ins:sref seq2 -2)))
    ;; then randomly over the rest of the sequence
    (for-all ((index (gen-integer :min 3 :max array-max-index)))
      (is (= index (ins:sref seq1 index)))
      (is (= (- index) (ins:sref seq2 (- index)))))
    ;; and check how settings stuff works
    (setf (ins:sref seq1 17) 89)
    (is (equal 89 (ins:sref seq1 17)))))

(test inf-subsequence
  (let* ((seq1 (ins:inf+seq nil (n) n))
         (seq2 (ins:shift seq1 8))
         (seq3 (ins:subsequence seq1 8 infinite-math:infinity+))
         (seq4 (ins:subsequence seq3 -8 90)))
    (is (= 0 (ins:sref seq2 8)))
    (is (= 8 (ins:sref seq3 8)))
    (for-all ((index (gen-integer :min 8 :max array-max-index)))
      (is (= (ins:sref seq3 index) (ins:sref seq4 index) ))
      (is (= (ins:sref seq1 (- index 8)) (ins:sref seq2 index)))
      (is (= (ins:sref seq1 index) (ins:sref seq3 index))))
    (is (= 0 (ins:sref seq1 :start)))
    (is (= 8 (ins:sref seq3 :start)))
    (is (= 90 (ins:sref seq4 :end)))
    (is (= 83 (ins:sequence-length seq4)))))

(test map-sequence
  (let ((seq1 (ins:inf+seq '(4 2 3) (n) (+ n 2)))
        (seq2 (ins:inf+seq '(16 4 9) (n) (expt (+ n 2) 2)))
        (seq4 (ins:inf-seq '(4 2 3) (n) (+ n 2)))
        (seq5 (ins:inf-seq '(16 4 9) (n) (expt (+ n 2) 2))))
    (let ((seq3 (ins:map-sequence (clambda (* x! x!)) seq1))
          (seq6 (ins:map-sequence (clambda (* x! x!)) seq4)))
      (for-all ((index (gen-integer :min 0 :max array-max-index)))
        (is (= (ins:sref seq2 index) (ins:sref seq3 index)))
        (is (= (ins:sref seq5 (- index)) (ins:sref seq6 (- index))))))
    (let ((seq3 (ins:map-sequences/or #'max seq1 seq2))
          (seq6 (ins:map-sequences/or #'min seq4 seq5)))
      (for-all ((index (gen-integer :min 0 :max array-max-index)))
        (is (= (ins:sref seq2 index) (ins:sref seq3 index)))
        (is (= (ins:sref seq4 (- index)) (ins:sref seq6 (- index))))))
    (let ((seq3 (ins:map-sequences/and #'max seq2 (ins:shift seq4 20))))
      (for-all ((index (gen-integer :min 0 :max 20)))
        (is (= (ins:sref seq2 index) (ins:sref seq3 index)))))))



(test strip-if-seq
  (let ((seq1 #(0 0 0 8 8 0 0 0 0))
        (seq2 #(0 0 8 8 0 0 0 0 0 0)))
    (mvbind (stripped number) (ins:strip-if #'zerop seq1 :from :start)
      (is (= 3 number))
      (is (sequence= #(8 8 0 0 0 0) stripped)))
    (mvbind (stripped number) (ins:strip-if #'zerop seq1 :from :end)
      (is (= 4 number))
      (is (sequence= #(0 0 0 8 8) stripped)))
    (mvbind (stripped number) (ins:strip-if #'zerop seq2 :from :start)
      (is (= 2 number))
      (is (sequence= #(8 8 0 0 0 0 0 0) stripped)))
    (mvbind (stripped number) (ins:strip-if #'zerop seq2 :from :end)
      (is (= 6 number))
      (is (sequence= #(0 0 8 8) stripped)))
    (mvbind (stripped number) (ins:strip-if #'oddp seq2 :from :start)
      (is (= 0 number))
      (is (sequence= seq2 stripped)))
    (mvbind (stripped number) (ins:strip-if #'oddp seq2 :from :end)
      (is (= 0 number))
      (is (sequence= stripped seq2)))
    ))

