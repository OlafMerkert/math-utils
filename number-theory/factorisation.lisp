(defpackage :number-theory/factorisation
  (:nicknames :nt-f)
  (:use :cl :ol
        :iterate
        :number-theory)
  (:export
   :factorise
   :factor-into-prime-powers
   :factorise-over-s
   :divisors))

(in-package :number-theory/factorisation)

(define-condition factorisation-large-input ()
  ((input-integer :initarg :input-integer
                  :reader input-integer))
  (:report (lambda (condition stream)
             (format stream "Factorisation of integer ~A will potentially take very long."
                     (input-integer condition)))))

(defun factorise% (n &optional s)
  "Find all the prime factors of the given integer and return them in
a list, sorted by size. Multiple factors appear multiple times. We search for factors "
  (with-simple-restart (factorise-anyway "Factorise integer despite its size.")
    (unless s ; default value for s
      (setf s (floor (sqrt n)))
      (when (> (integer-length n) 50)
        (error 'factorisation-large-input :input-integer n))))
  ;; TODO add restart allowing to factorise up to a certain factor
  ;; size -- that is allow adjusting s
  (if (minusp n)
      (list* -1 (factorise% (- n)))
      (do ((i 2)
           (m n)
           (divs nil))
          ;; TODO verify everything goes well even when we have
          ;; factors smaller than S.
          ((or (= m 1) (> i s))
           (if (= m 1) (nreverse divs) (nreverse (cons m divs))))
        ;; TODO perhaps signal when we are certain we have the entire
        ;; factorisation
        (multiple-value-bind (mm rr) (floor m i)
          (if (zerop rr)
              (progn
                (push i divs)
                (setf m mm))
              (incf i))))))

(defun compress-wrap (list compress)
  (ecase compress
    ((nil) #1=list)
    ((t)  (compress #1#))
    ((:singletons) (compress #1# :singletons t))))

(defun factorise (n &optional (compress t) factor-bound)
  "As factorise%, but if compress is t, compress the list of
  factors."
  (compress-wrap (factorise% n factor-bound) compress))

(defun factor-into-prime-powers (n)
  "give a list of all the maximal prime power p^e dividing N."
  (mapcar (lambda (x) (expt (car x) (cdr x)))
          (factorise n :singletons)))

(defun factorise-over-s% (n s)
  "Extract all the factors from the prime factor system S."
  (if (minusp n)
      (list* -1 (factorise-over-s (- n) s))
      (do ((p (pop s))
           (m n)
           (divs nil))
          ((or (= m 1) (not p))
           (if (= m 1) (nreverse divs) (nreverse (cons m divs))))
        (mvbind (mm rr) (floor m p)
          (if (zerop rr)
              (progn
                (push p divs)
                (setf m mm))
              (setf p (pop s)))))))

(defun factorise-over-s (n s &optional (compress t))
  "As factorise-over-s%, but if compress is t, compress the list of
  factors."
  (compress-wrap (factorise-over-s% n s) compress))

(defun divisors (n)
  "Produce a list of all natural numbers dividing the number `n'."
  (labels ((add-factor (factorisation divisors)
             (if factorisation
                 (dbind ((first . m) &rest others) factorisation
                   (if (zerop m) (add-factor others divisors)
                       (add-factor (cons (cons first (- m 1)) others)
                                   ;; todo this can probably be improved
                                   (remove-duplicates
                                    (append divisors
                                            (mapcar (lambda (x) (* first x))
                                                    divisors))))))
                 divisors)))
    (add-factor (factorise n :singletons) (list 1))))
