(defpackage :number-theory/factorisation
  (:nicknames :nt-f)
  (:use :cl :ol
        :iterate
        :number-theory)
  (:export
   :factorise
   :factor-into-prime-powers))

(in-package :number-theory/factorisation)

(define-condition factorisation-large-input ()
  ((input-integer :initarg :input-integer
                  :reader input-integer))
  (:report (lambda (condition stream)
             (format stream "Factorisation of integer ~A will potentially take very long."
                     (input-integer condition)))))

(defun factorise% (n &optional (s (floor (sqrt n))))
  "Find all the prime factors of the given integer and return them in
a list, sorted by size. Multiple factors appear multiple times. We search for factors "
  (with-simple-restart (factorise-anyway "Factorise integer despite its size.")
    (when (> (integer-length n) 50)
      (error 'factorisation-large-input :input-integer n)))
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

(defun factorise (n &optional (compress t))
  "As factorise%, but if compress is t, compress the list of
  factors."
  (ecase compress
    ((nil) #1=(factorise% n))
    ((t)  (compress #1#))
    ((:singletons) (compress #1# :singletons t))))

(defun factor-into-prime-powers (n)
  "give a list of all the maximal prime power p^e dividing N."
  (mapcar (lambda (x) (expt (car x) (cdr x)))
          (factorise n :singletons)))
