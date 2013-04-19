(defpackage :chinese-remainder-theorem
  (:nicknames :crt)
  (:shadowing-import-from :vectors :vector)
  (:use :cl :ol :iterate
        :finite-fields
        :vectors)
  (:export))

(in-package :chinese-remainder-theorem)

;;; chinese remainder theorem

(defmemfun ensure-pairwise-prime (numbers)
  (if (null numbers) t
      (and (every (lambda (m) (cl:= (gcd m (first numbers)) 1)) (rest numbers))
           (ensure-pairwise-prime (rest numbers)))))

(defun crt-transform-matrix% (moduli)
  "compute a_i s.t. sum a_i x_i = x mod (* MODULI) where x_i = x mod M
  for all M in MODULI."
  (ecase (length moduli)
    (1 (list 1))
    (2 (multiple-value-bind (d u v) (nt:xgcd (first moduli) (second moduli))
         (declare (ignore d))
         (list (* v (second moduli))
               (* u (first moduli)))))))

(defun crt-transform-matrix (moduli)
  (make-instance 'matrix :entries (coerce (crt-transform-matrix% moduli) 'cl:vector)))

(defun crt-to-product (integer-mod moduli)
  (unless (ensure-pairwise-prime moduli)
    (error "CRT only works with pairwise squarefree moduli, but got ~A." moduli))
  (unless (= (modulus integer-mod) (reduce #'* moduli))
    (error "The product of moduli ~A must be the modulus ~A." moduli (modulus integer-mod)))
  ;; now just reduce
  (make-instance 'vector :entries
                 (map 'cl:vector (lambda (n) (int% (remainder integer-mod) n)) moduli)))

;;; use vectors, because then we already have multiplication and
;;; addition in the product ring.

(defun elemwise-remainder (vector)
  (elementwise-operation (vector)
    (remainder vector)))

(defun crt-from-product (imod-vector)
  (let* ((moduli (map 'list #'modulus (entries imod-vector)))
         (n (reduce #'* moduli)))
    (unless (ensure-pairwise-prime moduli)
      (error "CRT only works with pairwise squarefree moduli, but got ~A." moduli))
    (int% (gm:* (crt-transform-matrix moduli)
                (elemwise-remainder imod-vector))
          n)))

;;; TODO figure out a system that allows to declare explicit levels
;;; for math objects -- i.e. a set where they live, which might have
;;; constructed from another set. We want to say what we model, so far
;;; we only have means to model stuff, but without semantic
;;; information.
