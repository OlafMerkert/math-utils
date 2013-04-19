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

(defmemfun crt-transform-matrix (moduli n)
  "compute a_i s.t. sum a_i x_i = x mod (* MODULI) = n where x_i = x mod M
  for all M in MODULI."
  (make-instance
   'matrix
   :entries (map 'cl:vector
                 (lambda (q)
                   (let ((m (/ n q)))
                     (multiple-value-bind (d r s) (nt:xgcd q m)
                       (declare (ignore d r))
                       (int% (* m s) n))))
                 moduli)))

(defun crt-to-product (integer-mod moduli)
  (unless (ensure-pairwise-prime moduli)
    (error "CRT only works with pairwise prime moduli, but got ~A." moduli))
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
      (error "CRT only works with pairwise prime moduli, but got ~A." moduli))
    (gm:* (crt-transform-matrix moduli n)
          (elemwise-remainder imod-vector))))

;;; TODO figure out a system that allows to declare explicit levels
;;; for math objects -- i.e. a set where they live, which might have
;;; constructed from another set. We want to say what we model, so far
;;; we only have means to model stuff, but without semantic
;;; information.
