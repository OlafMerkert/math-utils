(defpackage :factorisation/rational-polynomials
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :generic-math :fractions
        :polynomials :finite-fields)
  (:export))

(in-package :factorisation/rational-polynomials)

;;; with lift we produce a polynomial defined over the integers from
;;; one that is given over a finite field, keeping the size of
;;; coefficients as small as possible.
(defmethod lift ((n integer))
  n)

(defmethod lift ((n integer-mod))
  (with-slots ((n remainder) (p modulus)) n
    ;; want a number between -p/2 and p/2
    (if (> (* 2 n) p)
        (- n p)
        n)))

(defmethod lift ((poly polynomial))
  (with-slots (coefficients var) poly
    (make-instance 'polynomial :var var
                   :coefficients (map 'vector #'lift coefficients))))

(defstruct factor-product  product missing occuring)

(defmacro asetf (place value &rest other)
  "Anaphorically bind the `place' to `this' in the `setf' form"
  `(let ((this ,place))
     (setf ,place ,value)
     ,@(when other
             `((asetf ,@other)))))

(defun fp-eval (fp)
  (aif #1=(factor-product-missing fp)
       (prog1
         (asetf (factor-product-product fp)
                (* this it))
         (setf #1# nil))
       (factor-product-product fp)))

(defun make-empty-product (nr-of-factors)
  (make-factor-product :product 1
                       :missing nil
                       :occuring (make-array nr-of-factors
                                             :initial-element nil
                                             :element-type 'boolean)))

(defun fp-occuring-p (index fp)
  (aref (factor-product-occuring fp) index))

(defun make-fp (simple-factor index fp)
  (let ((occuring (copy-seq (factor-product-occuring fp))))
    ;; perhaps we can save the check
    (if #2=(aref occuring index)
        (error "index was already set -- will get forbidden double factor.")
        (setf #2# t))
    (make-factor-product
           :product (factor-product-product fp)
           :missing simple-factor
           :occuring occuring)))

;; NOTE: for this method to work, we need to have chosen `p'
;; sufficiently high, otherwise we are in trouble.

(defun combine-factors (poly simple-factors)
  (let ((empty-product (make-empty-product (length simple-factors)))
        (actual-factors))
    (block finding-factors
      (labels ((simple-products ()
                 (iter (for i from 0)
                       (for s in simple-factors)
                       (collect (make-fp s i empty-product))))
               (test-one ()
                 (when (one-p poly)
                   (return-from finding-factors)))
               (test-divide (products)
                 (iter (for f in products)
                       ;; todo do we need to use the ggt here, or can we
                       ;; just test for divisibility?
                       (aif (non-constant-p (ggt (lift (fp-eval f)) poly))
                            (progn (setf poly (/ poly it))
                                   (push (make-factor :base it) actual-factors))
                            ;; the idea is to discard any factors we
                            ;; used already
                            (collect f))
                       (test-one)))
               (form-new-factors (products)
                 (let (new-products)
                   (iter (for i from 0)
                         (for g in simple-factors)
                         (iter (for f in products)
                               (unless (fp-occuring-p i f)
                                 (push (make-fp g i f) new-products))))
                   new-products))
               (rec (products)
                 (when products
                   (test-divide products)
                   (rec (form-new-factors products)))))
        (rec (simple-products))
        ;; if we get here, we did not find all factors, which should
        ;; not happen -- because `test-one' should have us jump out
        ;; after some division.
        (error "remaining factor ~A in `combine-factors'." poly)))
    actual-factors))

