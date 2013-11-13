(defpackage :factorisation/rational-polynomials
  (:nicknames :fac-poly)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :fac-sqf
        :fac-ffp
        :generic-math :fractions
        :polynomials :finite-fields)
  (:export
   #:lift
   #:factorise
   #:compute-mignotte-bounded-prime
   #:maxnorm
   #:factorise/big-prime-method
   #:combine-factors))

(in-package :factorisation/rational-polynomials)

;;; first some normalisation steps are in order
(defun make-monic/integer (polynomial &optional (leading-coeff (leading-coefficient polynomial)))
  "Transform poly a_0 X^n + ... to (a_0)^(n-1) a_0 (X/a_0)^n + ... to
  make it monic. The inverse of this operation is the same, but called
  with (/ original-leading-coeff)."
  ;; this corresponds to multiplying a_i with a_i^( (n-1) - i )
  (let ((factor (/ leading-coeff)))
    (values
     (make-instance 'polynomial :var (var polynomial)
                    ;; careful, here we assume map operates
                    ;; sequentially from the beginning
                    :coefficients (map 'vector (lambda (x) (prog1 (* x factor)
                                                        (setf factor (* factor leading-coeff))))
                                       (coefficients polynomial)))
     leading-coeff)))

(defun lcm-of-coeffs (polynomial)
  (reduce #'lcm (coefficients polynomial) :key #'cl:denominator))

(defun eliminate-denominator (polynomial)
  "Multiplity the `polynomial' with a factor, so it is defined over
the integers. Second value is the factor we multiply with"
  (let ((m (lcm-of-coeffs polynomial)))
    (values (poly*constant polynomial m) m)))

(defun multiply-var (polynomial multiplier &optional (var (var polynomial)))
  "Given a poly in X, produce a poly in Y where X = m Y."
  (make-instance 'polynomial :var var
                 :coefficients (iter (for coeff in-vector (coefficients polynomial) downto 0)
                                     (for m initially 1 then (* m multiplier))
                                     (collect (* m coeff) at start result-type vector))))


(defun factorise/rational-polynomial (polynomial)
  ;; first produce a polynomial over the integers
  (mvbind (polynomial denom) (eliminate-denominator polynomial)
    ;; then get rid of content
    (let* ((content (content polynomial))
           (polynomial (gm:/ polynomial content))
           ;; and make sure its monic
           (leading-coeff (leading-coefficient polynomial))
           (polynomial (make-monic/integer polynomial leading-coeff))
           ;; and split into squarefree factors
           (sqf-factors (square-free-factorise polynomial))
           ;; then factorise each of them
           (factorisation (map-on-factors #'factorise/big-prime-method sqf-factors))
           ;; now we have to reverse the make-monic/integer operation: if
           ;; we apply it to a primitive polynomial, then we essentially
           ;; factorise a_0^(d-1) f(X) = g(Y) = g_1(Y) ... g_r(Y) monic in
           ;; Y = a_0 X. The crucial observation is that the content of
           ;; g_1(a_0 X) ... g_r(a_0 X) is precisely a_0^(d-1), so all we
           ;; have to do is divide each factor by its content -- and
           ;; account for the sign of a_0
           (factorisation (map-on-factors (lambda (factor)
                                            (let ((transformed (multiply-var factor leading-coeff)))
                                              (list (make-factor :base (gm:/ transformed (content transformed))))))
                                          factorisation))
           ;; and account for possible constant factors
           (constant-coeff (gm:* (if (and (minusp leading-coeff)
                                          (evenp (degree polynomial)))
                                     -1 1)
                                 (gm:/ content denom))))
      (if (one-p constant-coeff)
          factorisation
          (list* (make-factor :base constant-coeff)
                 factorisation)))))

(defun maxnorm (seq)
  (reduce #'max seq :key #'abs))

(defun compute-mignotte-bounded-prime (poly)
  (let* ((n+1 (cl:+ (degree poly) 1))
         (lc (abs (leading-coefficient poly)))
         (coeff-bound (maxnorm (coefficients poly)))
         (mignotte-lower-bound (ceiling
                                (cl:* (cl:sqrt n+1) (cl:expt 2 n+1)
                                      lc coeff-bound))))
    (iter (for p initially (nt-p:next-prime mignotte-lower-bound)
               then (nt-p:next-prime (cl:+ 2 p)))
          (for red-poly next (-> 'finite-fields:integer-mod poly :mod p))
          (until (square-free-p red-poly))
          (finally (return (values red-poly p))))))

(defun factorise/big-prime-method (polynomial)
  "Given a square-free polynomial with integer coefficients, find a
  big prime `p' s.t. we can successfully `lift' a factorisation mod
  `p' to the integers. We use Mignotte's bound here."
  (combine-factors polynomial
                   (mapcar #'factor-base
                           (factorisation/berlekamp:berlekamp
                            (compute-mignotte-bounded-prime polynomial)))))

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
  ;; NOTE `simple-factors' is a list
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

;; general polynomial factorisation
(defun factorise (polynomial)
  "Factorise a `polynomial', automatically detecting whether the base
field is F_p or Q."
  (compact-factors
   (if (modulus polynomial)
       (fac-ffp:factorise/poly-over-finite-field polynomial)
       (factorise/rational-polynomial polynomial))))
