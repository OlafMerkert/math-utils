(in-package :factorisation-polynomials-modp)

;;; square-free factorisation (for polynomials over a finite field)

(defstruct factor  base (exponent 1))

(defun from-poly-in-x^p (poly p)
  (let ((n (degree poly)))
    (assert (nt:divides-p p n))
    (make-instance 'polynomial
                   :coefficients (make-nlazy-array (:index-var i :finite (cl:/ n p))
                                   (expt (nth-coefficient% poly (cl:* i p))
                                         (cl:/ (cl:- n i) p))))))

(defun map-on-exponents (mult-function factorisation)
  (mapcar (lambda (factor)
            (make-factor :base (factor-base factor)
                         :exponent (funcall mult-function (factor-exponent factor))))
          factorisation))

(defun map-on-factors (poly-function factorisation &optional (multiplicity 1))
  (mapcan (lambda (factor)
            (map-on-exponents
             (lambda (mult)
               (cl:* mult multiplicity (factor-exponent factor)))
             (funcall poly-function (factor-base factor))))
          factorisation))

(defun square-free-factorise (poly)
  "produce a factorisation of poly into squarefree factors."
  (let ((derivative (derivative poly)))
    (acond ((zero-p derivative)
            ;; TODO careful when going to q = p^e
            (let ((p (modulus (leading-coefficient poly))))
              (map-on-exponents (lambda (x) (* x p))
                                (square-free-factorise (from-poly-in-x^p poly p)))))
           ((non-constant-p (ggt poly derivative))
            (list*
             (make-factor :base (/ poly it))
             (square-free-factorise it)))
           (t (list (make-factor :base poly))))))

(defun two-monomials (deg-1 coeff-1 deg-2 coeff-2)
  (make-instance
   'polynomial
   :coefficients (make-nlazy-array (:index-var i :finite (max deg-1 deg-2))
                   (cond
                     ((cl:= i deg-1) coeff-1)
                     ((cl:= i deg-2) coeff-2)
                     (t 0)))))


(defun distinct-degree-factorise (poly)
  "produce a factorisation of a squarefree poly into factors, where
  all irreducibles dividing a factor have same degree."
  (let ((p (modulus (leading-coefficient poly)))
        (factors))
    (do* ((i 1 (+ i 1))
          (factor #1=(ggt (two-monomials 1 (int% -1 p)
                                         (expt p i) (int% 1 p))
                          poly) #1#)
          (poly #2=(/ poly factor) #2#))
         ((constant-p poly) #3=(unless (constant-p factor)
                                 (push factor factors)))
      #3#)
    factors))

;;; baby-step, giant-step
(defun compute-step-polys (poly p start end &optional (step 1))
  (iter (for i from start to end)
        (collect (poly-mod (make-monomial (expt p (* step i)) (int% 1 p))
                           poly)
          result-type vector)))

;;; TODO implement quotients of polynomial rings
(defun poly-mod (poly modulus)
  (nth-value 1 (/ poly modulus)))


(defun baby-step-giant-step (poly beta)
  (let* ((n (degree poly))
         (p todo)
         (baby-step (ceiling (cl:expt n beta)))
         (giant-step (ceiling (cl:/ n (cl:* 2 baby-step)))))
    (let* ((h-list  (compute-step-polys poly p 0 baby-step))
           (hh-list (compute-step-polys poly p 1 giant-step baby-step))
           (ii-list (map 'vector
                         (lambda (hh)
                           (reduce (lambda (a b) (poly-mod (* a b) poly))
                                   (subseq h-list 0 baby-step) :key (lambda (h) (- hh h))))
                         hh-list))
           (f poly)
           ;; coarse ddf
           (fj-list (iter (for ii in-vector ii-list)
                          (for fj = (ggt f ii))
                          (setf f (/ f fj))
                          (collect fj result-type vector)))
           (ff-list (make-array (* baby-step giant-step))))
      (iter (for k from 1 to n)
            (setf (aref ff-list k) 1))
      ;; fine ddf
      (iter (for j from 1 to giant-step)
            (for g in-vector fj-list)
            (for hh in-vector hh-list)
            (iter (for i from (- baby-step 1) downto 0)
                  (for h in-vector h-list)
                  (for ff = (ggt g (- hh h)))
                  (setf g (/ g ff))
                  (setf (aref ff-list (- (* baby-step j) i))
                        ff)))
      (unless (one-p f)
        (setf (aref ff-list (degree f)) f))
      ff-list)))
