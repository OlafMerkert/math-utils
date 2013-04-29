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
    (acond ((zero-p derivate)
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
