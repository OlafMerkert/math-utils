(in-package :factorisation-polynomials-modp)

;;; square-free factorisation (for polynomials over a finite field)

(defun monomial-diff (deg+ deg- &optional (coeff+ 1) (coeff- -1))
  (let* ((deg (max deg+ deg-))
         (i+ (- deg deg+))
         (i- (- deg deg-)))
    (make-instance 'polynomial
                   :coefficients (make-array/sparse (deg) 0
                                   (i+ coeff+)
                                   (i- coeff-)))))

(defmethod modulus ((poly polynomial))
  ;; todo perhaps check all coefficients have same modulus.
  (modulus (leading-coefficient poly)))

(defun distinct-degree-factorise (poly)
  "produce a factorisation of a squarefree poly into factors, where
  all irreducibles dividing a factor have same degree."
  (let ((p (modulus poly))
        (factors))
    (do* ((i 1 (+ i 1))
          (factor #1=(ggt (monomial-diff (expt p i) 1 (int% 1 p) (int% -1 p))
                          poly) #1#)
          (poly #2=(/ poly factor) #2#))
         ((constant-p poly) #3=(unless (constant-p factor)
                                 (push factor factors)))
      #3#)
    factors))

;;; baby-step, giant-step
(defun compute-step-polys (poly p start end &optional (step 1))
  "Produce a list of the powers X^(p^i) where i goes of the supplied
range (end inclusive). For better efficiency, work mod `poly'."
  (let ((q (expt p step)))
    (iter (for i from start to end)
          (for h initially (div (make-monomial (expt q start)
                                               (int% 1 p))
                                poly)
               then (expt-mod h q poly))
          (collect h result-type vector))))

(defun baby-step-giant-step (poly beta)
  (let* ((n (degree poly))
         (p (modulus poly))
         (baby-step (ceiling (cl:expt n beta)))
         (giant-step (ceiling (cl:/ n (cl:* 2 baby-step)))))
    (let* ((h-list  (compute-step-polys poly p 0 baby-step))
           (hh-list (compute-step-polys poly p 1 giant-step baby-step))
           ;; after having our list of powers, build a vector of the
           ;; products of differences. todo check explanation
           (ii-list (map 'vector
                         (lambda (hh)
                           (reduce (lambda (a b) (div (* a b) poly))
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
