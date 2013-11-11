(defpackage :factorisation/degree-separation
  (:nicknames :fac-degsep)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :fac-ds
        :generic-math :fractions
        :polynomials :finite-fields)
  (:export
   #:distinct-degree-factorise
   #:baby-step-giant-step
   #:equal-degree-factorise
   #:map-equal-degree-factorise))

(in-package :factorisation/degree-separation)

(defun distinct-degree-factorise (poly)
  "produce a factorisation of a squarefree, monic `poly' into a vector
products of irreducible factors of same degree grouped by this degree
coinciding with the index."
  (let* ((p (modulus poly))
         (x (make-monomial 1 (int% 1 p))))
    (iter (initially (push 1 factors))
          (for h initially (expt-mod x p poly) then (expt-mod h p poly))
          (for f initially poly then (/ f g))
          ;; extract the part that divides x^(p^i) - x
          (for g next (ggt (- h x) f))
          (until (one-p g))
          (collect g result-type vector into factors))
    ;; todo what about constant factor? -> we assume `poly' is monic here!
    ))

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
  "Compute a distinct degree factorisation of a squarefree, monic `poly', where the return is an array of length 1+deg `poly',
which holds the products of irreducible factors of degree `i' at index
`i'. `beta' should be a number between 0 and 1."
  (assert (<= 0 beta 1))
  (let* ((n (degree poly))
         (p (modulus poly))
         (baby-step (ceiling (cl:expt n beta)))
         (giant-step (ceiling (cl:/ n baby-step 2))))
    (let* ((h-list  (compute-step-polys poly p 0 (- baby-step 1)))
           (hh-list (compute-step-polys poly p 1 giant-step baby-step))
           ;; after having our list of powers, build a vector of the
           ;; products of differences x^(p^i) - x^(p^j) where i-j
           ;; covers a babystep interval below i. We use this for the
           ;; coarse DDF (distinct-degree-factorisation)
           (ii-list (map 'vector
                         (lambda (hh)
                           (reduce (lambda (a b) (div (* a b) poly))
                                   h-list :key (lambda (h) (- hh h))))
                         hh-list))
           (f poly)
           ;; coarse ddf: irreducible polynomials of degree l divide
           ;; ii iff l divides i-j. Starting with the smallest i, we
           ;; split these factors out
           (fj-list (iter (for ii in-vector ii-list)
                          (for fj = (ggt f ii))
                          (setf f (/ f fj))
                          (collect fj result-type vector)))
           ;; The array will hold at position `d' the product of
           ;; irreducible factors of degree `d'.
           (ff-list (make-array n :initial-element 1)))
      ;; fine ddf: 
      (iter (for j from 1 to giant-step)
            (for g in-vector fj-list)
            (for hh in-vector hh-list)
            (iter (for i from (- baby-step 1) downto 0)
                  (for h in-vector h-list)
                  (for ff = (ggt g (- hh h)))
                  ;; if g is constant, we can skip the rest of the iteration
                  (when (constant-p g) (return))
                  (setf g (/ g ff))
                  (setf (aref ff-list (- (* baby-step j) i)) ff)))
      ;; note that baby-step * giant-step ~ n/2, so we divide out only
      ;; irreducible factors of degree <= n/2. It might however happen
      ;; that there is one irreducible factor of degree > n/2, for
      ;; which we account here (this also takes care of a remaining
      ;; constant factor)
      (unless (one-p f)
        (setf (aref ff-list (degree f)) f))
      ff-list)))

;;; Equal-Degree factorisation due to Cantor, Zassenhaus
(defun equal-degree-factorise-helper (poly d &optional (p (modulus poly)) (n (degree poly)))
  (let* ((a (random-polynomial/non-constant p n))
         (g1 (ggt a poly)))
    (if (not (one-p g1))
        g1
        (let* ((r (cl:/ (cl:- (cl:expt p d) 1) 2))
               (b (expt-mod a r poly))
               (g2 (ggt (- b 1) poly)))
          (if (or (one-p g2) (= g2 poly))
              nil
              g2)))))

(defun equal-degree-factorise (poly d)
  "Factorise square-free, monic `poly', of which every irreducible factor has degree
  `d'."
  (do ((p (modulus poly))
       (irreducibles)
       (reducibles (make-queue))
       (poly poly (dequeue reducibles)))
      ((not poly) (mapcar (clambda (make-factor :base x!)) irreducibles))
      (let ((n (degree poly)))
        (if (cl:= d n)
            (push poly irreducibles)
            (let ((g (equal-degree-factorise-helper poly d p n)))
              (if g
                  (progn ; on success we refine to two new factors
                    (enqueue g reducibles)
                    (enqueue (/ poly g) reducibles))
                  (enqueue poly reducibles)))))))


(defun map-equal-degree-factorise (ed-fac-fun ed-poly-vector)
  (iter (for d from 0)
        (for poly in-vector ed-poly-vector)
        (unless (one-p poly)
          (nconcing (funcall ed-fac-fun poly d)))))

;; todo check whether x^(p^i) is really the right thing to compute
