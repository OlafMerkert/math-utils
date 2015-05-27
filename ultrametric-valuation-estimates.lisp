(defpackage :ultrametric-valuation-estimates
  (:nicknames :valest)
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:use :cl :ol :iterate
        :infinite-math
        :generic-math :fractions
        :polynomials :power-series)
  (:export
   #:valuation-estimate
   #:ve))

(in-package :ultrametric-valuation-estimates)

(defclass valuation-estimate (generic-math-object)
  ((bound :initarg :bound
          :initform infinity+
          :accessor bound)
   (sharp :initarg :sharp
          :initform nil
          :accessor sharp))
  (:documentation "Represent the value of a discrete ultrametric
  valuation. This should allow to "))

(defmethod initialize-instance :after ((object valuation-estimate) &key))

(defmethod print-object ((object valuation-estimate) stream)
  (with-slots (bound sharp) object
    (format stream "v~:[>~;~]= ~A" sharp bound)))

(defmacro! ve-bin-debug (o!expression a op b)
  `(progn
     (format t "~&~A ~A ~A = ~A" ,a ,op ,b ,g!expression)
     ,g!expression))

;; (defmacro def-gm-operation/sym (op type vars &body body)
;;   `(defmethod ,(symb 'generic- op) ,(mapcar #`(,a1 ,type) vars)
;;      (ve-bin-debug (progn ,@body)
;;                    ,(first vars) ',op ,(second vars))))

(defmacro def-gm-operation/sym (op type vars &body body)
  `(defmethod ,(symb 'generic- op) ,(mapcar #`(,a1 ,type) vars)
     ,@body))

(defun ve (n &optional s)
  (make-instance 'valuation-estimate :bound n :sharp s))

(defmethod simplified-p ((valuation-estimate valuation-estimate)) t)

(def-gm-operation/sym =  valuation-estimate (a b)
  (i= (bound a) (bound b))
  ;; todo also check for sharpness?
  )

(defmethod zero-p ((a valuation-estimate))
  (i= (bound a) infinity+))

(defmethod one-p ((a valuation-estimate))
  (zerop (bound a)))

(def-gm-operation/sym +  valuation-estimate (a b)
  (let ((bound-a (bound a))
        (bound-b (bound b)))
    (cond ((i< bound-a bound-b) a)
          ((i< bound-b bound-a) b)
          ((and (infinite-p bound-a) (infinite-p bound-b)) a)
          (t (ve (min bound-a bound-b))))))

(def-gm-operation/sym -  valuation-estimate (a b)
  (generic-+ a b))

(def-gm-operation/sym *  valuation-estimate (a b)
  (ve (generic-+ (bound a) (bound b))
      (and (sharp a) (sharp b))))

(def-gm-operation/sym /  valuation-estimate (a b)
  (ve (generic-- (bound a) (bound b))
      (and (sharp a) (sharp b))))

(defmethod sqrt ((a valuation-estimate))
  (ve (/ (bound a) 2) (sharp a)))

(defmethod expt ((a valuation-estimate) (n integer))
  (ve (* n (bound a)) (sharp a)))
