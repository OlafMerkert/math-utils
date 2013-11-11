(in-package :generic-math)

;;; some generally useful math functions
(defun expt-mod (base exponent modulus)
  (square-multiply base exponent
                   (clambda (div (generic-* x!a x!b) modulus))
                   (clambda (div (generic-* x!a x!a) modulus))))

(defun order-find (group-element multiplication identity-test &optional (order-bound))
  "Determine the order of some `group-element' in a group with given
`multiplication' law and identity described by `identity-test'. If no
positive \"power\" of `group-element' up to `order-bound' satisfies
`identity-test', return NIL, otherwise the smallest \"exponent\"
found."
  (iter (for n from 1 to order-bound)
        (for g initially group-element then (funcall multiplication g group-element))
        (when (funcall identity-test g)
          (return n))
        (finally (return nil))))
