(defpackage :math-user
  #.gm:+gm-shadow-imports+
  #.gm:+frac-shadow-imports+
  (:shadowing-import-from :linear-algebra/vectors :vector)
  (:use :cl :ol
        :generic-math
        :fractions
        :finite-fields
        :polynomials
        :infinite-math
        :number-theory
        :linear-algebra/vectors
        :linear-algebra/elementary-matrices
        :linear-algebra/linear-solve
        )
  (:export))
