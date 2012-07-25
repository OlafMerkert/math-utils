(defsystem math-utils
  :serial t
  :depends-on (ol-utils cl-utilities)
  :components ((:file "generic-math")
               (:module "number-theory"
                        :serial t
                        :components ((:file "basic")
                                     (:file "primes")
                                     (:file "factorisation")))
               (:file "finite-fields")
               (:file "polynomials")
               (:file "power-series")))
