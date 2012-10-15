(defsystem math-utils
  :serial t
  :depends-on (ol-utils cl-utilities iterate)
  :components ((:file "generic-math")
               (:module "number-theory"
                        :serial t
                        :components ((:file "basic")
                                     (:file "primes")
                                     (:file "factorisation")))
               (:file "finite-fields")
               (:file "localisations")
               (:file "polynomials")
               (:file "power-series")))
