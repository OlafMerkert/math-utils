(defsystem math-utils
  :depends-on (ol-utils cl-utilities iterate)
  :components ((:file "generic-math")
               (:file "infinite-math" :depends-on ("generic-math"))
               (:file "elementary-combinatorics")
               (:module "number-theory"
                        :depends-on ("generic-math")
                        :components ((:file "packages")
                                     (:file "basic"         :depends-on ("packages"))
                                     (:file "primes"        :depends-on ("packages"))
                                     (:file "factorisation" :depends-on ("packages"))
                                     (:file "multiplicative-functions" :depends-on ("packages"))))
               (:file "finite-fields" :depends-on ("generic-math"
                                                   "number-theory"))
               (:file "fractions" :depends-on ("generic-math"))
               (:file "localisations" :depends-on ("generic-math"
                                                   "number-theory"
                                                   "fractions"
                                                   "finite-fields"))
               (:file "variables")
               (:file "polynomials" :depends-on ("generic-math"
                                                 "variables"
                                                 "finite-fields"
                                                 "fractions"))
               (:file "multivariate-polynomials" :depends-on ("generic-math"
                                                              "variables"
                                                              "fractions"
                                                              "polynomials"))
               (:file "power-series"  :depends-on ("generic-math"
                                                   "polynomials"
                                                   "finite-fields"))
               (:module "elliptic-curves" :depends-on ("generic-math" "number-theory") 
                        :components ((:file "weierstrass")))
               (:module "factorisation" :depends-on ("generic-math"
                                                     "finite-fields"
                                                     "polynomials"
                                                     "fractions"
                                                     "linear-algebra"
                                                     "number-theory")
                        :components ((:file "datastructures")
                                     (:file "squarefree-factorisation"
                                            :depends-on ("datastructures"))))
               (:file "valuations" :depends-on ("infinite-math"
                                                "number-theory"
                                                "polynomials" "power-series"
                                                "fractions"))
               (:file "valuations-coeff" :depends-on ("infinite-math"
                                                      "valuations"
                                                      "polynomials" "power-series"))
               (:module "linear-algebra" :depends-on ("generic-math"
                                                      "number-theory"
                                                      "finite-fields")
                        :components ((:file "vectors")
                                     (:file "elementary-matrices" :depends-on ("vectors"))
                                     (:file "linear-solve" :depends-on ("vectors" "elementary-matrices"))
                                     (:file "chinese-remainder-thm" :depends-on ("vectors"))))))
