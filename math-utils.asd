(defsystem math-utils
  :depends-on (ol-utils cl-utilities iterate)
  :components ((:file "generic-math")
               (:file "general-algorithms"
                      :depends-on ("generic-math"))
               (:file "infinite-math" :depends-on ("generic-math"))
               (:file "infinite-sequence" :depends-on ("generic-math"
                                                       "infinite-math"))
               (:file "elementary-combinatorics")
               (:file "permutations")
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
                                                   "finite-fields"
                                                   "elementary-combinatorics"))
               (:file "heights" :depends-on ("fractions"
                                             "polynomials"))
               (:module "elliptic-curves" :depends-on ("generic-math" "number-theory") 
                        :components ((:file "weierstrass")))
               (:module "factorisation" :depends-on ("generic-math"
                                                     "finite-fields"
                                                     "polynomials"
                                                     "fractions"
                                                     "linear-algebra"
                                                     "number-theory")
                        :serial t
                        :components ((:file "datastructures")
                                     (:file "squarefree-factorisation")
                                     (:file "degree-separation")
                                     (:file "berlekamp")
                                     (:file "finite-field-polynomials")
                                     (:file "rational-polynomials")))
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
                                     (:file "chinese-remainder-thm" :depends-on ("vectors"))))
               (:file "packages" :depends-on ("generic-math"
                                              "polynomials"
                                              "finite-fields"
                                              "linear-algebra"
                                              "infinite-math"
                                              "factorisation"
                                              "fractions"
                                              "number-theory"))
               ))
