(module eopl-without-exp (lib "eopl.ss" "eopl")

  ;; remove "exp" from the eopl language level, because we use it as
  ;; a mutable variable.

  (provide (all-from-except (lib "eopl.ss" "eopl") exp))

)