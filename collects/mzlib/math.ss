
(module math scheme/base
  (require scheme/math)
  (provide (except-out (all-from-out scheme/math)
                       euler)
           (rename-out [euler e])))
