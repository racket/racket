(module test-tools mzscheme

  (require (lib "class.ss"))

  (provide (all-from (lib "class.ss"))
           mixin?)

  (define mixin? null?)
  
  )
