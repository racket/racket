
;;----------------------------------------------------------------------
;; -define, when, unless, let/ec, define-struct

(module define-et-al '#%kernel
  (#%require (for-syntax '#%kernel "stx.rkt" 
                         (only "pico.rkt" member memw)
                         "core-macros.rkt")

             "core-macros.rkt")

  (#%provide -define -define-syntax when unless call/ec let/ec))
