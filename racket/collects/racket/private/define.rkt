
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%declare #:require=define)

  (#%require (for-syntax '#%kernel
                         
                         "letstx-scheme.rkt" "stxcase-scheme.rkt" "stx.rkt" "qqstx.rkt"
                         "norm-define.rkt")
             "core-macros.rkt")

  (#%provide define 
             define-syntax 
             define-values-for-syntax
             define-for-syntax)

  )
