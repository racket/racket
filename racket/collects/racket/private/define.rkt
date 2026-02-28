
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%require "define-et-al.rkt")

  (#%provide define 
             define-syntax 
             define-values-for-syntax
             define-for-syntax))
