
;;----------------------------------------------------------------------
;; #%define : define and define-syntax

(module define '#%kernel
  (#%require "core-syntax.rkt")

  (#%provide define 
             define-syntax 
             define-values-for-syntax
             define-for-syntax))
