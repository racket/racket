
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, letrec-syntax, etc.

(module letstx-scheme '#%kernel
  (#%require "define-et-al.rkt" "core-syntax.rkt" "cond.rkt")

  (#%provide (all-from "define-et-al.rkt") (all-from "core-syntax.rkt") (all-from "cond.rkt")
             letrec-syntaxes letrec-syntax let-syntaxes let-syntax))
