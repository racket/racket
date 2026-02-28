
;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, letrec-syntax, etc.

(module letstx-scheme '#%kernel
  (#%require "core-syntax.rkt" "core-syntax.rkt" "core-syntax.rkt")

  (#%provide (all-from "core-syntax.rkt") (all-from "core-syntax.rkt") (all-from "core-syntax.rkt")
             letrec-syntaxes letrec-syntax let-syntaxes let-syntax))
