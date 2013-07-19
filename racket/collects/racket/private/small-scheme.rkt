
;;----------------------------------------------------------------------
;; assembles all basic forms we have so far

(module small-scheme '#%kernel
  (#%require "qq-and-or.rkt" "cond.rkt" "define-et-al.rkt")

  (#%provide (all-from "qq-and-or.rkt")
             (all-from "cond.rkt")
             (all-from "define-et-al.rkt")))

