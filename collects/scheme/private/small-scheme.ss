
;;----------------------------------------------------------------------
;; assembles all basic forms we have so far

(module small-scheme '#%kernel
  (#%require "stx.ss" "qq-and-or.ss" "cond.ss" "define-et-al.ss")

  (#%provide (all-from "qq-and-or.ss")
             (all-from "cond.ss")
             (all-from "define-et-al.ss")))

