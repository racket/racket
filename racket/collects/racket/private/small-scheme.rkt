
;;----------------------------------------------------------------------
;; assembles all basic forms we have so far

;; Not used in racket/base but kept for backwards-compatibility

(module small-scheme '#%kernel
  (#%require "core-macros.rkt" "core-macros.rkt" "core-macros.rkt")
  
  (#%provide (all-from "core-macros.rkt")
             (all-from "core-macros.rkt")
             (all-from "core-macros.rkt")))

