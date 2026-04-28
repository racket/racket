
;;----------------------------------------------------------------------
;; assembles all basic forms we have so far

;; Not used in racket/base but kept for backwards-compatibility

(module small-scheme '#%kernel
  (#%require "core-syntax.rkt")
  
  (#%provide (all-from "core-syntax.rkt")))

