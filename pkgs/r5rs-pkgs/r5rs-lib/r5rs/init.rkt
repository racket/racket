#lang scheme/base

(read-case-sensitive #f)
(read-accept-infix-dot #f)
(read-curly-brace-as-paren #f)
(read-square-bracket-as-paren #f)

(print-mpair-curly-braces #f)
;; Printing pairs with curly braces is a bad idea, because
;;  syntax errors then use curly braces!

(define-syntax out
  (syntax-rules ()
    [(_) (begin
           (require "main.rkt")
           (provide (all-from-out "main.rkt")))]))
(out)
