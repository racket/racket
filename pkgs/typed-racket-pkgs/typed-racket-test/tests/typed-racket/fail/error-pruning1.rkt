#;
(exn-pred #px"^((?!Integer).)+$")
#lang typed/racket

;; Tests that function type pruning in errors is working. If the string
;; "Integer" shows up the error (which should be about Number), then
;; pruning is broken.
;; Because `exn-pred' only checks the first line of the error, only one
;; error expression per file (otherwise, the first line is a summary).

(add1 'foo1)
