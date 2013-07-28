#;
(exn:pred #rx"Type (All (a) Flonum) could not be converted to a contract")
#lang typed/racket
(require/typed racket/base [list (All (a) Float)])
(* 3.3 list)

