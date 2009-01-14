#;
(exn-pred 1)
#lang typed-scheme


(: f (Foo -> String))
(define (f x) (string-append x))

(f 1)
