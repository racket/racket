#;
(exn:pred (lambda (e) (regexp-match? "Type Checker" e)))
#lang typed/racket

;; This should raise a type error and not an internal error

(: f (#:foo False -> False))
(define (f #:foo foo) foo)

;; bad call that's missing a mandatory keyword
(f)
