#;
(exn-fail "incomplete coverage; missing coverage of Negative-Integer")
#lang typed/racket

(define-syntax (cond* stx)
  (syntax-case stx ()
    [(_ x clause ...)
     #`(cond clause ... [else (typecheck-fail #,stx "incomplete coverage" #:covered-id x)])]))

(: f : (U String Integer) -> Boolean)
(define (f x)
  (cond* x
         [(string? x) #t]
         [(exact-nonnegative-integer? x) #f]))
