#lang racket/base
(require net/base64)

(define (test expect f . args)
  (unless (equal? expect (apply f args))
    (error "fail")))

(test #"" base64-encode #"")
(test #"" base64-encode #"" #"<>")
(test #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5\r\nIG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=\r\n"
      base64-encode #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath")
(test #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5<>IG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=<>"
      base64-encode #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath" #"<>")
