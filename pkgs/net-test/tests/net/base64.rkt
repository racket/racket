#lang racket/base
(require net/base64)

(define (test expect f . args)
  (define got (apply f args))
  (unless (equal? expect got)
    (error "fail" expect got)))

(for ([base64-encode (list base64-encode
                           (lambda (bstr [linesep #"\r\n"])
                             (define out (open-output-bytes))
                             (base64-encode-stream (open-input-bytes bstr) out linesep)
                             (get-output-bytes out)))]
      [base64-decode (list base64-decode
                           (lambda (bstr)
                             (define out (open-output-bytes))
                             (base64-decode-stream (open-input-bytes bstr) out)
                             (get-output-bytes out)))])
  (test #"" base64-encode #"")
  (test #"" base64-encode #"" #"<>")
  (test #"" base64-decode #"")
  (test #"" base64-decode #"a")
  (test #"i" base64-decode #"ab")
  (test #"i\267" base64-decode #"abc")
  (test #"i\267" base64-decode #"abc=d")
  (test #"eA==\n" base64-encode #"x" #"\n")
  (test #"eHk=\n" base64-encode #"xy" #"\n")
  (test #"eHl6\n" base64-encode #"xyz" #"\n")
  (test #"eHh4\n" base64-encode #"xxx" #"\n")
  (test #"eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4\n"
        base64-encode #"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" #"\n")
  (test #"eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4eHh4\neA==\n"
        base64-encode #"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" #"\n")
  (test #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5\r\nIG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=\r\n"
        base64-encode #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath")
  (test #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5<>IG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=<>"
        base64-encode #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath" #"<>")
  (test #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5_X_IG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=_X_"
        base64-encode #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath" '_X_)
  (test #"Why do axe murderers only attack\nWhen you're partially nude\nOr you're taking a bath"
        base64-decode #"V2h5IGRvIGF4ZSBtdXJkZXJlcnMgb25seSBhdHRhY2sKV2hlbiB5b3UncmUgcGFydGlhbGx5\r\nIG51ZGUKT3IgeW91J3JlIHRha2luZyBhIGJhdGg=\r\n"))
