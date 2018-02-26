(define/who string-copy!
  (case-lambda
   [(dest d-start src)
    (string-copy! dest d-start src 0 (and (string? src) (string-length src)))]
   [(dest d-start src s-start)
    (string-copy! dest d-start src s-start (and (string? src) (string-length src)))]
   [(dest d-start src s-start s-end)
    (check who mutable-string? :contract "(and/c string? (not/c immutable?))" dest)
    (check who exact-nonnegative-integer? d-start)
    (check who string? src)
    (check who exact-nonnegative-integer? s-start)
    (check who exact-nonnegative-integer? s-end)
    (let ([d-len (string-length dest)])
      (check-range who "string" dest d-start #f d-len)
      (check-range who "string" src s-start s-end (string-length src))
      (let ([s-len (fx- s-end s-start)])
        (check-space who "string" d-start d-len s-len)
        (#%string-copy! src s-start dest d-start s-len)))]))

(define/who substring
  (case-lambda
   [(s start) (substring s start (and (string? s) (string-length s)))]
   [(s start end)
    (check who string? s)
    (check who exact-nonnegative-integer? start)
    (check who exact-nonnegative-integer? end)
    (check-range who "string" s start end (string-length s))
    (#%substring s start end)]))
