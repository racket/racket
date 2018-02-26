
(define (immutable? v)
  (let ([v (strip-impersonator v)])
    (or (intmap? v)
        (immutable-string? v)
        (immutable-bytevector? v)
        (immutable-vector? v)
        (immutable-box? v))))
