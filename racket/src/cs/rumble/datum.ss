(define datums (make-weak-hash))

(define intern-regexp? #f)
(define (set-intern-regexp?! p) (set! intern-regexp? p))

(define (datum-intern-literal v)
  (cond
   [(or (and (number? v)
             ;; `eq?` doesn't work on flonums
             (not (flonum? v)))
        (string? v)
        (char? v)
        (bytes? v)
        (intern-regexp? v))
    (or (weak-hash-ref-key datums v)
        (let ([v (cond
                  [(string? v) (string->immutable-string v)]
                  [(bytes? v) (bytes->immutable-bytes v)]
                  [else v])])
          (hash-set! datums v #t)
          v))]
   [else v]))
