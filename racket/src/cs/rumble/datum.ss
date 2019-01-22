(define-thread-local datums (make-weak-hash))

(define intern-regexp? #f)
(define (set-intern-regexp?! p) (set! intern-regexp? p))

(define (datum-intern-literal v)
  (cond
   [(or (number? v)
        (string? v)
        (char? v)
        (bytes? v)
        (intern-regexp? v))
    (with-interrupts-disabled
     (or (weak-hash-ref-key datums v)
         (let ([v (cond
                   [(string? v) (string->immutable-string v)]
                   [(bytes? v) (bytes->immutable-bytes v)]
                   [else v])])
           (hash-set! datums v #t)
           v)))]
   [else v]))
