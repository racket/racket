(define-thread-local datums (make-weak-hashtable equal-hash-code equal?))

(define intern-regexp? #f)
(define (set-intern-regexp?! p) (set! intern-regexp? p))

(define (datum-intern-literal v)
  (let ([intern* (lambda (v)
                  (with-interrupts-disabled*
                   (car (hashtable-cell datums v #f))))])
    (let ([intern (lambda (v)
                    (cond
                      [(current-future)
                       (block-future)
                       (let ([v (intern* v)])
                         (unblock-future)
                         v)]
                      [else (intern* v)]))])
      (cond
        [(or (and (number? v)
                  (not (fixnum? v)))
             (immutable-string? v)
             (immutable-bytevector? v)
             (intern-regexp? v))
         (intern v)]
        [(string? v) (intern (string->immutable-string v))]
        [(bytes? v) (intern (bytes->immutable-bytes v))]
        [else v]))))

