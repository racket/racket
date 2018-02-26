
(define (mask->arity mask)
  (let loop ([mask mask] [pos 0])
    (cond
     [(= mask 0) null]
     [(= mask -1) (|#%app| arity-at-least pos)]
     [(bitwise-bit-set? mask 0)
      (let ([rest (loop (bitwise-arithmetic-shift-right mask 1) (add1 pos))])
        (cond
         [(null? rest) pos]
         [(pair? rest) (cons pos rest)]
         [else (list pos rest)]))]
     [else
      (loop (bitwise-arithmetic-shift-right mask 1) (add1 pos))])))

(define (arity->mask a)
  (cond
   [(exact-nonnegative-integer? a)
    (bitwise-arithmetic-shift-left 1 a)]
   [(arity-at-least? a)
    (bitwise-xor -1 (sub1 (bitwise-arithmetic-shift-left 1 (arity-at-least-value a))))]
   [(list? a)
    (let loop ([mask 0] [l a])
      (cond
       [(null? l) mask]
       [else
        (let ([a (car l)])
          (cond
           [(or (exact-nonnegative-integer? a)
                (arity-at-least? a))
            (loop (bitwise-ior mask (arity->mask a)) (cdr l))]
           [else #f]))]))]
   [else #f]))

(define (procedure-arity? a)
  (and (arity->mask a) #t))

(define-struct arity-at-least (value)
  :guard (lambda (value who)
           (check who exact-nonnegative-integer? value)
           value))
