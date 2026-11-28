
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
    (guard-arity-mask a)
    (bitwise-arithmetic-shift-left 1 a)]
   [(arity-at-least? a)
    (guard-arity-mask (arity-at-least-value a))
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

(define (guard-arity-mask n)
  ;; a mask for arity `n` has `n` bits
  (unless (and (fixnum? n) (fx< n 32768))
    (guard-large-allocation 'procedure-reduce-arity "arity mask" (quotient n 8) 1)))

(define (procedure-arity? a)
  (let ([simple? (lambda (a) (or (exact-nonnegative-integer? a)
                                 (arity-at-least? a)))])
    (or (simple? a)
        (and (list? a)
             (andmap simple? a)))))

(define-struct arity-at-least (value)
  :guard (lambda (value who)
           (check who exact-nonnegative-integer? value)
           value))
