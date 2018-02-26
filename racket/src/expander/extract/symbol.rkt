#lang racket
(require "../common/set.rkt")

(provide all-used-symbols
         distinct-symbol
         substitute-symbols)

;; We only have to consider symbols and pairs, because we're looking
;; of variables in a `linklet` form. Also, since there's no shadowing
;; of primitives, we can be especially simplistic about "parsing" to
;; detect `quote`.
(define (all-used-symbols s)
  (let loop ([s s] [used (seteq)])
    (cond
     [(symbol? s) (set-add used s)]
     [(pair? s)
      (if (eq? (car s) 'quote)
          used
          (loop (cdr s) (loop (car s) used)))]
     [else used])))

;; Pick a symbol like `sym` that's not in the set `used`
(define (distinct-symbol sym used)
  (let loop ([n 1])
    (define s (string->symbol (format "~a$~a" sym n)))
    (if (set-member? used s)
        (loop (add1 n))
        s)))

(define (substitute-symbols s substs)
  (let loop ([s s])
    (cond
     [(symbol? s) (hash-ref substs s s)]
     [(pair? s)
      (if (eq? (car s) 'quote)
          s
          (cons (loop (car s)) (loop (cdr s))))]
     [else s])))
