#lang racket/base
(require racket/private/place-local)

;; Represent a range as a list of `(cons start end)`
;; pairs, where `start` and `end` are inclusive.

(provide empty-range
         range-invert
         range-add
         range-union
         range-add-span
         range-in?
         range-singleton
         range-includes?
         range-overlaps?
         range-within?
         range->list
         
         compile-range
         rng-in?

         range-place-init!)

(define empty-range null)

(define (range-invert r limit-c)
  (let loop ([r r] [start 0])
    (cond
     [(null? r)
      (cond
       [(start . > . limit-c) null]
       [else (list (cons start limit-c))])]
     [(= start (caar r))
      (loop (cdr r) (add1 (cdar r)))]
     [else
      (cons (cons start (sub1 (caar r)))
            (loop (cdr r) (add1 (cdar r))))])))

(define (range-in? r v)
  (for/or ([p (in-list r)])
    (and (v . >= . (car p))
         (v . <= . (cdr p)))))

(define (range-add r v)
  (cond
   [(not v) r]
   [(range-in? r v) r]
   [else (range-union r (list (cons v v)))]))
           
(define (range-union r1 r2)
  (cond
   [(null? r1) r2]
   [(null? r2) r1]
   [((caar r1) . <= . (caar r2))
    (cond
     [((add1 (cdar r1)) . >= . (caar r2))
      ;; First elements overlap or are contiguous
      (cond
       [((cdar r1) . <= . (cdar r2))
        ;; First of second extends further
        (range-union (cons (cons (caar r1) (cdar r2))
                           (cdr r2))
                     (cdr r1))]
       [else
        ;; First of first subsumes first of second
        (range-union r1 (cdr r2))])]
     [else
      ;; First of first is wholly before first of second
      (cons (car r1)
            (range-union (cdr r1) r2))])]
   [else
    ;; First of second starts earlier, so change places
    (range-union r2 r1)]))
  
(define (range-add-span range from-c to-c)
  (range-union range (list (cons from-c to-c))))

(define (range-singleton range)
  (and (pair? range)
       (null? (cdr range))
       (= (caar range) (cdar range))
       (caar range)))

(define (range-includes? range low hi)
  (cond
   [(null? range) null]
   [(low . > . (cdar range)) (range-includes? (cdr range) low hi)]
   [else
    (and (low . >= . (caar range))
         (hi . <= . (cdar range)))]))

(define (range-within? range low hi)
  (cond
   [(null? range) #t]
   [((caar range) . < . low) #f]
   [((cdar range) . > . hi) #f]
   [else (range-within? (cdr range) low hi)]))

(define (range-overlaps? range low hi)
  (cond
   [(null? range) null]
   [(low . > . (cdar range)) (range-overlaps? (cdr range) low hi)]
   [else
    (or (and (low . >= . (caar range))
             (low . <= . (cdar range)))
        (and (hi . >= . (caar range))
             (hi . <= . (cdar range))))]))

(define (range->list range)
  range)

;; ----------------------------------------

(define-place-local rngs (make-weak-hash))

(define (range-place-init!)
  (set! rngs (make-weak-hash)))

(define (compile-range range)
  (or (hash-ref rngs range #f)
      (let ([rng (make-bytes 256 0)])
        (for* ([p (in-list range)]
               [i (in-range (car p) (add1 (cdr p)))])
          (bytes-set! rng i 1))
        (hash-set! rngs range rng)
        rng)))
          
(define (rng-in? rng v)
  (eq? 1 (bytes-ref rng v)))
