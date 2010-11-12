#lang racket

(begin
  (require syntax/parse)
  (define (mkstx n) (datum->syntax #f (for/list ([i (in-range n)]) #'hello)))

  (define stx1 (mkstx 10))
  (define stx2 (mkstx 100))
  (define stx3 (mkstx 1000))
  (define stx4 (mkstx 10000))

  (define bad-stx (datum->syntax #f (append (for/list ([i (in-range 10000)]) #'hello) (list #'#f))))
  (define-syntax-class plain-id
    #:attributes ()
    (pattern x #:when (identifier? #'x)))
  (define-syntax-class commit-id #:commit
    #:attributes ()
    (pattern x #:when (identifier? #'x)))
  (define (parse/id x n)
    (for ([i (in-range n)])
      (syntax-parse x [(z:id ...) 'ok] [_ 'bad!])))
  (define (parse/plain-id x n)
    (for ([i (in-range n)])
      (syntax-parse x [(z:plain-id ...) 'ok] [_ 'bad!])))
  (define (parse/commit-id x n)
    (for ([i (in-range n)])
      (syntax-parse x [(z:commit-id ...) 'ok] [_ 'bad!])))
  (define (parse/listpred x n)
    (for ([i (in-range n)])
      (syntax-case x ()
        [(x ...) (andmap identifier? (syntax->list #'(x ...))) 'ok]
        [_ 'bad!])))
  (define (parse/pred x n)
    (for ([i (in-range n)])
      (let loop ([x x])
        (syntax-case x ()
          [(x . y) (identifier? #'x) (loop #'y)]
          [() 'ok])))))

(begin
  (define (stx->list1 x)
    (cond [(syntax? x)
           (stx->list1 (syntax-e x))]
          [(pair? x)
           (cons (car x) (stx->list1 (cdr x)))]
          [(null? x)
           null]))
  (define (stx->list2 x)
    (let ([d (syntax-e x)])
      (cond [(pair? d)
             (cons (car d) (stx->list2 (datum->syntax x (cdr d) x)))]
            [(null? d)
             null])))
  (define (stx->list3 x)
    (cond [(syntax? x)
           (stx->list3 (syntax-e x))]
          [(box? x)
           (stx->list3 (unbox x))]
          [(pair? x)
           (cons (car x) (stx->list3 (box (cdr x))))]
          [(null? x)
           null])))

#|
> (time (parse/id stx 10))
cpu time: 2829 real time: 2826 gc time: 20
> (time (parse/plain-id stx 10))
cpu time: 3072 real time: 3090 gc time: 40
> (time (parse/commit-id stx 10))
cpu time: 3076 real time: 3125 gc time: 24
> (time (parse/listpred stx 10))
cpu time: 4 real time: 7 gc time: 0

> (time (parse/pred stx 10))
cpu time: 2760 real time: 2757 gc time: 8
> (collect-garbage)
> (collect-garbage)
> (time (parse/pred stx 10))
cpu time: 2808 real time: 2813 gc time: 64
> (collect-garbage)
> (collect-garbage)
> (time (parse/id stx 10))
cpu time: 2880 real time: 2876 gc time: 84
> (time (parse/id stx 10))
cpu time: 2821 real time: 2810 gc time: 8
> (time (parse/id stx 10))
cpu time: 2816 real time: 2812 gc time: 16
> (time (parse/plain-id stx 10))
cpu time: 2912 real time: 2906 gc time: 24
> (time (parse/plain-id stx 10))
cpu time: 2908 real time: 2910 gc time: 24
> (time (parse/plain-id stx 10))
cpu time: 3128 real time: 3144 gc time: 32
> (time (parse/plain-id stx 10))
cpu time: 2925 real time: 2922 gc time: 36
> (time (parse/plain-id stx 10))
cpu time: 2908 real time: 2901 gc time: 12
|#


#|
given pattern (E ...) where E = A _ | A
the sequence (A A B A A B A A B ...)
causes each E to backtrack
|#

(begin
  (define-syntax-class id/nat
    #:attributes ()
    (pattern x:id)
    (pattern n:nat))
  (define-splicing-syntax-class trip
    #:attributes ()
    (pattern (~seq #:a _))
    (pattern (~seq #:a)))
  (define (mktripstx n)
    (apply append (for/list ([i (in-range n)]) (list #'#:a #'#:a #'#:b))))
  (define tripstx3 (mktripstx 1000))
  (define tripstx4 (mktripstx 10000))
  (define (parse/trip x n)
    (for ([i (in-range n)])
      (syntax-parse x
        [(t:trip ...) 'ok])))

  (define (mknatstx n)
    (datum->syntax #f (for/list ([i (in-range n)]) (add1 i))))
  (define (solve n rep)
    (let ([stx (mknatstx n)])
      (for ([i (in-range rep)])
        (syntax-parse stx
          [((~or x:nat y:nat) ...)
           #:when (= (apply + (syntax->datum #'(x ...)))
                     (apply + (syntax->datum #'(y ...))))
           (syntax->datum #'(y ...))])))))

;; (solve 35 _) and (solve 36 _) seem manageable

#|

#| before markparams |#

> (time (parse/trip tripstx3 100))
cpu time: 812 real time: 817 gc time: 92
> (time (parse/trip tripstx3 100))
cpu time: 788 real time: 791 gc time: 76
> (time (parse/trip tripstx3 100))
cpu time: 772 real time: 774 gc time: 52
> (time (parse/trip tripstx4 10))
cpu time: 1148 real time: 1147 gc time: 436
> (time (parse/trip tripstx4 10))
cpu time: 1368 real time: 1385 gc time: 520
> (time (parse/trip tripstx4 10))
cpu time: 1240 real time: 1240 gc time: 516

> (time (solve 35 20))
cpu time: 1572 real time: 1568 gc time: 332
> (time (solve 35 20))
cpu time: 1548 real time: 1551 gc time: 304
> (time (solve 35 20))
cpu time: 1548 real time: 1548 gc time: 304
> (time (solve 36 20))
cpu time: 716 real time: 714 gc time: 80
> (time (solve 36 20))
cpu time: 704 real time: 703 gc time: 64
> (time (solve 36 20))
cpu time: 700 real time: 701 gc time: 72


#| with partial defunctionalization (failures-so-far) |#

> (time (parse/trip tripstx3 100))
cpu time: 1932 real time: 1933 gc time: 88
> (time (parse/trip tripstx3 100))
cpu time: 1900 real time: 1903 gc time: 76
> (time (parse/trip tripstx3 100))
cpu time: 2052 real time: 2052 gc time: 224
> (time (parse/trip tripstx4 10))
cpu time: 2536 real time: 2535 gc time: 708
> (time (parse/trip tripstx4 10))
cpu time: 2620 real time: 2622 gc time: 756
> (time (parse/trip tripstx4 10))
cpu time: 2372 real time: 2372 gc time: 556

> (time (solve 35 20))
cpu time: 3409 real time: 3404 gc time: 340
> (time (solve 35 20))
cpu time: 3244 real time: 3244 gc time: 312
> (time (solve 35 20))
cpu time: 3240 real time: 3242 gc time: 312
> (time (solve 36 20))
cpu time: 1588 real time: 1589 gc time: 76
> (time (solve 36 20))
cpu time: 1576 real time: 1579 gc time: 64
> (time (solve 36 20))
cpu time: 1580 real time: 1575 gc time: 52


#| with failure function as markparam |#

> (time (parse/trip tripstx3 100))
cpu time: 1840 real time: 1843 gc time: 116
> (time (parse/trip tripstx3 100))
cpu time: 1792 real time: 1789 gc time: 48
> (time (parse/trip tripstx3 100))
cpu time: 1956 real time: 1960 gc time: 228
> (time (parse/trip tripstx4 10))
cpu time: 2352 real time: 2353 gc time: 608
> (time (parse/trip tripstx4 10))
cpu time: 2488 real time: 2495 gc time: 748
> (time (parse/trip tripstx4 10))
cpu time: 2416 real time: 2415 gc time: 684

> (time (solve 35 20))
cpu time: 3205 real time: 3201 gc time: 324
> (time (solve 35 20))
cpu time: 3208 real time: 3203 gc time: 316
> (time (solve 35 20))
cpu time: 3048 real time: 3050 gc time: 184
> (time (solve 36 20))
cpu time: 1692 real time: 1695 gc time: 208
> (time (solve 36 20))
cpu time: 1564 real time: 1566 gc time: 84
> (time (solve 36 20))
cpu time: 1540 real time: 1542 gc time: 64


#| with fail & cut-prompt as stxparams |#

> (time (parse/trip tripstx3 100))
cpu time: 532 real time: 534 gc time: 68
> (time (parse/trip tripstx3 100))
cpu time: 524 real time: 524 gc time: 48
> (time (parse/trip tripstx3 100))
cpu time: 656 real time: 657 gc time: 168
> (time (parse/trip tripstx4 10))
cpu time: 992 real time: 993 gc time: 512
> (time (parse/trip tripstx4 10))
cpu time: 860 real time: 861 gc time: 380
> (time (parse/trip tripstx4 10))
cpu time: 1004 real time: 999 gc time: 516

> (time (solve 35 20))          
cpu time: 1132 real time: 1129 gc time: 140
> (time (solve 35 20))
cpu time: 1320 real time: 1316 gc time: 340
> (time (solve 35 20))
cpu time: 1300 real time: 1299 gc time: 296
> (time (solve 36 20))
cpu time: 588 real time: 588 gc time: 76
> (time (solve 36 20))
cpu time: 580 real time: 584 gc time: 68
> (time (solve 36 20))
cpu time: 580 real time: 586 gc time: 56
|#
