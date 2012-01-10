#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-min-value flomap-max-value flomap-extreme-values
         flomap-nonzero-rect)

(: flomap-min-value (flomap -> Flonum))
(define (flomap-min-value fm)
  (for/fold ([v-min +inf.0]) ([v  (in-flvector (flomap-values fm))])
    (min v-min v)))

(: flomap-max-value (flomap -> Flonum))
(define (flomap-max-value fm)
  (for/fold ([v-max -inf.0]) ([v  (in-flvector (flomap-values fm))])
    (max v-max v)))

(: flomap-extreme-values (flomap -> (values Flonum Flonum)))
(define (flomap-extreme-values fm)
  (for/fold: ([v-min : Flonum  +inf.0] [v-max : Flonum  -inf.0]
                                       ) ([v : Flonum  (in-flvector (flomap-values fm))])
    (values (min v-min v) (max v-max v))))

(: flomap-nonzero-rect (flomap -> (values Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum
                                          Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum)))
(define (flomap-nonzero-rect fm)
  (match-define (flomap vs c w h) fm)
  (with-asserts ([c  nonnegative-fixnum?] [w  nonnegative-fixnum?] [h  nonnegative-fixnum?])
    (define: k-min : Nonnegative-Fixnum  c)
    (define: x-min : Nonnegative-Fixnum  w)
    (define: y-min : Nonnegative-Fixnum  h)
    (define: k-max : Nonnegative-Fixnum  0)
    (define: x-max : Nonnegative-Fixnum  0)
    (define: y-max : Nonnegative-Fixnum  0)
    (let: y-loop : Void ([y : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  0])
      (when (y . fx< . h)
        (let: x-loop : Void ([x : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
          (cond [(x . fx< . w)
                 (let: k-loop : Void ([k : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
                   (cond [(k . fx< . c)  (define v (unsafe-flvector-ref vs i))
                                         (when (not (v . = . 0.0))
                                           (set! k-min (fxmin k-min k))
                                           (set! x-min (fxmin x-min x))
                                           (set! y-min (fxmin y-min y))
                                           (set! k-max (fxmax k-max (fx+ 1 k)))
                                           (set! x-max (fxmax x-max (fx+ 1 x)))
                                           (set! y-max (fxmax y-max (fx+ 1 y))))
                                         (k-loop (fx+ k 1) (fx+ i 1))]
                         [else  (x-loop (fx+ x 1) i)]))]
                [else  (y-loop (fx+ y 1) i)]))))
    (values k-min x-min y-min k-max x-max y-max)))
