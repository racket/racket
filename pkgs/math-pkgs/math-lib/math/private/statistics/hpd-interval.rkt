#lang typed/racket

(require "../../flonum.rkt"
         "order-statistics.rkt"
         "statistics-utils.rkt")

(provide hpd-interval
         hpd-interval/sorted
         real-hpd-interval
         real-hpd-interval/sorted)

(: unweighted-hpd-interval (All (A) (-> Symbol (-> A A Real) Real (Listof A) (Values A A))))
(define (unweighted-hpd-interval name metric α xs)
  (define n (length xs))
  (define m (exact-ceiling (* n α)))
  (define as (take xs m))
  (define bs (drop xs m))
  (cond
    [(empty? as)  (error name "interval is empty; given ~e ~e" α xs)]
    [(empty? bs)  (values (first as) (last as))]
    [else
     (define a* (first as))
     (define b* (first bs))
     (define d* (abs (metric b* a*)))
     (define-values (a b _)
       (for/fold: ([a* : A  a*] [b* : A  b*] [d* : Real  d*]) ([a  (in-list as)]
                                                               [b  (in-list bs)])
         (define d (abs (metric b a)))
         (if (d . < . d*) (values a b d) (values a* b* d*))))
     (values a b)]))

(: weighted-hpd-interval (All (A) (-> Symbol (-> A A Real) Real (Listof A) (Listof Nonnegative-Real)
                                      (Values A A))))
(define (weighted-hpd-interval name metric α xs ws)
  (let* ([xs  (list->vector xs)]
         [ws  (weights->normalized-weights name ws)]
         [ws  (flvector-sums (list->flvector ws))])
    (define n (vector-length xs))
    
    (: find-new-endpoint (Index Index -> (Option Index)))
    ;; Returns the next index after i1 for which the sum of weights >= α
    (define (find-new-endpoint i0 i1)
      (define w0 (flvector-ref ws i0))
      (let loop ([i1 : Nonnegative-Fixnum  i1])
        (cond [(i1 . < . n)
               (cond [((- (flvector-ref ws i1) w0) . >= . α)  i1]
                     [else  (loop (+ i1 1))])]
              [else  #f])))
    
    (define i1 (find-new-endpoint 0 0))
    (cond
      [(not i1)  (values (vector-ref xs 0) (vector-ref xs (- n 1)))]
      [else
       (define a* (vector-ref xs 0))
       (define b* (vector-ref xs i1))
       (define d* (abs (metric b* a*)))
       (let loop ([i0 : Nonnegative-Fixnum  1] [i1 : Index  i1] [a* a*] [b* b*] [d* d*])
         ;(printf "i0 = ~v  i1 = ~v~na* = ~v  b* = ~v~nd* = ~v~n" i0 i1 a* b* d*)
         (cond [(i0 . >= . n)  (values a* b*)]
               [else
                (let ([i1  (find-new-endpoint i0 i1)])
                  (cond [(not i1)  (values a* b*)]
                        [else
                         ;(printf "α = ~v~n~n" (- (flvector-ref ws i1) (flvector-ref ws i0)))
                         (define a (vector-ref xs i0))
                         (define b (vector-ref xs i1))
                         (define d (abs (metric b a)))
                         (cond [(d . < . d*)  (loop (+ i0 1) i1 a b d)]
                               [else          (loop (+ i0 1) i1 a* b* d*)])]))]))])))


(: hpd-interval/sorted (All (A) (->* [(-> A A Real) Real (Sequenceof A)]
                                     [(Option (Sequenceof Real))]
                                     (Values A A))))
(define (hpd-interval/sorted metric α xs [ws #f])
  (cond [(or (α . <= . 0) (α . > . 1))
         (raise-argument-error 'hpd-interval/sorted "Real in (0,1]" 1 metric α xs ws)]
        [ws    (let-values ([(xs ws)  (sequences->weighted-samples 'sorted-hpd-interval xs ws)])
                 (weighted-hpd-interval 'hpd-interval/sorted metric α xs ws))]
        [else  (unweighted-hpd-interval 'hpd-interval/sorted metric α (sequence->list xs))]))

(: hpd-interval (All (A) (->* [(-> A A Any) (-> A A Real) Real (Sequenceof A)]
                              [(Option (Sequenceof Real))]
                              (Values A A))))
(define (hpd-interval lt? metric α xs [ws #f])
  (cond [(or (α . <= . 0) (α . > . 1))
         (raise-argument-error 'hpd-interval "Real in (0,1]" 2 lt? metric α xs ws)]
        [ws    (let-values ([(xs ws)  (sort-samples lt? xs ws)])
                 (weighted-hpd-interval 'hpd-interval metric α xs ws))]
        [else  (let ([xs  (sort-samples lt? xs)])
                 (unweighted-hpd-interval 'hpd-interval metric α xs))]))

(: real-hpd-interval/sorted (->* [Real (Sequenceof Real)]
                                 [(Option (Sequenceof Real))]
                                 (Values Real Real)))
(define (real-hpd-interval/sorted α xs [ws #f])
  (hpd-interval/sorted - α xs ws))

(: real-hpd-interval (->* [Real (Sequenceof Real)]
                          [(Option (Sequenceof Real))]
                          (Values Real Real)))
(define (real-hpd-interval α xs [ws #f])
  (hpd-interval < - α xs ws))
