#lang racket/base
(require racket/extflonum
         racket/fixnum)

(provide lift-quoted?
         large-quoted?)

;; Check whether a quoted value needs to be lifted to run-time construction
(define (lift-quoted? q for-cify? datum-intern?)
  (let lift-quoted? ([q q])
    (cond
      [for-cify?
       (not (or (and (exact-integer? q)
                     ;; always a fixnum:
                     (<= (- (expt 2 29)) q (expt 2 29)))
                (boolean? q)
                (null? q)
                (void? q)))]
      [(impersonator? q) #t] ; i.e., strip impersonators when serializaing
      [(path? q) #t]
      [(regexp? q) #t]
      [(srcloc? q) #t]
      [(byte-regexp? q) #t]
      [(keyword? q) #t]
      [(hash? q) #t]
      [(string? q) datum-intern?]
      [(bytes? q) datum-intern?]
      [(pair? q) (or (lift-quoted? (car q))
                     (lift-quoted? (cdr q)))]
      [(vector? q) (for/or ([e (in-vector q)])
                     (lift-quoted? e))]
      [(box? q) (lift-quoted? (unbox q))]
      [(prefab-struct-key q) #t]
      [(extflonum? q) #t]
      [else #f])))

;; Check whether a quoted value is large enough to be worth representing
;; in fasl format:
(define (large-quoted? q)
  (define fuel
    (let remain ([q q] [fuel 128])
      (cond
        [(fx= fuel 0) 0]
        [(pair? q) (remain (cdr q) (remain (car q) (fx- fuel 1)))]
        [(vector? q) (for/fold ([fuel (fx- fuel 1)]) ([e (in-vector q)])
                       (remain e fuel))]
        [(box? q) (remain (unbox q) (fx- fuel 1))]
        [(prefab-struct-key q) (remain (struct->vector q) fuel)]
        [else (fx- fuel 1)])))
  (fx= fuel 0))
