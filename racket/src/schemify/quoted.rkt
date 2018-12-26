#lang racket/base
(require racket/extflonum)

(provide lift-quoted?)

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
