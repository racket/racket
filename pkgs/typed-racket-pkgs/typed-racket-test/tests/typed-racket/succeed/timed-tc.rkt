#lang racket/base

(require racket/sandbox)

;; These examples used to take minutes to typecheck.
;; Now they take ~3 seconds on my machine, so this should 
;; be a conservative test to ensure they don't revert back to
;; there original run times

(call-with-limits
 30
 500
 (λ () 
   (eval 
    '(module 
        a 
      typed/racket
      (: filters-more (-> Boolean Boolean Boolean Boolean Boolean 
                          Boolean Boolean Boolean Boolean Natural))
      (define (filters-more inc1? inc2? inc3? a-min? a-max? b-min? b-max? c-min? c-max?)
        (let-values ([(a-min? a-max?)  (if inc1? (values a-min? a-max?) 
                                           (values a-max? a-min?))]
                     [(b-min? b-max?)  (if inc2? (values b-min? b-max?) 
                                           (values b-max? b-min?))]
                     [(c-min? c-max?)  (if inc3? (values c-min? c-max?) 
                                           (values c-max? c-min?))])
          (cond [a-min?  0]
                [b-min?  1]
                [a-max?  2]
                [b-max?  3]
                [c-min?  4]
                [c-max?  5]
                [else    6])))
      
      
      
      (: foo : Any -> Number)
      (define (foo x)
        (match x
          [(cons (or 'x 'y) more)
           (match x
             [(list _ (list (list (? number? x) y (? number? xs) ys))) 1]
             [_ 2])])))
    (make-base-namespace))))