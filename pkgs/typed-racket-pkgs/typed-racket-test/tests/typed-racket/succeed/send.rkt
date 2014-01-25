#lang racket/base

(module fish racket/base
  (require racket/class)
  (define fish%
    (class object%
      (init-field weight)
      (define/public (get-weight) weight)
      (define/public (inc-weight-by! x negate?) 
        (set! weight (+ (if negate? (- weight) weight) x)))
      (super-new)))
  (provide fish%))

(module fish-client typed/racket/base
  (require racket/class)
  (require/typed (submod ".." fish) [fish% Fish%])
  
  (define-type-alias Fish% (Class (init [weight Real])
                                  [get-weight (-> Real)]
                                  [inc-weight-by! (Real Boolean -> Void)]))
  
  (: f ((Instance Fish%) -> Real))
  (define (f b) (send b get-weight))
  
  (: g (Real -> (Instance Fish%)))
  (define (g w) (new fish% [weight w]))
  
  (: h ((Instance Fish%) -> Void))
  (define (h b) (send b inc-weight-by! 2 #f))
  
  (unless (= 0 (f (g 0))) (error 'send.rkt "expected 0"))
  (unless (= 3 (let ([b (g 1)])
                 (h b)
                 (f b)))
    (error 'send.rkt "expected 3")))

(require (submod "." fish-client))
