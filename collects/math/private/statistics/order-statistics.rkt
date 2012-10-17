#lang typed/racket/base

(require racket/sequence
         racket/fixnum
         "../../base.rkt"
         "quickselect.rkt"
         "statistics-utils.rkt")

(provide quantile
         median
         real-quantile
         real-median
         absolute-deviation/median
         absolute-deviation)

(: quantile (All (A) (case-> (Real (A A -> Any) (Sequenceof A) -> A)
                             (Real (A A -> Any) (Sequenceof A) (Option (Sequenceof Real)) -> A))))
(define (quantile p lt? xs [ws #f])
  (cond [(or (p . < . 0) (p . > . 1))
         (raise-argument-error 'quantile "Real in [0,1]" 0 p < xs ws)]
        [ws
         (let-values ([(xs ws)  (sequences->weighted-samples 'median xs ws)])
           (define xws ((inst sort (Pair A Nonnegative-Real) Real)
                        (map (inst cons A Nonnegative-Real) xs ws)
                        (λ: ([xw1 : (Pair A Nonnegative-Real)]
                             [xw2 : (Pair A Nonnegative-Real)])
                          (if (lt? (car xw1) (car xw2)) #t #f))))
           (let ([xs  (map (inst car A Nonnegative-Real) xws)]
                 [ws  (map (inst cdr A Nonnegative-Real) xws)])
             (define total-w (sum ws))
             (cond [(zero? total-w)
                    (raise-argument-error 'quantile "weights with positive sum" 3 p lt? xs ws)]
                   [else
                    (define thresh (* p total-w))
                    (let loop ([xs (cdr xs)] [ws (cdr ws)] [x (car xs)] [s (car ws)])
                      (cond [(s . > . thresh)  x]
                            [(null? xs)  x]
                            [else  (loop (cdr xs) (cdr ws) (car xs) (+ s (car ws)))]))])))]
        [else
         (let ([xs  (sequence->vector xs)])
           (define n (vector-length xs))
           (cond [(n . fx<= . 0)  (raise-argument-error 'quantile "nonempty Sequence" 2 p lt? xs)]
                 [else  (kth-value! xs (min (- n 1) (exact-ceiling (* p n))) lt?)]))]))

(: median (All (A) (case-> ((A A -> Any) (Sequenceof A) -> A)
                           ((A A -> Any) (Sequenceof A) (Option (Sequenceof Real)) -> A))))
(define (median lt? xs [ws #f])
  (cond [ws  (quantile 1/2 lt? xs ws)]
        [else
         (let ([xs  (sequence->vector xs)])
           (define n (vector-length xs))
           (cond [(n . fx<= . 0)  (raise-argument-error 'median "nonempty Sequence" xs)]
                 [else  (kth-value! xs (quotient n 2) lt?)]))]))

(: real-quantile (case-> (Real (Sequenceof Real) -> Real)
                         (Real (Sequenceof Real) (Option (Sequenceof Real)) -> Real)))
(define (real-quantile p xs [ws #f])
  (quantile p < xs ws))

(: real-median (case-> ((Sequenceof Real) -> Real)
                       ((Sequenceof Real) (Option (Sequenceof Real)) -> Real)))
(define (real-median xs [ws #f])
  (median < xs ws))

(: absolute-deviation* (Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real))
(define (absolute-deviation* name m xs ws)
  (define-values (axs n)
    (cond [ws  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
                 (values (map (λ: ([x : Real] [w : Real]) (* w (abs (- x m)))) xs ws)
                         (max 0 (sum ws))))]
          [else  (let ([xs  (sequence->list xs)])
                   (values (map (λ: ([x : Real]) (abs (- x m))) xs)
                           (length xs)))]))
  (cond [(zero? n)  +nan.0]
        [else
         (max 0 (/ (sum axs) n))]))

(: absolute-deviation/median
   (case-> (Real (Sequenceof Real) -> Nonnegative-Real)
           (Real (Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real)))
(define (absolute-deviation/median m xs [ws #f])
  (absolute-deviation* 'absolute-deviation/median m xs ws))
  
(: absolute-deviation
   (case-> ((Sequenceof Real) -> Nonnegative-Real)
           ((Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real)))
(define (absolute-deviation xs [ws #f])
  (absolute-deviation* 'absolute-deviation (real-median xs ws) xs ws))
