#lang typed/racket/base

(require racket/sequence
         racket/list
         racket/fixnum
         "../../base.rkt"
         "quickselect.rkt"
         "statistics-utils.rkt")

(provide sort-samples
         quantile
         median
         absdev/median
         absdev)

(: sort-weighted-samples
   (All (A) (Symbol (A A -> Any) (Sequenceof A) (Sequenceof Real)
                    -> (Values (Listof A) (Listof Nonnegative-Real)))))
(define (sort-weighted-samples name lt? xs ws)
  (let-values ([(xs ws)  (sequences->weighted-samples name xs ws)])
    (define xws ((inst sort (Pair A Nonnegative-Real) Real)
                 (map (inst cons A Nonnegative-Real) xs ws)
                 (λ: ([xw1 : (Pair A Nonnegative-Real)]
                      [xw2 : (Pair A Nonnegative-Real)])
                   (and (lt? (car xw1) (car xw2)) #t))))
    (values (map (inst car A Nonnegative-Real) xws)
            (map (inst cdr A Nonnegative-Real) xws))))

(: sort-samples (All (A) (case-> ((A A -> Any) (Sequenceof A) -> (Listof A))
                                 ((A A -> Any) (Sequenceof A) (U #f (Sequenceof Real))
                                               -> (Values (Listof A) (Listof Nonnegative-Real))))))
(define sort-samples
  (case-lambda
    [(lt? xs)
     (sort (sequence->list xs)
           (λ: ([x1 : A] [x2 : A])
             (and (lt? x1 x2) #t)))]
    [(lt? xs ws)
     (cond [ws  (sort-weighted-samples 'sort-samples lt? xs ws)]
           [else  (define ys (sort (sequence->list xs)
                                   (λ: ([x1 : A] [x2 : A])
                                     (and (lt? x1 x2) #t))))
                  (values ys (make-list (length ys) 1))])]))

(: quantile (All (A) (case-> (Real (A A -> Any) (Sequenceof A) -> A)
                             (Real (A A -> Any) (Sequenceof A) (U #f (Sequenceof Real)) -> A))))
(define (quantile p lt? xs [ws #f])
  (cond [(or (p . < . 0) (p . > . 1))
         (raise-argument-error 'quantile "Real in [0,1]" 0 p lt? xs ws)]
        [ws
         (let-values ([(xs ws)  (sort-weighted-samples 'quantile lt? xs ws)])
           (define total-w (sum ws))
           (cond [(zero? total-w)
                  (raise-argument-error 'quantile "weights with positive sum" 3 p lt? xs ws)]
                 [else
                  (let loop ([xs (cdr xs)] [ws (cdr ws)] [x (car xs)] [s (car ws)])
                    (cond [((/ s total-w) . >= . p)  x]
                          [(null? xs)  x]
                          [else  (loop (cdr xs) (cdr ws) (car xs) (+ s (car ws)))]))]))]
        [else
         (let ([xs  (sequence->vector xs)])
           (define n (vector-length xs))
           (cond [(n . fx<= . 0)  (raise-argument-error 'quantile "nonempty Sequence" 2 p lt? xs)]
                 [else
                  (define i (max 0 (- (exact-ceiling (* p n)) 1)))
                  (kth-value! xs i lt?)]))]))

(: median (All (A) (case-> ((A A -> Any) (Sequenceof A) -> A)
                           ((A A -> Any) (Sequenceof A) (U #f (Sequenceof Real)) -> A))))
(define (median lt? xs [ws #f])
  (quantile 1/2 lt? xs ws))

;; ===================================================================================================
;; Absolute deviation

(: absdev* (Symbol Real (Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real))
(define (absdev* name m xs ws)
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

(: absdev/median
   (case-> (Real (Sequenceof Real) -> Nonnegative-Real)
           (Real (Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real)))
(define (absdev/median m xs [ws #f])
  (absdev* 'absdev/median m xs ws))

(: absdev
   (case-> ((Sequenceof Real) -> Nonnegative-Real)
           ((Sequenceof Real) (Option (Sequenceof Real)) -> Nonnegative-Real)))
(define (absdev xs [ws #f])
  (absdev* 'absdev (median < xs ws) xs ws))
