#lang typed/racket/base

(require racket/math racket/match racket/list
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-stats.rkt"
         "flomap-pointwise.rkt"
         "flomap-blur.rkt"
         "flomap-composite.rkt"
         "flomap-resize.rkt"
         "flomap-transform.rkt")

(provide flomap-shadow flomap-outline
         flomap-whirl-morph)

(: colorize-alpha (flomap (U (Vectorof Real) FlVector) -> flomap))
(define (colorize-alpha fm vs)
  (match-define (flomap _ 1 w h) fm)
  (fm* fm (make-flomap* w h vs)))

(: shadow-color (Integer (Option (U (Vectorof Real) FlVector)) -> (U (Vectorof Real) FlVector)))
(define (shadow-color c color)
  (cond [color  color]
        [else  (define vs (make-flvector c))
               (flvector-set! vs 0 1.0)
               vs]))

(: flomap-shadow (case-> (flomap Real -> flomap)
                         (flomap Real (Option (U (Vectorof Real) FlVector)) -> flomap)))
(define flomap-shadow
  (case-lambda
    [(fm σ)  (flomap-shadow fm σ #f)]
    [(fm σ color)
     (define c (flomap-components fm))
     (cond [(= c 0)  (raise-type-error 'flomap-shadow "flomap with at least one component" fm)]
           [(= c 1)  (flomap-blur fm σ)]
           [else     (colorize-alpha (flomap-blur (flomap-ref-component fm 0) σ)
                                     (shadow-color c color))])]))

(: flomap-outline (case-> (flomap Real -> flomap)
                          (flomap Real (Option (U (Vectorof Real) FlVector)) -> flomap)))
(define flomap-outline
  (case-lambda
    [(fm amt)  (flomap-outline fm amt #f)]
    [(fm amt color)
     (define c (flomap-components fm))
     (unless (c . > . 0)
       (raise-type-error 'flomap-outline "flomap with at least one component" fm))
     (let ([amt  (real->double-flonum amt)])
       (define σ (* 0.5 (max 1.0 amt)))
       (define ceiling-amt (fl->fx (ceiling amt)))
       (define test-size (fx* 2 (fx+ 1 ceiling-amt)))
       (define test-mid (fxquotient test-size 2))
       (define alpha-fm (flomap-ref-component fm 0))
       (define-values (a-min a-max) (flomap-extreme-values alpha-fm))
       (define a-size (- a-max a-min))
       (define test-fm (inline-build-flomap 1 test-size 1
                                            (λ (k x y i) (if (x . fx>= . test-mid) a-max a-min))))
       (define blur-test-fm (flomap-blur-x test-fm σ))
       (define v-min (flomap-bilinear-ref blur-test-fm 0 (+ 0.5 (- test-mid amt 1.0)) 0.5))
       (define v-max (flomap-bilinear-ref blur-test-fm 0 (+ 0.5 (- test-mid amt)) 0.5))
       (define av-scale (/ a-size (- v-max v-min)))
       (let* ([outline-fm  (flomap-blur alpha-fm σ)]
              [outline-fm  ((inline-flomap-lift
                             (λ (v) (+ a-min (max 0.0 (min a-size (* av-scale (- v v-min)))))))
                            outline-fm)]
              [outline-fm  ((inline-flomap-lift2
                             'subtract-alpha
                             (λ (o a) (if (= a 1.0) 0.0 (/ (- o a) (- 1.0 a)))))
                            outline-fm alpha-fm)])
         (define color-vs (shadow-color c color))
         (colorize-alpha outline-fm color-vs)))]))

(define blend-start 1/3)
(define blend-end 2/3)

(: flomap-whirl-morph (flomap flomap -> (Real -> flomap)))
(define (flomap-whirl-morph fm1 fm2)
  (define w (max (flomap-width fm1) (flomap-width fm2)))
  (define h (max (flomap-height fm1) (flomap-height fm2)))
  (let ([fm1  (flomap-cc-crop fm1 w h)]
        [fm2  (flomap-cc-crop fm2 w h)])
    
    (define: (whirled-fm1 [t : Real]) : flomap
      (define t1 (sqr t))
      (define trans1
        (flomap-transform-compose (flomap-rotate-transform (* t1 (* -1 pi)))
                                  (flomap-whirl-transform (* t1 (* -8 pi)))))
      (flomap-transform fm1 trans1 0 0 w h))
    
    (define: (whirled-fm2 [t : Real]) : flomap
      (define t2 (sqr (- 1 t)))
      (define trans2
        (flomap-transform-compose (flomap-rotate-transform (* t2 (* 1 pi)))
                                  (flomap-whirl-transform (* t2 (* 8 pi)))))
      (flomap-transform fm2 trans2 0 0 w h))
    
    (λ (t)
      (cond [(t . <= . 0)  fm1]
            [(t . >= . 1)  fm2]
            [else
             (cond [(t . <= . blend-start)  (whirled-fm1 t)]
                   [(t . >= . blend-end)    (whirled-fm2 t)]
                   [else
                    (define b (/ (- t blend-start) (- blend-end blend-start)))
                    (fm+ (fm* (- 1 b) (whirled-fm1 t)) (fm* b (whirled-fm2 t)))])]))))
