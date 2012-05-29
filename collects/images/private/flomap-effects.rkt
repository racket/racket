#lang typed/racket/base

(require racket/math racket/match racket/list
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-pointwise.rkt"
         "flomap-blur.rkt"
         "flomap-composite.rkt"
         "flomap-resize.rkt"
         "flomap-transform.rkt")

(provide flomap-outline flomap-outlined
         flomap-shadow flomap-shadowed
         flomap-whirl-morph)

(: colorize-alpha (flomap FlVector -> flomap))
(define (colorize-alpha fm vs)
  (match-define (flomap _ 1 w h) fm)
  (flomap-append-components fm (fm* fm (make-flomap* w h vs))))

(: flomap-shadow (case-> (flomap Real -> flomap)
                         (flomap Real (Option FlVector) -> flomap)))
(define flomap-shadow
  (case-lambda
    [(fm σ)  (flomap-shadow fm σ #f)]
    [(fm σ color)
     (match-define (flomap _ c w h) fm)
     (cond [(c . = . 0)  fm]
           [else  (define alpha-fm (flomap-ref-component fm 0))
                  (define color-vs (if (flvector? color) color (make-flvector (- c 1) 0.0)))
                  (colorize-alpha (flomap-blur alpha-fm σ) color-vs)])]))

(: flomap-shadowed (case-> (flomap Real -> flomap)
                           (flomap Real (Option FlVector) -> flomap)))
(define flomap-shadowed
  (case-lambda
    [(fm σ)    (flomap-shadowed fm σ #f)]
    [(fm σ c)  (flomap-cc-superimpose (flomap-shadow fm σ c) fm)]))

(: flomap-outline (case-> (flomap Real -> flomap)
                          (flomap Real (Option FlVector) -> flomap)))
(define flomap-outline
  (case-lambda
    [(fm amt)  (flomap-outline fm amt #f)]
    [(fm amt color)
     (match-define (flomap _ c w h) fm)
     (let ([amt  (exact->inexact amt)])
       (define σ (* 0.5 (max 1.0 amt)))
       (define ceiling-amt (fl->fx (ceiling amt)))
       (define test-size (fx* 2 (fx+ 1 ceiling-amt)))
       (define test-mid (fxquotient test-size 2))
       (define test-fm (inline-build-flomap 1 test-size test-size
                                            (λ (k x y i) (if (x . fx>= . test-mid) 1.0 0.0))))
       (define blur-fm (flomap-blur test-fm σ))
       (define v-max (flomap-bilinear-ref blur-fm 0 (+ 0.5 (- test-mid amt)) test-mid))
       (define v-min (flomap-bilinear-ref blur-fm 0 (+ 0.5 (- test-mid amt 1)) test-mid))
       (define alpha-fm (flomap-ref-component fm 0))
       (define new-alpha-fm (fmmax 0.0 (fmmin 1.0 (fm/ (fm- (flomap-blur alpha-fm σ) v-min)
                                                       (- v-max v-min)))))
       (define color-vs (if (flvector? color) color (make-flvector (- c 1) 0.0)))
       (colorize-alpha new-alpha-fm color-vs))]))

(: flomap-outlined (case-> (flomap Real -> flomap)
                           (flomap Real (Option FlVector) -> flomap)))
(define flomap-outlined
  (case-lambda
    [(fm amt)    (flomap-outlined fm amt #f)]
    [(fm amt c)  (flomap-cc-superimpose (flomap-outline fm amt c) fm)]))

(define blend-start 1/3)
(define blend-end 2/3)

(: flomap-whirl-morph (flomap flomap -> (Real -> flomap)))
(define (flomap-whirl-morph fm1 fm2)
  (define w (max (flomap-width fm1) (flomap-width fm2)))
  (define h (max (flomap-height fm1) (flomap-height fm2)))
  (let ([fm1  (flomap-crop fm1 w h 1/2 1/2)]
        [fm2  (flomap-crop fm2 w h 1/2 1/2)])
    
    (define: (whirled-fm1 [t : Real]) : flomap
      (define t1 (sqr t))
      (define trans1
        (transform-compose (whirl-and-pinch-transform (* t1 (* -8 pi)) (* -4 t1) 1)
                           (rotate-transform (* t1 (* -1 pi)))))
      (flomap-transform fm1 trans1 0 w 0 h))
    
    (define: (whirled-fm2 [t : Real]) : flomap
      (define t2 (sqr (- 1 t)))
      (define trans2
        (transform-compose (rotate-transform (* t2 (* 1 pi)))
                           (whirl-and-pinch-transform (* t2 (* 8 pi)) (* -4 t2) 1)))
      (flomap-transform fm2 trans2 0 w 0 h))
    
    (λ (t)
      (cond [(t . <= . 0)  fm1]
            [(t . >= . 1)  fm2]
            [else
             (cond [(t . <= . blend-start)  (whirled-fm1 t)]
                   [(t . >= . blend-end)    (whirled-fm2 t)]
                   [else
                    (define b (/ (- t blend-start) (- blend-end blend-start)))
                    (fm+ (fm* (- 1 b) (whirled-fm1 t)) (fm* b (whirled-fm2 t)))])]))))
