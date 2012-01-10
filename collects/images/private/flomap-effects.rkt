#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match racket/list
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-pointwise.rkt"
         "flomap-blur.rkt"
         "flomap-composite.rkt")

(provide flomap-outline flomap-outlined
         flomap-shadow flomap-shadowed)

(: colorize-alpha (flomap (Listof Real) -> flomap))
(define (colorize-alpha fm color)
  (match-define (flomap _ 1 w h) fm)
  (flomap-append-components fm (fm* fm (make-flomap/components w h color))))

(: flomap-shadow (case-> (flomap Real -> flomap)
                         (flomap Real (Option (Listof Real)) -> flomap)))
(define flomap-shadow
  (case-lambda
    [(fm σ)  (flomap-shadow fm σ #f)]
    [(fm σ color)
     (match-define (flomap _ c w h) fm)
     (cond [(c . = . 0)  fm]
           [else  (define alpha-fm (flomap-ref-component fm 0))
                  (define color-vs (if (list? color) color (make-list (- c 1) 0.0)))
                  (colorize-alpha (flomap-blur alpha-fm σ) color-vs)])]))

(: flomap-shadowed (case-> (flomap Real -> flomap)
                           (flomap Real (Option (Listof Real)) -> flomap)))
(define flomap-shadowed
  (case-lambda
    [(fm σ)    (flomap-shadowed fm σ #f)]
    [(fm σ c)  (flomap-cc-superimpose (flomap-shadow fm σ c) fm)]))

(: flomap-outline (case-> (flomap Real -> flomap)
                          (flomap Real (Option (Listof Real)) -> flomap)))
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
       (define color-vs (if (list? color) color (make-list (- c 1) 0.0)))
       (colorize-alpha new-alpha-fm color-vs))]))

(: flomap-outlined (case-> (flomap Real -> flomap)
                           (flomap Real (Option (Listof Real)) -> flomap)))
(define flomap-outlined
  (case-lambda
    [(fm amt)    (flomap-outlined fm amt #f)]
    [(fm amt c)  (flomap-cc-superimpose (flomap-outline fm amt c) fm)]))
