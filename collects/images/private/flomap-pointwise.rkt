#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match racket/math
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-stats.rkt")

(provide flomap-lift flomap-lift2 inline-flomap-lift inline-flomap-lift2
         fmneg fmabs fmsqr fmsin fmcos fmtan fmlog fmexp fmsqrt fmasin fmacos fmatan
         fmround fmfloor fmceiling fmtruncate fmzero
         fm+ fm- fm* fm/ fmmin fmmax
         flomap-normalize flomap-multiply-alpha flomap-divide-alpha)

;; ===================================================================================================
;; Unary

(define-syntax-rule (inline-flomap-lift f)
  (λ: ([fm : flomap])
    (match-define (flomap vs c w h) fm)
    (flomap (inline-build-flvector (* c w h) (λ (i) (f (unsafe-flvector-ref vs i))))
            c w h)))

(: flomap-lift ((Flonum -> Real) -> (flomap -> flomap)))
(define (flomap-lift op)
  (inline-flomap-lift (λ (x) (exact->inexact (op x)))))

(define fmneg (inline-flomap-lift -))
(define fmabs (inline-flomap-lift abs))
(define fmsqr (inline-flomap-lift sqr))
(define fmsin (inline-flomap-lift sin))
(define fmcos (inline-flomap-lift cos))
(define fmtan (inline-flomap-lift tan))
(define fmlog (inline-flomap-lift fllog))
(define fmexp (inline-flomap-lift exp))
(define fmsqrt (inline-flomap-lift flsqrt))
(define fmasin (inline-flomap-lift asin))
(define fmacos (inline-flomap-lift acos))
(define fmatan (inline-flomap-lift atan))
(define fmround (inline-flomap-lift round))
(define fmfloor (inline-flomap-lift floor))
(define fmceiling (inline-flomap-lift ceiling))
(define fmtruncate (inline-flomap-lift truncate))
(define fmzero (inline-flomap-lift (λ (x) (if (x . = . 0.0) 1.0 0.0))))

;; ===================================================================================================
;; Binary

(define-syntax-rule (inline-flomap-lift2 name f)
  (let: ()
    (λ: ([fm1 : (U Real flomap)] [fm2 : (U Real flomap)])
      (cond
        [(and (real? fm1) (real? fm2))
         (error name "expected at least one flomap argument; given ~e and ~e" fm1 fm2)]
        [(real? fm1)  (let ([fm1  (exact->inexact fm1)])
                        ((inline-flomap-lift (λ (v) (f fm1 v))) fm2))]
        [(real? fm2)  (let ([fm2  (exact->inexact fm2)])
                        ((inline-flomap-lift (λ (v) (f v fm2))) fm1))]
        [else
         (match-define (flomap vs1 c1 w h) fm1)
         (match-define (flomap vs2 c2 w2 h2) fm2)
         (cond
           [(not (and (= w w2) (= h h2)))
            (error name "expected same-size flomaps; given sizes ~e×~e and ~e×~e" w h w2 h2)]
           [(= c1 c2)  (define n (* c1 w h))
                       (define res-vs (make-flvector n))
                       (flomap (inline-build-flvector n (λ (i) (f (unsafe-flvector-ref vs1 i)
                                                                  (unsafe-flvector-ref vs2 i))))
                               c1 w h)]
           [(= c1 1)  (inline-build-flomap
                       c2 w h
                       (λ (k x y i) (f (unsafe-flvector-ref vs1 (coords->index 1 w 0 x y))
                                       (unsafe-flvector-ref vs2 i))))]
           [(= c2 1)  (inline-build-flomap
                       c1 w h
                       (λ (k x y i) (f (unsafe-flvector-ref vs1 i)
                                       (unsafe-flvector-ref vs2 (coords->index 1 w 0 x y)))))]
           [else
            (error name (string-append "expected flomaps with the same number of components, "
                                       "or a flomap with 1 component and any same-size flomap; "
                                       "given flomaps with ~e and ~e components")
                   c1 c2)])]))))

(: flomap-lift2 (Symbol (Flonum Flonum -> Real) -> ((U Real flomap) (U Real flomap) -> flomap)))
(define (flomap-lift2 name f)
  (inline-flomap-lift2 name (λ (x y) (exact->inexact (f x y)))))

(define fm+ (inline-flomap-lift2 'fm+ +))
(define fm- (inline-flomap-lift2 'fm- -))
(define fm* (inline-flomap-lift2 'fm* *))
(define fm/ (inline-flomap-lift2 'fm/ /))
(define fmmin (inline-flomap-lift2 'fmmin min))
(define fmmax (inline-flomap-lift2 'fmmax max))

(: flomap-normalize (flomap -> flomap))
(define (flomap-normalize fm)
  (define-values (v-min v-max) (flomap-extreme-values fm))
  (define v-size (- v-max v-min))
  (let* ([fm  (fm- fm v-min)]
         [fm  (if (v-size . = . 0.0) fm (fm/ fm v-size))])
    fm))

(define fmdiv/zero
  (inline-flomap-lift2 'fmdiv/zero (λ (x y) (if (y . = . 0.0) 0.0 (/ x y)))))

(: flomap-divide-alpha (flomap -> flomap))
(define (flomap-divide-alpha fm)
  (match-define (flomap _ c w h) fm)
  (cond [(c . <= . 1)  fm]
        [else
         (define alpha-fm (flomap-ref-component fm 0))
         (flomap-append-components alpha-fm (fmdiv/zero (flomap-drop-components fm 1) alpha-fm))]))

(: flomap-multiply-alpha (flomap -> flomap))
(define (flomap-multiply-alpha fm)
  (match-define (flomap _ c w h) fm)
  (cond [(c . > . 1)
         (define alpha-fm (flomap-ref-component fm 0))
         (flomap-append-components alpha-fm (fm* (flomap-drop-components fm 1) alpha-fm))]
        [else  fm]))
