#lang typed/racket/base

(require racket/match racket/math
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-stats.rkt")

(provide flomap-lift flomap-lift2 inline-flomap-lift inline-flomap-lift2
         fmsqrt fm+ fm- fm* fm/ fmmin fmmax fmsqr
         flomap-normalize flomap-multiply-alpha flomap-divide-alpha)

;; ===================================================================================================
;; Unary

;(: inline-flomap-lift ((Float -> Float) -> (flomap -> flomap)))
(define-syntax-rule (inline-flomap-lift f)
  (λ: ([fm : flomap])
    (match-define (flomap vs c w h) fm)
    (flomap (inline-build-flvector (* c w h) (λ (i) (f (flvector-ref vs i))))
            c w h)))

(: flomap-lift ((Float -> Real) -> (flomap -> flomap)))
(define (flomap-lift op)
  (inline-flomap-lift (λ (x) (real->double-flonum (op x)))))

(define fmsqrt (inline-flomap-lift flsqrt))

;; ===================================================================================================
;; Binary

(: raise-two-reals-error (Symbol Real Real -> flomap))
(define (raise-two-reals-error name r1 r2)
  (error name "expected at least one flomap argument; given ~e and ~e" r1 r2))

(: raise-size-error (Symbol Integer Integer Integer Integer -> flomap))
(define (raise-size-error name w h w2 h2)
  (error name "expected same-size flomaps; given sizes ~e×~e and ~e×~e" w h w2 h2))

(: raise-component-error (Symbol Integer Integer -> flomap))
(define (raise-component-error name c1 c2)
  (error name (string-append "expected flomaps with the same number of components, "
                             "or a flomap with 1 component and any same-size flomap; "
                             "given flomaps with ~e and ~e components")
         c1 c2))

#;
(: inline-flomap-lift2* (Symbol (Float Float -> Float)
                                -> (flomap flomap -> flomap)))
(define-syntax-rule (inline-flomap-lift2* name f)
  (λ: ([fm1 : flomap] [fm2 : flomap])
    (match-define (flomap vs1 c1 w h) fm1)
    (match-define (flomap vs2 c2 w2 h2) fm2)
    (cond
      [(not (and (= w w2) (= h h2)))  (raise-size-error name w h w2 h2)]
      [(= c1 c2)  (flomap (inline-build-flvector (* c1 w h)
                                                 (λ (i) (f (flvector-ref vs1 i)
                                                           (flvector-ref vs2 i))))
                          c1 w h)]
      [(= c1 1)  (inline-build-flomap
                  c2 w h
                  (λ (k x y i) (f (flvector-ref vs1 (coords->index 1 w 0 x y))
                                  (flvector-ref vs2 i))))]
      [(= c2 1)  (inline-build-flomap
                  c1 w h
                  (λ (k x y i) (f (flvector-ref vs1 i)
                                  (flvector-ref vs2 (coords->index 1 w 0 x y)))))]
      [else  (raise-component-error name c1 c2)])))

#;
(: inline-flomap-lift2 (Symbol (Float Float -> Float)
                               -> ((U Real flomap) (U Real flomap) -> flomap)))
(define-syntax-rule (inline-flomap-lift2 name f)
  (λ: ([fm1 : (U Real flomap)] [fm2 : (U Real flomap)])
    (cond
      [(and (real? fm1) (real? fm2))  (raise-two-reals-error name fm1 fm2)]
      [(real? fm1)  (let ([fm1  (real->double-flonum fm1)])
                      ((inline-flomap-lift (λ (v) (f fm1 v))) fm2))]
      [(real? fm2)  (let ([fm2  (real->double-flonum fm2)])
                      ((inline-flomap-lift (λ (v) (f v fm2))) fm1))]
      [else  ((inline-flomap-lift2* name f) fm1 fm2)])))

(: flomap-lift2 (Symbol (Float Float -> Real) -> ((U Real flomap) (U Real flomap) -> flomap)))
(define (flomap-lift2 name f)
  (inline-flomap-lift2 name (λ (x y) (real->double-flonum (f x y)))))

(define fm+ (inline-flomap-lift2 'fm+ +))
(define fm- (inline-flomap-lift2 'fm- -))
(define fm* (inline-flomap-lift2 'fm* *))
(define fm/ (inline-flomap-lift2 'fm/ /))
(define fmmin (inline-flomap-lift2 'fmmin min))
(define fmmax (inline-flomap-lift2 'fmmax max))

(: fmsqr (flomap -> flomap))
(define (fmsqr fm) (fm* fm fm))

(: flomap-normalize (flomap -> flomap))
(define (flomap-normalize fm)
  (define-values (v-min v-max) (flomap-extreme-values fm))
  (define v-size (- v-max v-min))
  (let* ([fm  (fm- fm v-min)]
         [fm  (if (v-size . = . 0.0) fm (fm/ fm v-size))])
    fm))

(define fmdiv/zero
  (inline-flomap-lift2* 'fmdiv/zero (λ (x y) (if (y . = . 0.0) 0.0 (/ x y)))))

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
