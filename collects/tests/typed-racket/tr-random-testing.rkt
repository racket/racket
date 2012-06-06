#lang racket

;; Random testing of type preservation for floats.

(require redex
         racket/flonum racket/unsafe/ops unstable/flonum
         racket/sandbox)

(require (except-in typed-racket/utils/utils infer)
         (typecheck typechecker)
         (utils tc-utils)
         (types subtype utils)
         typed-racket/infer/infer-dummy typed-racket/infer/infer)

(require (prefix-in b: (base-env base-env))
         (prefix-in n: (base-env base-env-numeric)))

(b:init) (n:init)
(define-namespace-anchor anch)

;; TODO exact numbers

(define-language tr-arith ; to stay within floats, removed some numeric ops
  [n real]
  [E n
     (* E)
     (* E E)
     (* E E E)
     (+ E)
     (+ E E)
     (+ E E E)
     (- E)
     (- E E)
     (- E E E)
     (/ E)
     (/ E E)
     (/ E E E)
     (max E)
     (max E E)
     (max E E E)
     (min E)
     (min E E)
     (min E E E)
     (add1 E)
     (sub1 E)
     (abs E)
     (floor E)
     (ceiling E)
     (truncate E)
     (round E)
     (log E)
     (exp E)
     (cos E)
     (sin E)
     (tan E)
     (sqr E)
     (flabs E)
     (flround E)
     (flfloor E)
     (flceiling E)
     (fltruncate E)
     (flsin E)
     (flcos E)
     (fltan E)
     (flatan E)
     (flasin E)
     (flacos E)
     (fllog E)
     (flexp E)
     (flsqrt E)
     (unsafe-flabs E)
     (unsafe-flmin E E)
     (unsafe-flmax E E)
     (unsafe-flsqrt E)
     (fl+ E E)
     (fl- E E)
     (fl* E E)
     (fl/ E E)
     (flmin E E)
     (flmax E E)
     (flexpt E E)
     (unsafe-fl+ E E)
     (unsafe-fl- E E)
     (unsafe-fl* E E)
     (unsafe-fl/ E E)
     ])
;; generated from: (map car (file->list "base-env-parts"))

(define (get-type e [typecheck (compose tc-expr expand)])
  (parameterize ([delay-errors? #f]
                 [current-namespace (namespace-anchor->namespace anch)]
                 [custom-printer #t]
                 [infer-param infer]
                 [orig-module-stx (quote-syntax e)])
    (typecheck (datum->syntax #'here e))))

(define (right-type? before)
  (define type-before (match (get-type before) [(tc-result1: b) b]))
  (define after (eval before (namespace-anchor->namespace anch)))
  (define type-after (get-type after tc-literal))
  (define subtype? (subtype type-after type-before))
  subtype?)

;; Takes random redex reals (mostly integers, sometimes rationals, floats
;; once in a blue moon).
(define (random->random-float E)
  (define r (random))
  (cond
    ;; probability 1/4: noisify and convert to single flonum
    [(r . < . 0.25)
     (real->single-flonum (* (random) E))]
    ;; probability 1/4: noisify and convert to double flonum
    [(r . < . 0.5)
     (real->double-flonum (* (random) E))]
    ;; probability 1/4: convert to very small double flonum
    [(r . < . 0.75)
     (define x (ordinal->flonum (round (inexact->exact E))))
     (cond [(= x 0.0)  (if ((random) . < . 0.5) 0.0 -0.0)]
           [else  x])]
    ;; probability 1/20: +nan.0
    [(r . < . 0.8)
     +nan.0]
    ;; remaining probability: convert to very large double flonum
    [else
     (if ((random) . < . 0.5)
         (flstep -inf.0 (round (inexact->exact E)))
         (flstep +inf.0 (- (round (inexact->exact E)))))]))

;; Redex can't generate floats, so we convert ints to floats.
(define (exp->float-exp E) ; numbers or symbols or lists
  (cond [(number? E)
         (random->random-float E)]
        [(list? E)
         (map exp->float-exp E)]
        [else
         E]))

(define (check-all-floats sexp)
  (or (with-handlers
          ;; something went wrong, almost certainly typechecking failed
          ;; in which case we ignore the expression
          ([exn? (const #t)])
        (get-type sexp)
        #f) ; go on and check preservation
      (right-type? sexp)))

(call-with-limits
 #f 1000
 (lambda ()
   (redex-check tr-arith E (check-all-floats (term E))
                #:attempts 500
                #:prepare exp->float-exp)))
