#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/string)
         "../../flonum.rkt"
         "types.rkt"
         "fldelta-dist.rkt")

(provide (all-defined-out))

(define-syntax (define-distribution-type: stx)
  (syntax-case stx ()
    [(_ name type-name parent-type-name ([arg-names arg-opts ...] ...))
     (let ([arg-name-lst  (syntax->list #'(arg-names ...))])
       (with-syntax ([hidden-type-name  (datum->syntax #f (syntax->datum #'type-name))]
                     [(hidden-proc-names ...)  (map (λ (arg-name)
                                                      (format-id #f "~a-~a" #'type-name arg-name))
                                                    arg-name-lst)]
                     [hidden-pred-name  (format-id #f "~a?" #'type-name)]
                     [make-name  (format-id #'name "make-~a" #'name)]
                     [(proc-names ...)  (map (λ (arg-name) (format-id #'name "~a-~a" #'name arg-name))
                                             arg-name-lst)]
                     [pred-name  (format-id #'name "~a?" #'name)]
                     [format-str
                      (string-append "(~a "
                                     (string-join (build-list (length arg-name-lst) (λ _ "~a")))
                                     ")")])
         (syntax/loc stx
           (begin
             (struct: hidden-type-name parent-type-name ([arg-names arg-opts ...] ...)
               #:property prop:custom-print-quotable 'never
               #:property prop:custom-write
               (λ (v port write?)
                 (fprintf port format-str 'name (proc-names v) ...)))
             (define-type type-name hidden-type-name)
             (define proc-names hidden-proc-names) ...
             (define make-name hidden-type-name)
             (define pred-name hidden-pred-name)))))]))

;; ===================================================================================================
;; One-sided scale family distributions (e.g. exponential)

(define-syntax-rule (make-one-sided-scale-flpdf standard-flpdf)
  (λ: ([s : Float] [x : Float] [log? : Any])
    (cond [(s . = . 0.0)  (fldelta-pdf 0.0 x log?)]
          [(and (s . > . 0.0) (x . < . 0.0))  (if log? -inf.0 0.0)]
          [(and (s . < . 0.0) (x . > . 0.0))  (if log? -inf.0 0.0)]
          [else  (let ([q  (standard-flpdf (/ x s) log?)])
                   (if log? (- q (fllog (abs s))) (/ q (abs s))))])))

(define-syntax-rule (make-one-sided-scale-flcdf standard-flcdf)
  (λ: ([s : Float] [x : Float] [log? : Any] [upper-tail? : Any])
    (cond [(s . = . 0.0)  (fldelta-cdf 0.0 x log? upper-tail?)]
          [(and (s . > . 0.0) (x . < . 0.0))
           (cond [upper-tail?  (if log? 0.0 1.0)]
                 [else  (if log? -inf.0 0.0)])]
          [(and (s . < . 0.0) (x . > . 0.0))
           (cond [upper-tail?  (if log? -inf.0 0.0)]
                 [else  (if log? 0.0 1.0)])]
          [else
           (standard-flcdf (/ x s) log? (if (s . > . 0.0) upper-tail? (not upper-tail?)))])))

(define-syntax-rule (make-one-sided-scale-flinv-cdf standard-flinv-cdf)
  (λ: ([s : Float] [q : Float] [log? : Any] [upper-tail? : Any])
    (cond [(s . = . 0.0)  (fldelta-inv-cdf 0.0 q log? upper-tail?)]
          [(not (flprobability? q log?))  +nan.0]
          [else  (* s (standard-flinv-cdf q log? upper-tail?))])))

(define-syntax-rule (make-one-sided-scale-flrandom standard-flinv-cdf)
  (λ: ([s : Float])
    (* s (standard-flinv-cdf (* 0.5 (random)) #f ((random) . > . 0.5)))))

;; ===================================================================================================
;; Location-scale family distributions (e.g. Cauchy, logistic, normal)

(define-syntax-rule (make-symmetric-location-scale-flpdf standard-flpdf)
  (λ: ([x0 : Float] [s : Float] [x : Float] [log? : Any])
    (cond [(s . = . 0.0)  (fldelta-pdf x0 x log?)]
          [else  (let ([q  (standard-flpdf (abs (/ (- x x0) s)) log?)])
                   (if log? (- q (fllog (abs s))) (/ q (abs s))))])))

(define-syntax-rule (make-symmetric-location-scale-flcdf standard-flcdf)
  (λ: ([x0 : Float] [s : Float] [x : Float] [log? : Any] [upper-tail? : Any])
    (cond [(s . = . 0.0)  (fldelta-cdf x0 x log? upper-tail?)]
          [else  (let ([x  (/ (- x x0) s)])
                   (standard-flcdf (if upper-tail? (- x) x) log?))])))

(define-syntax-rule (make-symmetric-location-scale-flinv-cdf standard-flinv-cdf)
  (λ: ([x0 : Float] [s : Float] [q : Float] [log? : Any] [upper-tail? : Any])
    (cond [(s . = . 0.0)  (fldelta-inv-cdf x0 q log? upper-tail?)]
          [(not (flprobability? q log?))  +nan.0]
          [else  (let* ([x  (standard-flinv-cdf q log?)]
                        [x  (if upper-tail? (- x) x)])
                   (+ (* x s) x0))])))

(define-syntax-rule (make-symmetric-location-scale-flrandom standard-flinv-cdf)
  (λ: ([x0 : Float] [s : Float])
    (define x (standard-flinv-cdf (* 0.5 (random)) #f))
    (+ x0 (* s (if ((random) . > . 0.5) x (- x))))))
