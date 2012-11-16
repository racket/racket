#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/string)
         racket/performance-hint
         "../../flonum.rkt"
         "impl/delta-dist.rkt"
         "dist-struct.rkt")

(provide (all-defined-out))

(begin-encourage-inline
  
  (: flprobability (Flonum Any Any -> Flonum))
  (define (flprobability p log? 1-p?)
    (cond [1-p?  (if log? (fllog1p (- p)) (- 1.0 p))]
          [else  (if log? (fllog p) p)]))
  
  (: flprobability-zero? (Flonum Any Any -> Boolean))
  (define (flprobability-zero? p log? 1-p?)
    (cond [1-p?  (if log? (fl= p 0.0) (fl= p 1.0))]
          [else  (if log? (fl= p -inf.0) (fl= p 0.0))]))
  
  (: flprobability-one? (Flonum Any Any -> Boolean))
  (define (flprobability-one? p log? 1-p?)
    (cond [1-p?  (if log? (fl= p -inf.0) (fl= p 0.0))]
          [else  (if log? (fl= p 0.0) (fl= p 1.0))]))
  
  )

;; ===================================================================================================

(define-syntax (define-distribution-type: stx)
  (syntax-case stx ()
    [(_ (type-name T ...) (parent-type-name In Out)
        (A B) name ([arg-names arg-opts ...] ...))
     (let ([arg-name-lst  (syntax->list #'(arg-names ...))])
       (with-syntax* ([internal-type-name  (format-id #'type-name "~a-Struct" #'type-name)]
                      [(internal-proc-names ...)  (map (λ (arg-name)
                                                         (format-id #'type-name
                                                                    "~a-~a"
                                                                    #'internal-type-name
                                                                    arg-name))
                                                       arg-name-lst)]
                      [internal-pred-name  (format-id #'type-name "~a?" #'internal-type-name)]
                      [make-name  (format-id #'name "make-~a" #'name)]
                      [(proc-names ...)  (map (λ (arg-name)
                                                (format-id #'name "~a-~a" #'name arg-name))
                                              arg-name-lst)]
                      [pred-name  (format-id #'name "~a?" #'name)]
                      [format-str
                       (string-append "(~a "
                                      (string-join (build-list (length arg-name-lst) (λ _ "~v")))
                                      ")")])
         (syntax/loc stx
           (begin
             (struct: (A B) internal-type-name parent-type-name ([arg-names arg-opts ...] ...)
               #:property prop:custom-print-quotable 'never
               #:property prop:custom-write
               (λ (v port write?)
                 (fprintf port format-str 'name (proc-names v) ...)))
             (define-type (type-name T ...) (internal-type-name In Out))
             (define proc-names internal-proc-names) ...
             (define make-name internal-type-name)
             (define pred-name internal-pred-name)))))]
    [(_ type-name (parent-type-name In Out) name ([arg-names arg-opts ...] ...))
     (syntax/loc stx
       (define-distribution-type: (type-name) (parent-type-name In Out)
         (A B) name ([arg-names arg-opts ...] ...)))]))

;; ===================================================================================================
;; One-sided scale family distributions (e.g. exponential)

(define-syntax-rule (make-one-sided-scale-flpdf standard-flpdf)
  (λ: ([s : Float] [x : Float] [log? : Any])
    (cond [(fl= s 0.0)  (fldelta-pdf 0.0 x log?)]
          [(and (s . fl> . 0.0) (x . fl< . 0.0))  (if log? -inf.0 0.0)]
          [(and (s . fl< . 0.0) (x . fl> . 0.0))  (if log? -inf.0 0.0)]
          [else  (let ([q  (standard-flpdf (fl/ x s) log?)])
                   (if log? (fl- q (fllog (flabs s))) (fl/ q (flabs s))))])))

(define-syntax-rule (make-one-sided-scale-flcdf standard-flcdf)
  (λ: ([s : Float] [x : Float] [log? : Any] [1-p? : Any])
    (cond [(fl= s 0.0)  (fldelta-cdf 0.0 x log? 1-p?)]
          [(and (s . fl> . 0.0) (x . fl< . 0.0))
           (cond [1-p?  (if log? 0.0 1.0)]
                 [else  (if log? -inf.0 0.0)])]
          [(and (s . fl< . 0.0) (x . fl> . 0.0))
           (cond [1-p?  (if log? -inf.0 0.0)]
                 [else  (if log? 0.0 1.0)])]
          [else
           (standard-flcdf (fl/ x s) log? (if (s . fl> . 0.0) 1-p? (not 1-p?)))])))

(define-syntax-rule (make-one-sided-scale-flinv-cdf standard-flinv-cdf)
  (λ: ([s : Float] [q : Float] [log? : Any] [1-p? : Any])
    (cond [(fl= s 0.0)  (fldelta-inv-cdf 0.0 q log? 1-p?)]
          [(not (flprobability? q log?))  +nan.0]
          [else  (fl* s (standard-flinv-cdf q log? 1-p?))])))

(define-syntax-rule (make-one-sided-scale-flrandom standard-flinv-cdf)
  (λ: ([s : Float])
    (fl* s (standard-flinv-cdf (fl* 0.5 (random)) #f ((random) . fl> . 0.5)))))

;; ===================================================================================================
;; Location-scale family distributions (e.g. Cauchy, logistic, normal)

(define-syntax-rule (make-symmetric-location-scale-flpdf standard-flpdf)
  (λ: ([c : Float] [s : Float] [x : Float] [log? : Any])
    (cond [(fl= s 0.0)  (fldelta-pdf c x log?)]
          [else  (let ([q  (standard-flpdf (flabs (fl/ (fl- x c) s)) log?)])
                   (if log? (fl- q (fllog (flabs s))) (fl/ q (flabs s))))])))

(define-syntax-rule (make-symmetric-location-scale-flcdf standard-flcdf)
  (λ: ([c : Float] [s : Float] [x : Float] [log? : Any] [1-p? : Any])
    (cond [(fl= s 0.0)  (fldelta-cdf c x log? 1-p?)]
          [else  (let ([x  (fl/ (fl- x c) s)])
                   (standard-flcdf (if 1-p? (- x) x) log?))])))

(define-syntax-rule (make-symmetric-location-scale-flinv-cdf standard-flinv-cdf)
  (λ: ([c : Float] [s : Float] [q : Float] [log? : Any] [1-p? : Any])
    (cond [(fl= s 0.0)  (fldelta-inv-cdf c q log? 1-p?)]
          [(not (flprobability? q log?))  +nan.0]
          [else  (let* ([x  (standard-flinv-cdf q log?)]
                        [x  (if 1-p? (- x) x)])
                   (fl+ (fl* x s) c))])))

(define-syntax-rule (make-symmetric-location-scale-flrandom standard-flinv-cdf)
  (λ: ([c : Float] [s : Float])
    (define x (standard-flinv-cdf (fl* 0.5 (random)) #f))
    (fl+ c (fl* s (if ((random) . fl> . 0.5) x (- x))))))
