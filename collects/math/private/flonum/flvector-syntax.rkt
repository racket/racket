#lang racket/base

(provide inline-build-flvector
         build-flvector
         inline-flvector-map
         flvector-map)

(module syntax-defs racket/base
  
  (require (for-syntax racket/base
                       syntax/parse
                       typed/untyped-utils)
           typed/racket/base
           racket/unsafe/ops
           racket/flonum
           racket/fixnum
           "../syntax-utils.rkt"
           "../exception.rkt"
           "../utils.rkt")
  
  (provide (all-defined-out))
  
  (define-syntax (unsafe-flvector-fill! stx)
    (syntax-parse stx
      [(_ xs:id n:id f-expr:expr)
       (syntax/loc stx
         (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
           (if (i . unsafe-fx< . n)
               (begin (unsafe-flvector-set! xs i (f-expr i))
                      (loop (unsafe-fx+ i 1)))
               xs)))]))
  
  (define-syntax (inline-build-flvector stx)
    (syntax-case stx ()
      [(_ n-expr f-expr)
       (cond
         [(syntax-local-typed-context?)
          (syntax/loc stx
            (let*: ([xs : FlVector  (make-flvector (ann n-expr Integer))]
                    [n  : Index     (flvector-length xs)])
              (unsafe-flvector-fill! xs n (ann f-expr (Index -> Flonum)))))]
         [else
          (syntax/loc stx
            (let* ([xs  (make-flvector n-expr)]
                   [n   (flvector-length xs)])
              (define-syntax-rule (new-f i)
                (let ([x  (f-expr i)])
                  (if (flonum? x) x (raise-result-error 'build-flvector "Flonum" x))))
              (unsafe-flvector-fill! xs n new-f)))])]))
  
  (define-syntax (inline-flvector-map stx)
    (syntax-case stx ()
      [(_ f-expr xs-expr)
       (cond
         [(syntax-local-typed-context?)
          (syntax/loc stx
            (let*: ([xs : FlVector  xs-expr]
                    [n  : Index     (flvector-length xs)])
              (define-syntax-rule (new-f i)
                ((ann f-expr (Flonum -> Flonum)) (unsafe-flvector-ref xs i)))
              (define ys (make-flvector n))
              (unsafe-flvector-fill! ys n new-f)))]
         [else
          (syntax/loc stx
            (let* ([xs  (ensure-flvector 'flvector-map xs-expr)]
                   [n   (flvector-length xs)])
              (define-syntax-rule (new-f i)
                (let ([y  (f-expr (unsafe-flvector-ref xs i))])
                  (if (flonum? y) y (raise-result-error 'flvector-map "Flonum" y))))
              (define ys (make-flvector n))
              (unsafe-flvector-fill! ys n new-f)))])]
      [(_ f-expr xs-expr xss-expr ...)
       (with-syntax ([(f)  (generate-temporaries #'(f-expr))]
                     [(xs xss ...)  (generate-temporaries #'(xs-expr xss-expr ...))]
                     [(n ns ...)    (generate-temporaries #'(xs-expr xss-expr ...))]
                     [(Flonums ...)  (build-list (length (syntax->list #'(xs-expr xss-expr ...)))
                                                 (λ _ #'Flonum))])
         (cond
           [(syntax-local-typed-context?)
            (syntax/loc stx
              (let*: ([xs : FlVector   xs-expr]
                      [n  : Index      (flvector-length xs)]
                      [xss : FlVector  xss-expr] ...)
                (check-flvector-lengths! 'flvector-map n xss ...)
                (define-syntax-rule (new-f i)
                  ((ann f-expr (Flonums ... -> Flonum)) (unsafe-flvector-ref xs i)
                                                        (unsafe-flvector-ref xss i) ...))
                (define ys (make-flvector n))
                (unsafe-flvector-fill! ys n new-f)))]
           [else
            (syntax/loc stx
              (let* ([xs  (ensure-flvector 'flvector-map xs-expr)]
                     [n  (unsafe-flvector-length xs)]
                     [xss  (ensure-flvector 'flvector-map xss-expr)] ...)
                (check-flvector-lengths! 'flvector-map n xss ...)
                (define-syntax-rule (new-f i)
                  (let ([y  (f-expr (unsafe-flvector-ref xs i) (unsafe-flvector-ref xss i) ...)])
                    (cond [(flonum? y)  y]
                          [else  (raise-result-error 'flvector-map "Flonum" y)])))
                (define ys (make-flvector n))
                (unsafe-flvector-fill! ys n new-f)))]))]))
  
  )  ; module

(module defs typed/racket/base
  (require racket/flonum
           racket/fixnum
           racket/unsafe/ops
           (submod ".." syntax-defs)
           "../utils.rkt"
           "../exception.rkt")
  
  (provide (all-defined-out))
  
  (: build-flvector (Integer (Index -> Flonum) -> FlVector))
  (define (build-flvector n f) (inline-build-flvector n f))
  
  (: flvector-map (case-> ((Flonum -> Flonum) FlVector -> FlVector)
                          ((Flonum Flonum Flonum * -> Flonum) FlVector FlVector FlVector *
                                                              -> FlVector)))
  (define flvector-map
    (case-lambda:
      [([f : (Flonum -> Flonum)] [xs : FlVector])
       (inline-flvector-map f xs)]
      [([f : (Flonum Flonum -> Flonum)] [xs : FlVector] [ys : FlVector])
       (inline-flvector-map f xs ys)]
      [([f : (Flonum Flonum Flonum * -> Flonum)] [xs : FlVector] [ys : FlVector] . [yss : FlVector *])
       (define n (flvector-length xs))
       (apply check-flvector-lengths! 'flvector-map n ys yss)
       (inline-build-flvector
        n (λ: ([i : Index])
            (apply f
                   (unsafe-flvector-ref xs i)
                   (unsafe-flvector-ref ys i)
                   (map (λ: ([ys : FlVector]) (unsafe-flvector-ref ys i)) yss))))]))
  
  )  ; module

(require 'syntax-defs 'defs)

