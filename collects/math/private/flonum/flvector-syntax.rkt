#lang racket/base

(module syntax-defs* racket/base
  
  (require (for-syntax racket/base)
           typed/racket/base
           racket/flonum
           racket/fixnum
           racket/unsafe/ops
           "../syntax-utils.rkt"
           "../exception.rkt")
  
  (provide (all-defined-out))
  
  (define-syntax-rule (ensure-flvector/length name xs-expr n)
    (let*: ([xs : FlVector  (ensure-flvector name xs-expr)])
      (if (fx= (unsafe-flvector-length xs) n) xs (raise-length-error name "FlVector" xs n))))
  
  (define-syntax-rule (unsafe-build-flvector n f)
    (let ([xs  (make-flvector n)])
      (let: loop : FlVector ([i : Nonnegative-Fixnum  0])
        (if (i . unsafe-fx< . n)
            (begin (unsafe-flvector-set! xs i (f i))
                   (loop (unsafe-fx+ i 1)))
            xs))))
  
  (define-syntax-rule (inline-build-flvector* n-expr f-expr)
    (let ([n  (ensure-index 'build-flvector n-expr)]
          [f  (ensure-procedure 'build-flvector f-expr (Index -> Flonum))])
      (define-syntax-rule (new-f i)
        (let ([x  (f i)])
          (if (flonum? x) x (raise-result-error 'build-flvector "Flonum" x))))
      (unsafe-build-flvector n new-f)))
  
  (define-syntax (inline-flvector-map* stx)
    (syntax-case stx ()
      [(_ f-expr xs-expr)
       (syntax/loc stx
         (let ([f   (ensure-procedure 'flvector-map f-expr (Flonum -> Flonum))]
               [xs  (ensure-flvector 'flvector-map xs-expr)])
           (define n (unsafe-flvector-length xs))
           (define-syntax-rule (new-f i)
             (let ([y  (f (unsafe-flvector-ref xs i))])
               (if (flonum? y) y (raise-result-error 'flvector-map "Flonum" y))))
           (unsafe-build-flvector n new-f)))]
      [(_ f-expr xs-expr xss-expr ...)
       (with-syntax ([(f)  (generate-temporaries #'(f-expr))]
                     [(xs xss ...)  (generate-temporaries #'(xs-expr xss-expr ...))]
                     [(n ns ...)    (generate-temporaries #'(xs-expr xss-expr ...))]
                     [(Flonums ...)  (build-list (length (syntax->list #'(xs-expr xss-expr ...)))
                                                 (λ _ #'Flonum))])
         (syntax/loc stx
           (let* ([f  (ensure-procedure 'flvector-map f-expr (Flonums ... -> Flonum))]
                  [xs  (ensure-flvector 'flvector-map xs-expr)]
                  [n  (unsafe-flvector-length xs)]
                  [xss  (ensure-flvector/length 'flvector-map xss-expr n)] ...)
             (define-syntax-rule (new-f i)
               (let ([y  (f (unsafe-flvector-ref xs i) (unsafe-flvector-ref xss i) ...)])
                 (cond [(flonum? y)  y]
                       [else  (raise-result-error 'flvector-map "Flonum" y)])))
             (unsafe-build-flvector n new-f))))]))
  
  )

(module defs typed/racket/base
  (require racket/flonum
           racket/fixnum
           racket/unsafe/ops
           (submod ".." syntax-defs*)
           "../exception.rkt")
  
  (provide (all-defined-out))
  
  (: build-flvector (Integer (Index -> Flonum) -> FlVector))
  (define (build-flvector n f)
    (inline-build-flvector* n f))
  
  (: flvector-map (case-> ((Flonum -> Flonum) FlVector -> FlVector)
                          ((Flonum Flonum Flonum * -> Flonum) FlVector FlVector FlVector *
                                                              -> FlVector)))
  (define flvector-map
    (case-lambda:
      [([f : (Flonum -> Flonum)] [xs : FlVector])
       (inline-flvector-map* f xs)]
      [([f : (Flonum Flonum -> Flonum)] [xs : FlVector] [ys : FlVector])
       (inline-flvector-map* f xs ys)]
      [([f : (Flonum Flonum Flonum * -> Flonum)] [xs : FlVector] [ys : FlVector] . [yss : FlVector *])
       (define n (flvector-length xs))
       (for: ([ys  (in-list (cons ys yss))])
         (unless (fx= n (flvector-length ys)) (raise-length-error 'flvector-map "FlVector" ys n)))
       (inline-build-flvector*
        n (λ: ([i : Index])
            (apply f
                   (unsafe-flvector-ref xs i)
                   (unsafe-flvector-ref ys i)
                   (map (λ: ([ys : FlVector]) (unsafe-flvector-ref ys i)) yss))))]))
  
  )

(module syntax-defs racket/base
  (require (for-syntax racket/base)
           (submod ".." syntax-defs*)
           (submod ".." defs))
  
  (provide (all-defined-out))
  
  (define-syntax (inline-build-flvector stx)
    (syntax-case stx ()
      [(_ n-expr f-expr)  (syntax/loc stx (inline-build-flvector* n-expr f-expr))]
      [(_ . args)  (syntax/loc stx (build-flvector . args))]
      [_  (syntax/loc stx build-flvector)]))
  
  (define-syntax (inline-flvector-map stx)
    (syntax-case stx ()
      [(_ f-expr xs-expr xss-expr ...)
       (syntax/loc stx (inline-flvector-map* f-expr xs-expr xss-expr ...))]
      [(_ . args)  (syntax/loc stx (flvector-map . args))]
      [_  (syntax/loc stx flvector-map)]))
  
  )

(require 'defs
         'syntax-defs)

(provide (rename-out [inline-build-flvector  build-flvector]
                     [inline-flvector-map    flvector-map]))
