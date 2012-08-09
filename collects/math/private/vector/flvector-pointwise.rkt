#lang typed/racket/base

(require racket/flonum
         (for-syntax racket/base)
         "../unsafe.rkt")

(provide (rename-out [inline-build-flvector  build-flvector]
                     [inline-flvector-lift1  flvector-lift1]
                     [inline-flvector-lift2  flvector-lift2]
                     [inline-flvector-map    flvector-map]))

(define-syntax (inline-build-flvector stx)
  (syntax-case stx ()
    [(_ size f)
     (syntax/loc stx
       (let: ([n : Integer  size])
         (cond [(index? n)
                (define vs (make-flvector n))
                (let: loop : FlVector ([i : Nonnegative-Fixnum 0])
                  (cond [(i . < . n)  (unsafe-flvector-set! vs i (f i))
                                      (loop (+ i 1))]
                        [else  vs]))]
               [else
                (raise-type-error 'build-flvector "Index" 0 size f)])))]
    [(_ e ...)  (syntax/loc stx (build-flvector e ...))]
    [_  (syntax/loc stx build-flvector)]))

(define-syntax (inline-flvector-lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx
              (λ: ([vs : FlVector])
                (inline-build-flvector (flvector-length vs)
                                       (λ: ([i : Index]) (f (unsafe-flvector-ref vs i))))))]
    [(_ e ...)  (syntax/loc stx (flvector-lift1 e ...))]
    [_  (syntax/loc stx flvector-lift1)]))

(define-syntax (inline-flvector-lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx
              (λ: ([as : FlVector] [bs : FlVector])
                (define n (flvector-length as))
                (define m (flvector-length bs))
                (unless (= n m)
                  (error 'flvector-lift2 "flvectors must be the same length; given lengths ~e and ~e"
                         n m))
                (inline-build-flvector n (λ: ([i : Index]) (f (unsafe-flvector-ref as i)
                                                              (unsafe-flvector-ref bs i))))))]
    [(_ e ...)  (syntax/loc stx (flvector-lift2 e ...))]
    [_  (syntax/loc stx flvector-lift2)]))

(define-syntax (inline-flvector-map stx)
  (syntax-case stx ()
    [(_ f vs)  (syntax/loc stx ((inline-flvector-lift1 f) vs))]
    [(_ e ...)  (syntax/loc stx (flvector-map e ...))]
    [_  (syntax/loc stx flvector-map)]))

(: build-flvector (Integer (Index -> Float) -> FlVector))
(define (build-flvector size f) (inline-build-flvector size f))

(: flvector-lift1 ((Float -> Float) -> (FlVector -> FlVector)))
(define (flvector-lift1 f) (inline-flvector-lift1 f))

(: flvector-lift2 ((Float Float -> Float) -> (FlVector FlVector -> FlVector)))
(define (flvector-lift2 f) (inline-flvector-lift2 f))

(: flvector-map ((Float -> Float) FlVector -> FlVector))
(define (flvector-map f vs) (inline-flvector-map f vs))
