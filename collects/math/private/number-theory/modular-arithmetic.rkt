#lang racket/base

(require "modular-arithmetic-base.rkt")

(provide with-modulus
         current-modulus
         modular-inverse
         modular-expt
         mod+
         mod-
         mod*
         mod/
         modsqr
         modexpt
         mod
         mod=
         mod<
         mod<=
         mod>
         mod>=)

(module typed-defs typed/racket/base
  (require racket/performance-hint
           racket/flonum
           "divisibility.rkt"
           "modular-arithmetic-base.rkt")
  
  (provide (all-defined-out))
  
  (define-syntax-rule (define-comparison-op name op)
    (begin
      (: name (case-> (Integer -> Boolean)
                      (Integer Integer Integer * -> Boolean)))
      (define name
        (case-lambda:
          [([a : Integer])  #t]
          [([a : Integer] [b : Integer])
           (define n (current-modulus))
           (op (modulo a n) (modulo b n))]
          [([a : Integer] [b : Integer] . [cs : Integer *])
           (define n (current-modulus))
           (let ([a  (modulo a n)] [b  (modulo b n)])
             (and (op a b)
                  (let loop ([b b] [cs cs])
                    (or (null? cs)
                        (let ([c  (modulo (car cs) n)])
                          (and (op b c) (loop c (cdr cs))))))))]))))
  
  (begin-encourage-inline
    
    (: mod+ (case-> (-> Natural)
                    (Integer -> Natural)
                    (Integer Integer Integer * -> Natural)))
    (define mod+
      (case-lambda:
        [()  0]
        [([a : Integer])  (modulo a (current-modulus))]
        [([a : Integer] [b : Integer])  (modulo (+ a b) (current-modulus))]
        [([a : Integer] [b : Integer] . [cs : Integer *])
         (define n (current-modulus))
         (for/fold: ([a : Natural  (modulo (+ a b) n)]) ([c  (in-list cs)])
           (modulo (+ a c) n))]))
    
    (: mod- (case-> (Integer -> Natural)
                    (Integer Integer Integer * -> Natural)))
    (define mod-
      (case-lambda:
        [([a : Integer])  (modulo (- a) (current-modulus))]
        [([a : Integer] [b : Integer])  (modulo (- a b) (current-modulus))]
        [([a : Integer] [b : Integer] . [cs : Integer *])
         (define n (current-modulus))
         (for/fold: ([a : Natural  (modulo (- a b) n)]) ([c  (in-list cs)])
           (modulo (- a c) n))]))
    
    (: mod* (case-> (-> Natural)
                    (Integer -> Natural)
                    (Integer Integer Integer * -> Natural)))
    (define mod*
      (case-lambda:
        [()  (modulo 1 (current-modulus))]
        [([a : Integer])  (modulo a (current-modulus))]
        [([a : Integer] [b : Integer])  (modulo (* a b) (current-modulus))]
        [([a : Integer] [b : Integer] . [cs : Integer *])
         (define n (current-modulus))
         (for/fold: ([a : Natural  (modulo (* a b) n)]) ([c  (in-list cs)])
           (modulo (* a c) n))]))
    
    (: mod/ (case-> (Integer -> Natural)
                    (Integer Integer Integer * -> Natural)))
    (define mod/
      (case-lambda:
        [([a : Integer])  (modular-inverse a (current-modulus))]
        [([a : Integer] [b : Integer])  (define n (current-modulus))
                                        (modulo (* a (modular-inverse b n)) n)]
        [([a : Integer] [b : Integer] . [cs : Integer *])
         ;; It's ridiculous that `cs' has to be split here
         (mod/ a (apply mod* b (car cs) (cdr cs)))]))
    
    (: modsqr (Integer -> Natural))
    (define (modsqr a) (modulo (* a a) (current-modulus)))
    
    (: modexpt (Integer Integer -> Natural))
    (define (modexpt a b)
      (define n (current-modulus))
      (cond [(b . < . 0)  (modular-expt (modular-inverse a n) (- b) n)]
            [else  (modular-expt a b n)]))
    
    (: mod (Exact-Rational -> Natural))
    (define (mod a)
      (cond [(integer? a)  (modulo a (current-modulus))]
            [else  (mod/ (numerator a) (denominator a))]))
    
    (define-comparison-op mod= =)
    (define-comparison-op mod< <)
    (define-comparison-op mod<= <=)
    (define-comparison-op mod> >)
    (define-comparison-op mod>= >=)
    
    )  ; begin-encourage-inline
  )

(module untyped-defs racket/base
  (require (for-syntax racket/base)
           "../syntax-utils.rkt"
           "modular-arithmetic-base.rkt"
           (prefix-in typed- (submod ".." typed-defs)))
  
  (provide (all-defined-out))
  
  (define-inline-op mod+ inline-mod+ typed-mod+ (a ...))
  (define-inline-op mod* inline-mod* typed-mod* (a ...))
  (define-inline-op mod- inline-mod- typed-mod- (a b ...))
  (define-inline-op mod/ inline-mod/ typed-mod/ (a b ...))
  (define-inline-op modsqr inline-modsqr typed-modsqr (a))
  (define-inline-op modexpt inline-modexpt typed-modexpt (a b))
  (define-inline-op mod inline-mod typed-mod (a))
  (define-inline-op mod= inline-mod= typed-mod= (a b ...))
  (define-inline-op mod< inline-mod< typed-mod< (a b ...))
  (define-inline-op mod<= inline-mod<= typed-mod<= (a b ...))
  (define-inline-op mod> inline-mod> typed-mod> (a b ...))
  (define-inline-op mod>= inline-mod>= typed-mod>= (a b ...))
  )

(require (submod "." untyped-defs))
