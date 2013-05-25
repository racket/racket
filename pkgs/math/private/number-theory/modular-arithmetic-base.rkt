#lang racket/base

(provide with-modulus
         current-modulus
         modular-inverse
         modular-expt
         inline-mod+
         inline-mod*
         inline-mod-
         inline-mod/
         inline-modsqr
         inline-modexpt
         inline-mod
         inline-mod=
         inline-mod<
         inline-mod<=
         inline-mod>
         inline-mod>=)

(module typed-defs typed/racket/base
  (require racket/performance-hint
           "divisibility.rkt")
  
  (provide (all-defined-out))
  
  (: current-modulus-param (Parameterof Integer Positive-Integer))
  (define current-modulus-param
    (make-parameter
     1 (λ: ([n : Integer])
         (cond [(n . <= . 0)  (raise-argument-error 'with-modulus "Positive-Integer" n)]
               [else  n]))))
  
  (: current-modulus (-> Positive-Integer))
  (begin-encourage-inline
    (define (current-modulus) (current-modulus-param)))
  
  ; THEOREM
  ;  If gcd(a,n)=1 then there exist b such that
  ;    ab=1 mod n
  ;  The number b is called an inverse of a modulo n.
  (: modular-inverse* (Positive-Integer Integer -> Natural))
  (define (modular-inverse* n a)
    (cond [(zero? a)  (raise-argument-error 'modular-inverse "nonzero Integer" 0 a n)]
          [(coprime? n a)  (modulo (car (bezout a n)) n)]
          [else  (error 'modular-inverse "expected argument that is coprime to modulus ~e; given ~e"
                        n a)]))
  
  (: modular-expt* (Positive-Integer Integer Integer -> Natural))
  ;; Exponentiate by repeated modular multiplication and squaring
  (define (modular-expt* n a b)
    (cond [(b . < . 0)  (raise-argument-error 'modular-expt "Natural" 1 a b n)]
          [else
           (let loop ([a a] [b b])
             (cond [(b . <= . 1)  (if (zero? b) (modulo 1 n) (modulo a n))]
                   [(even? b)  (define c (loop a (quotient b 2)))
                               (modulo (* c c) n)]
                   [else  (modulo (* a (loop a (sub1 b))) n)]))]))
  
  (: modular-const* (Positive-Integer Exact-Rational -> Natural))
  (define (modular-const* n a)
    (cond [(integer? a)  (modulo a n)]
          [else  (modulo (* (numerator a) (modular-inverse* n (denominator a))) n)]))
  
  (: modular-inverse (Integer Integer -> Natural))
  ;; Return b, where a*b=1 mod n and b in {0,...,n-1}
  (define (modular-inverse a n)
    (cond [(n . <= . 0)  (raise-argument-error 'modular-inverse "Positive-Integer" 1 a n)]
          [else  (modular-inverse* n a)]))
  
  (: modular-expt (Integer Integer Integer -> Natural))
  (define (modular-expt a b n)
    (cond [(n . <= . 0)  (raise-argument-error 'modular-expt "Positive-Integer" 2 a b n)]
          [else  (modular-expt* n a b)]))
  )

(module untyped-defs racket/base
  (require (for-syntax racket/base)
           racket/stxparam
           (submod ".." typed-defs))
  
  (provide (all-defined-out))
  
  (define-syntax-parameter current-modulus-id #f)
  
  ;; Sets the `current-modulus-param' and `current-modulus-id'
  (define-syntax (with-modulus stx)
    (syntax-case stx ()
      [(_ modulus . body)
       (syntax/loc stx
         (let ([n modulus])
           (syntax-parameterize ([current-modulus-id #'n])
             (parameterize ([current-modulus-param n])
               . body))))]))
  
  ;; Checks for `current-modulus-id'; if an identifier, uses the identifier's bound value for the
  ;; modulus; otherwise, gets the current modulus and sets `current-modulus-id' for inner expressions
  (define-syntax (inline-mod-op stx)
    (syntax-case stx ()
      [(_ op-macro a ...)
       (with-syntax ([n  (syntax-parameter-value #'current-modulus-id)])
         (cond [(identifier? #'n)  (syntax/loc stx (op-macro n a ...))]
               [else
                (syntax/loc stx
                  (let ([m  (current-modulus-param)])
                    (syntax-parameterize ([current-modulus-id #'m])
                      (op-macro m a ...))))]))]))
  
  (define-syntax (fold-mod-op stx)
    (syntax-case stx ()
      [(_ op n a b)
       (syntax/loc stx (modulo (op a b) n))]
      [(_ op n a b cs ...)
       (syntax/loc stx
         (fold-mod-op op n (modulo (op a b) n) cs ...))]))
  
  (define-syntax (modular-compare stx)
    (syntax-case stx ()
      [(_ op n a)  (syntax/loc stx #t)]
      [(_ op n a b)  (syntax/loc stx (op (modulo a n) (modulo b n)))]
      [(_ op n a b-expr bs ...)
       (syntax/loc stx
         (let ([b  (modulo b-expr n)])
           (and (op (modulo a n) b)
                (fold-mod-compare-op op n b bs ...))))]))
  
  (define-syntax (fold-mod-compare-op stx)
    (syntax-case stx ()
      [(_ op n a b)
       (syntax/loc stx (op a (modulo b n)))]
      [(_ op n a b-expr bs ...)
       (syntax/loc stx
         (let ([b  (modulo b-expr n)])
           (and (op a b)
                (fold-mod-compare-op op n b bs ...))))]))
  
  (define-syntax (modular+ stx)
    (syntax-case stx ()
      [(_ n)  (syntax/loc stx 0)]
      [(_ n a)  (syntax/loc stx (modulo a n))]
      [(_ n a ...)  (syntax/loc stx (fold-mod-op + n a ...))]))
  
  (define-syntax (modular* stx)
    (syntax-case stx ()
      [(_ n)  (syntax/loc stx 1)]
      [(_ n a)  (syntax/loc stx (modulo a n))]
      [(_ n a ...)  (syntax/loc stx (fold-mod-op * n a ...))]))
  
  (define-syntax (modular- stx)
    (syntax-case stx ()
      [(_ n a)  (syntax/loc stx (modulo (- a) n))]
      [(_ n a b ...)  (syntax/loc stx (fold-mod-op - n a b ...))]))
  
  (define-syntax (modular/ stx)
    (syntax-case stx ()
      [(_ n a)  (syntax/loc stx (modular-inverse n a))]
      [(_ n a b ...)  (syntax/loc stx
                        (modular* n a (modular-inverse* n (modular* n b ...))))]))
  
  (define-syntax-rule (modular-sqr n a) (modulo (* a a) n))
  
  (define-syntax-rule (modular= n a b ...) (modular-compare = n a b ...))
  (define-syntax-rule (modular< n a b ...) (modular-compare < n a b ...))
  (define-syntax-rule (modular<= n a b ...) (modular-compare <= n a b ...))
  (define-syntax-rule (modular> n a b ...) (modular-compare > n a b ...))
  (define-syntax-rule (modular>= n a b ...) (modular-compare <= n a b ...))
  
  (define-syntax-rule (inline-mod+ a ...) (inline-mod-op modular+ a ...))
  (define-syntax-rule (inline-mod* a ...) (inline-mod-op modular* a ...))
  (define-syntax-rule (inline-mod- a b ...) (inline-mod-op modular- a b ...))
  (define-syntax-rule (inline-mod/ a b ...) (inline-mod-op modular/ a b ...))
  (define-syntax-rule (inline-modsqr a) (inline-mod-op modular-sqr a))
  (define-syntax-rule (inline-modexpt a b) (inline-mod-op modular-expt* a b))
  (define-syntax-rule (inline-mod a) (inline-mod-op modular-const* a))
  (define-syntax-rule (inline-mod= a b ...) (inline-mod-op modular= a b ...))
  (define-syntax-rule (inline-mod< a b ...) (inline-mod-op modular< a b ...))
  (define-syntax-rule (inline-mod<= a b ...) (inline-mod-op modular<= a b ...))
  (define-syntax-rule (inline-mod> a b ...) (inline-mod-op modular> a b ...))
  (define-syntax-rule (inline-mod>= a b ...) (inline-mod-op modular>= a b ...))
  )

(require (submod "." typed-defs)
         (submod "." untyped-defs))
