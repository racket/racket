#lang typed/racket/base

(require racket/performance-hint
         racket/flonum
         "divisibility.rkt"
         "../exception.rkt")

(provide modular-inverse
         modular-expt
         current-modulus
         mod+ mod- mod* mod/ modsqr modexpt
         mod
         mod= mod< mod<= mod> mod>=)

(: current-modulus (Parameterof Positive-Integer))
(define current-modulus (make-parameter 1))

; THEOREM
;  If gcd(a,n)=1 then there exist b such that
;    ab=1 mod n
;  The number b is called an inverse of a modulo n.

(: modular-inverse (Integer Integer -> Natural))
;; Return b, where a*b=1 mod n and b in {0,...,n-1}
(define (modular-inverse a n)
  (cond [(zero? a)  (raise-argument-error 'modular-inverse "nonzero Integer" 0 a n)]
        [(n . <= . 0)  (raise-argument-error 'modular-inverse "Positive-Integer" 1 a n)]
        [(coprime? a n)  (modulo (car (bezout a n)) n)]
        [else  (error 'mod/ "expected argument that is coprime to modulus ~e; given ~e" n a)]))

(: modular-expt (Integer Integer Integer -> Natural))
;; Exponentiate by repeated modular multiplication and squaring
(define (modular-expt a b n)
  (cond [(b . < . 0)  (raise-argument-error 'modular-expt "Natural" 1 a b n)]
        [(n . <= . 0)  (raise-argument-error 'modular-expt "Positive-Integer" 2 a b n)]
        [else
         (let loop ([a a] [b b])
           (cond [(b . <= . 1)  (if (zero? b) (modulo 1 n) (modulo a n))]
                 [(even? b)  (define c (loop a (quotient b 2)))
                             (modulo (* c c) n)]
                 [else  (modulo (* a (loop a (sub1 b))) n)]))]))

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
                  (cond [(null? cs)  #t]
                        [else
                         (let ([c  (modulo (car cs) n)])
                           (and (op b c) (loop c (cdr cs))))]))))]))))

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
