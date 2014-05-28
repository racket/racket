#lang racket/base

(require redex/reduction-semantics
         (only-in redex/private/generate-term pick-an-index)
         racket/match
         racket/list
         racket/contract
         racket/bool
         redex/tut-subst)

(provide (all-defined-out))

(define-language stlc
  (M N ::= 
     (λ (x σ) M)
     (M N)
     x
     c)
  (Γ (x σ Γ)
     •)
  (σ τ ::=
     int
     (list int)
     (σ → τ))
  (c d ::= cons nil hd tl + integer)
  ((x y) variable-not-otherwise-mentioned)
  
  (v (λ (x τ) M)
     c
     (cons v)
     ((cons v) v)
     (+ v))
  (E hole
     (E M)
     (v E)))

(define-judgment-form stlc
  #:mode (typeof I I O)
  #:contract (typeof Γ M σ)
  
  [---------------------------
   (typeof Γ c (const-type c))]
  
  [(where τ (lookup Γ x))
   ----------------------
   (typeof Γ x τ)]
  
  [(typeof (x σ Γ) M σ_2)
   --------------------------------
   (typeof Γ (λ (x σ) M) (σ → σ_2))]
  
  [(typeof Γ M (σ → σ_2))
   (typeof Γ M_2 σ)
   ----------------------
   (typeof Γ (M M_2) σ_2)])

(define-metafunction stlc
  const-type : c -> σ
  [(const-type nil)
   (list int)]
  [(const-type cons)
   (int → ((list int) → (list int)))]
  [(const-type hd)
   ((list int) → int)]
  [(const-type tl)
   ((list int) → (list int))]
  [(const-type +)
   (int → (int → int))]
  [(const-type integer)
   int])

(define-metafunction stlc
  lookup : Γ x -> σ or #f
  [(lookup (x σ Γ) x)
   σ]
  [(lookup (x σ Γ) x_2)
   (lookup Γ x_2)]
  [(lookup • x)
   #f])

(define red
  (reduction-relation
   stlc
   (--> (in-hole E ((λ (x τ) M) v))
        (in-hole E (subst M x v))
        "βv")
   (--> (in-hole E (hd ((cons v_1) v_2)))
        (in-hole E v_1)
        "hd")
   (--> (in-hole E (tl ((cons v_1) v_2)))
        (in-hole E v_2)
        "tl")
   (--> (in-hole E (hd nil))
        "error"
        "hd-err")
   (--> (in-hole E (tl nil))
        "error"
        "tl-err")
   (--> (in-hole E ((+ integer_1) integer_2))
        (in-hole E ,(+ (term integer_1) (term integer_2)))
        "+")))

(define M? (redex-match stlc M))
(define/contract (Eval M)
  (-> M? (or/c M? "error"))
  (define M-t (judgment-holds (typeof • ,M τ) τ))
  (unless (pair? M-t)
    (error 'Eval "doesn't typecheck: ~s" M))
  (define res (apply-reduction-relation* red M))
  (unless (= 1 (length res))
    (error 'Eval "internal error: not exactly 1 result ~s => ~s" M res))
  (define ans (car res))
  (if (equal? "error" ans)
      "error"
      (let ([ans-t (judgment-holds (typeof • ,ans τ) τ)])
        (unless (equal? M-t ans-t)
          (error 'Eval "internal error: type soundness fails for ~s" M))
        ans)))

(define x? (redex-match stlc x))
(define-metafunction stlc
  subst : M x M -> M
  [(subst M x N) 
   ,(subst/proc x? (term (x)) (term (N)) (term M))])

(define v? (redex-match? stlc v))
(define τ? (redex-match? stlc τ))
(define/contract (type-check M)
  (-> M? (or/c τ? #f))
  (define M-t (judgment-holds (typeof • ,M τ) τ))
  (cond
    [(empty? M-t)
     #f]
    [(null? (cdr M-t))
     (car M-t)]
    [else
     (error 'type-check "non-unique type: ~s : ~s" M M-t)]))

(test-equal (type-check (term 5))
            (term int))
(test-equal (type-check (term (5 5)))
            #f)

(define (progress-holds? M)
  (if (type-check M)
      (or (v? M)
          (not (null? (apply-reduction-relation red (term ,M)))))
      #t))

(define (interesting-term? M)
  (and (type-check M)
       (term (uses-bound-var? () ,M))))

(define-metafunction stlc
  [(uses-bound-var? (x_0 ... x_1 x_2 ...) x_1)
   #t]
  [(uses-bound-var? (x_0 ...) (λ (x τ) M))
   (uses-bound-var? (x x_0 ...) M)]
  [(uses-bound-var? (x ...) (M N))
   ,(or (term (uses-bound-var? (x ...) M))
        (term (uses-bound-var? (x ...) N)))]
  [(uses-bound-var? (x ...) (cons M))
   (uses-bound-var? (x ...) M)]
  [(uses-bound-var? (x ...) any)
   #f])

(define (really-interesting-term? M)
  (and (interesting-term? M)
       (term (applies-bv? () ,M))))

(define-metafunction stlc
  [(applies-bv? (x_0 ... x_1 x_2 ...) (x_1 M))
   #t]
  [(applies-bv? (x_0 ...) (λ (x τ) M))
   (applies-bv? (x x_0 ...) M)]
  [(applies-bv? (x ...) (M N))
   ,(or (term (applies-bv? (x ...) M))
        (term (applies-bv? (x ...) N)))]
  [(applies-bv? (x ...) (cons M))
   (applies-bv? (x ...) M)]
  [(applies-bv? (x ...) any)
   #f])

(define (reduction-step-count/func red v?)
  (λ (term)
    (let loop ([t term]
               [n 0])
      (define res (apply-reduction-relation red t))
      (cond 
        [(and (empty? res)
              (v? t))
         n]
        [(and (empty? res)
              (equal? t "error"))
         n]
        [(= (length res) 1)
         (loop (car res) (add1 n))]
        [else
         (error 'reduction-step-count "failed reduction: ~s\n~s\n~s" term t res)]))))

(define reduction-step-count
  (reduction-step-count/func red v?))


(module+ test
  
  (require redex/examples/stlc-tests-lib)
  
  (stlc-tests uses-bound-var?
              typeof
              red
              reduction-step-count
              Eval
              subst))