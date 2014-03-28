#lang racket/base

(define the-error "no error")

(require redex/reduction-semantics
         ;(only-in redex/private/generate-term pick-an-index)
         racket/match
         racket/list
         racket/contract
         racket/bool
         redex/tut-subst
         data/union-find)

(provide (all-defined-out))

(define-language stlc
  (M N ::= 
     (λ x M)
     (M N)
     x
     c
     (let ([x M]) N))
  (Γ (x σ Γ)
     •)
  (σ τ ::=
     int
     (list τ)
     (σ → τ)
     x)
  (c d ::= cons nil hd tl + integer)
  ((x y) variable-not-otherwise-mentioned)
  
  (v (λ x M)
     c
     (cons v)
     ((cons v) v)
     (+ v))
  (E hole
     (E M)
     (v E))

  (κ ::= 
     · 
     (λ τ κ)
     (1 Γ M κ)
     (2 τ κ))
  
  (G  ::= · (τ σ G))
  (Gx ::= · (x σ Gx)))

(define v? (redex-match? stlc v))
(define τ? (redex-match? stlc τ))
(define x? (redex-match? stlc x))

(define-judgment-form stlc
  #:mode (typeof I O)
  #:contract (typeof M σ)
  
  [(tc-down • M · σ)
   -------------
   (typeof M σ)])

(define-judgment-form stlc
  #:mode (tc-down I I I O)
  #:contract (tc-down Γ M κ σ)
  
  [(tc-up (const-type c) κ σ_ans)
   ------------------------------
   (tc-down Γ c κ σ_ans)]
  
  [(where τ (lookup Γ x))
   (tc-up τ κ σ_ans)
   ---------------------------
   (tc-down Γ x κ σ_ans)]
  
  [(where y ,(variable-not-in (term (x Γ M κ)) 'α))
   (tc-down (x y Γ) M (λ y κ) σ_ans)  ;; BUG: the continuation had 'x' not 'y' in it
   ------------------------------------------------
   (tc-down Γ (λ x M) κ σ_ans)]
  
  [(tc-down Γ M_1 (1 Γ M_2 κ) σ_2)
   --------------------------
   (tc-down Γ (M_1 M_2) κ σ_2)]

  [(tc-down Γ (subst N x M) κ σ_2)
   ----------------------------------
   (tc-down Γ (let ([x M]) N) κ σ_2)])

(define-judgment-form stlc
  #:mode (tc-up I I O)
  #:contract (tc-up τ κ σ)

  [---------------------
   (tc-up σ_ans · σ_ans)]
  
  [(tc-down Γ M (2 τ κ) σ_ans)
   ---------------------------
   (tc-up τ (1 Γ M κ) σ_ans)]
  
  [(where x ,(variable-not-in (term (τ_1 τ_2 κ)) 'β))
   (where G (unify τ_2 (τ_1 → x)))  ;; BUG: swap τ_2 and τ_1 in this line
   (tc-up (apply-subst-τ G x)
          (apply-subst-κ G κ)
          σ_ans)
   ---------------------------------------------------
   (tc-up τ_1 (2 τ_2 κ) σ_ans)]
  
#|  
  ;; BUG: this case was written assuming that τ_2 was 
  ;; already an arrow type (which is wrong only when it is
  ;; a variable:
  [(where G (unify τ_1 τ_2))
   (tc-up (apply-subst-τ G τ_3)
          (apply-subst-κ G κ)
          σ_ans)
   -----------------------------------
   (tc-up τ_1 (2 (τ_2 → τ_3) κ) σ_ans)]
|#  
  
  [(tc-up (τ_1 → τ_2) κ σ_ans)
   ---------------------------
   (tc-up τ_2 (λ τ_1 κ) σ_ans)])

(define-metafunction stlc
  unify : τ τ -> Gx or ⊥
  [(unify τ σ) (uh (τ σ ·) · #f)])

#|

Algorithm copied from _An Efficient Unification Algorithm_ by 
Alberto Martelli and Ugo Montanari (section 2).
http://www.nsl.com/misc/papers/martelli-montanari.pdf

The two iterate and the terminate rule are just how this code
searches for places to apply the rules; they are not from that 
section in the paper.

|#

(define-metafunction stlc
  uh : G G boolean -> Gx or ⊥
  ;; iterate
  [(uh · G #t) (uh G · #f)]
  ;; terminate
  [(uh · G #f) G]
  
  ;; (a)
  [(uh (τ x G) G_r boolean) (uh G (x τ G_r) #t) (where #t (not-var? τ))]
  
  ;; (b)
  [(uh (x x G) G_r boolean) (uh G G_r #t)]
  
  ;; (c) -- term reduction
  [(uh ((τ_1 → τ_2) (σ_1 → σ_2) G) G_r boolean) (uh (τ_1 σ_1 (τ_2 σ_2 G)) G_r #t)]
  [(uh ((list τ)    (list σ)    G) G_r boolean) (uh (τ σ G)               G_r #t)]
  [(uh (int         int         G) G_r boolean) (uh G                     G_r #t)]
  
  ;; (c) -- failure
  [(uh (τ σ G) G_r boolean) ⊥ (where #t (not-var? τ)) (where #t (not-var? σ))]
  
  ;; (d) -- x occurs in τ case
  ;; BUG: had (in-vars? x τ) not (in-vars-τ? x τ)
  [(uh (x τ G) G_r boolean) ⊥ (where #t (in-vars-τ? x τ))]
  
  ;; (d) -- x does not occur in τ case
  [(uh (x τ G) G_r boolean)
   (uh (eliminate-G x τ G) (x τ (eliminate-G x τ G_r)) #t)
   (where #t (∨ (in-vars-G? x G) (in-vars-G? x G_r)))]
  
  ;; iterate
  [(uh (τ σ G) G_r boolean) (uh G (τ σ G_r) boolean)])

(define-metafunction stlc
  eliminate-G : x τ G -> G
  [(eliminate-G x τ ·) ·]
  [(eliminate-G x τ (σ_1 σ_2 G))
   ((eliminate-τ x τ σ_1) (eliminate-τ x τ σ_2) (eliminate-G x τ G))])

#|
;; BUG: eliminate-G was written as if it was getting a Gx as input:
(define-metafunction stlc
  eliminate-G : x τ G -> G
  [(eliminate-G x τ ·) ·]
  [(eliminate-G x τ (x σ G))
   (τ (eliminate-τ x τ σ) (eliminate-G x τ G))]
  [(eliminate-G x τ (y σ G))
   (y (eliminate-τ x τ σ) (eliminate-G x τ G))])
|#

(define-metafunction stlc
  eliminate-τ : x τ σ -> σ
  [(eliminate-τ x τ (σ_1 → σ_2)) ((eliminate-τ x τ σ_1) → (eliminate-τ x τ σ_2))]
  [(eliminate-τ x τ (list σ)) (list (eliminate-τ x τ σ))]
  [(eliminate-τ x τ int) int]
  [(eliminate-τ x τ x) τ]
  [(eliminate-τ x τ y) y])

(define-metafunction stlc
  ∨ : boolean boolean -> boolean
  [(∨ #f #f) #f]
  
  ;; BUG: this case was [(∨ boolean boolean) #t]
  ;; (which, of course, means that the two bools have to be the same)
  [(∨ boolean_1 boolean_2) #t])

(define-metafunction stlc
  not-var? : τ -> boolean
  [(not-var? x) #f]
  [(not-var? τ) #t])

(define-metafunction stlc
  in-vars-τ? : x τ -> boolean
  [(in-vars-τ? x (τ_1 → τ_2)) (∨ (in-vars-τ? x τ_1) (in-vars-τ? x τ_2))]
  [(in-vars-τ? x (list τ)) (in-vars-τ? x τ)]
  [(in-vars-τ? x int) #f]
  [(in-vars-τ? x x) #t]
  [(in-vars-τ? x y) #f])

(define-metafunction stlc
  in-vars-G? : x G -> boolean
  [(in-vars-G? x ·) #f]
  [(in-vars-G? x (x τ G)) #t]
  [(in-vars-G? x (σ τ G)) (∨ (in-vars-τ? x σ) 
                             (∨ (in-vars-τ? x τ)
                                (in-vars-G? x G)))])

(define-metafunction stlc
  apply-subst-τ : Gx τ -> τ
  [(apply-subst-τ · τ) τ]
  [(apply-subst-τ (x τ G) σ)
   (apply-subst-τ G (eliminate-τ x τ σ))])

(define-metafunction stlc
  apply-subst-κ : Gx κ -> κ
  [(apply-subst-κ Gx ·) ·]
  [(apply-subst-κ Gx (λ σ κ)) 
   (λ (apply-subst-τ Gx σ) (apply-subst-κ Gx κ))]
  [(apply-subst-κ Gx (1 Γ M κ))
   (1 (apply-subst-Γ Gx Γ) M (apply-subst-κ Gx κ))]
  [(apply-subst-κ Gx (2 σ κ))
   (2 (apply-subst-τ Gx σ)
      (apply-subst-κ Gx κ))])

(define-metafunction stlc
  apply-subst-Γ : Gx Γ -> Γ
  [(apply-subst-Γ Gx (y σ Γ))
   (y (apply-subst-τ Gx σ)
      (apply-subst-Γ Gx Γ))]
  [(apply-subst-Γ Gx •) •])

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
  [(lookup (x σ Γ) y) 
   (lookup Γ y)]
  [(lookup • x)
   #f])

(define red
  (reduction-relation
   stlc
   (--> (in-hole E ((λ x M) v))
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
  (define M-t (judgment-holds (typeof ,M τ) τ))
  (unless (pair? M-t)
    (error 'Eval "doesn't typecheck: ~s" M))
  (define res (apply-reduction-relation* red M))
  (unless (= 1 (length res))
    (error 'Eval "internal error: not exactly 1 result ~s => ~s" M res))
  (define ans (car res))
  (if (equal? "error" ans)
      "error"
      (let ([ans-t (judgment-holds (typeof ,ans τ) τ)])
        (unless (equal? M-t ans-t)
          (error 'Eval "internal error: type soundness fails for ~s" M))
        ans)))

(define-metafunction stlc
  subst : M x M -> M
  [(subst M x N) 
   ,(subst/proc x? (term (x)) (term (N)) (term M))])

(define/contract (type-check M)
  (-> M? (or/c τ? #f))
  (define M-t (judgment-holds (typeof ,M τ) τ))
  (cond
    [(empty? M-t)
     #f]
    [(null? (cdr M-t))
     (car M-t)]
    [else
     (error 'type-check "non-unique type: ~s : ~s" M M-t)]))

(define (progress-holds? M)
  (if (type-check M)
      (or (v? M)
          (not (null? (apply-reduction-relation red (term ,M)))))
      #t))

(define-metafunction stlc   ;; LOC SKIP START
  [(uses-bound-var? (x_0 ... x_1 x_2 ...) x_1)
   #t]
  [(uses-bound-var? (x_0 ...) (λ x M))
   (uses-bound-var? (x x_0 ...) M)]
  [(uses-bound-var? (x ...) (M N))
   ,(or (term (uses-bound-var? (x ...) M))
        (term (uses-bound-var? (x ...) N)))]
  [(uses-bound-var? (x ...) (cons M))
   (uses-bound-var? (x ...) M)]
  [(uses-bound-var? (x ...) any)
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

(define (generate-M-term)
  (generate-term stlc M 5))

(define (generate-M-term-from-red)
  (generate-term #:source red 5))

(define (generate-typed-term)
  (match (generate-term stlc 
                        #:satisfying
                        (typeof • M τ)
                        5)
    [`(typeof • ,M ,τ)
     M]
    [#f #f]))

(define (typed-generator)
  (let ([g (redex-generator stlc 
                            (typeof • M τ)
                            5)])
    (λ () 
      (match (g)
        [`(typeof • ,M ,τ)
         M]
        [#f #f]))))

(define (check term)
  (or (not term)
      (v? term)
      (let ([t-type (type-check term)])
        (implies
         t-type
         (let ([red-res (apply-reduction-relation red term)])
           (and (= (length red-res) 1)
                (let ([red-t (car red-res)])
                  (or (equal? red-t "error")
                      (let ([red-type (type-check red-t)])
                        (equal? t-type red-type))))))))))

(define (generate-enum-term)
  '(generate-term stlc M #:i-th (pick-an-index 0.035)))

(define (ordered-enum-generator)
  (let ([index 0])
    (λ ()
      (begin0
        (generate-term stlc M #:i-th index)
        (set! index (add1 index))))))
