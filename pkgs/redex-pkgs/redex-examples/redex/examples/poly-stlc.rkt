#lang racket/base

(require redex/reduction-semantics
         (only-in redex/private/generate-term pick-an-index)
         racket/bool
         racket/match
         racket/contract
         redex/tut-subst)

(provide (all-defined-out))

(define-language poly-stlc
  (M N ::= 
     (λ (x σ) M)
     (M N)
     C
     integer
     x)
  (C ::=
     [C @ σ]
     c)
  (σ τ ::= 
     int
     (σ → τ)
     (list σ))
  (γ ::=
     int
     (γ → γ)
     (list γ)
     α)
  (Γ ::=
     (x σ Γ)
     •)
  (Σ Y ::= 
     (∀ α Σ)
     γ)
  (x y ::= variable-not-otherwise-mentioned)
  (α β ::= variable-not-otherwise-mentioned)
  (c d ::= map nil cons hd tl +)
  
  (v (λ (x τ) M)
     C
     integer
     (+ v)
     ([cons @ τ] v)
     (([cons @ τ] v) v)
     ([[map @ τ_1] @ τ_2] v))
  (E hole
     (E M)
     (v E)))

;; overlaps: random seed 35

(define-judgment-form poly-stlc
  #:mode (typeof I I O)
  
  [---------------------
   (typeof Γ number int)]
  
  [(typeof-C C τ)
   --------------
   (typeof Γ C τ)]
  
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

(define-judgment-form poly-stlc 
  #:mode (typeof-C I O)
  
  [(where (∀ α γ) (const-type c))
   (where σ (t-subst γ α τ))
   ------------------------------
   (typeof-C [c @ τ] σ)]
  
  [(where (∀ α (∀ β γ)) (const-type c))
   (where γ_1 (t-subst γ β τ_2))
   (where σ (t-subst γ_1 α τ_1))
   ------------------------------
   (typeof-C [[c @ τ_1] @ τ_2] σ)]
  
  [(where γ (const-type c))
   ------------------------------
   (typeof-C c γ)])

(define-extended-judgment-form poly-stlc typeof
  #:mode (typ-ind I I O)
  [(where (∀ α σ_c) (const-type c))
   (where (σ → τ) (t-subst σ_c α σ_1))
   (typ-ind Γ M σ)
   -----------------------------------
   (typ-ind Γ ([c @ σ_1] M) τ)])

(define-metafunction poly-stlc
  lookup : Γ x -> σ or #f
  [(lookup (x σ Γ) x)
   σ]
  [(lookup (x σ Γ) x_2)
   (lookup Γ x_2)]
  [(lookup • x)
   #f])

(define-metafunction poly-stlc 
  const-type : c -> Σ
  [(const-type nil)
   (∀ b (list b))]
  [(const-type cons)
   (∀ a (a → ((list a) → (list a))))]
  [(const-type hd)
   (∀ a ((list a) → a))]
  [(const-type tl)
   (∀ a ((list a) → (list a)))]
  [(const-type map)
   (∀ α (∀ β ((α → β) → ((list α) → (list β)))))]
  [(const-type +)
   (int → (int → int))])

(define-metafunction poly-stlc
  t-subst : γ α τ -> γ
  [(t-subst int α τ)
   int]
  [(t-subst α α τ)
   τ]
  [(t-subst α β τ)
   α]
  [(t-subst (list γ) α τ)
   (list (t-subst γ α τ))]
  [(t-subst (γ → γ_2) α τ)
   ((t-subst γ α τ) → (t-subst γ_2 α τ))])

(define red
  (reduction-relation
   poly-stlc
   (--> (in-hole E ((λ (x τ) M) v))
        (in-hole E (subst M x v))
        "βv")
   (--> (in-hole E ((hd @ τ) (((cons @ τ) v_1) v_2)))
        (in-hole E v_1)
        "hd")
   (--> (in-hole E ((tl @ τ) (((cons @ τ) v_1) v_2)))
        (in-hole E v_2)
        "tl")
   (--> (in-hole E ((((map @ τ_1) @ τ_2) v) (nil @ τ_1)))
        (in-hole E (nil @ τ_2))
        "map-nil")
   (--> (in-hole E ((((map @ τ_1) @ τ_2) v) (((cons @ τ_1) v_1) v_2)))
        (in-hole E (((cons @ τ_2) (v v_1)) ((((map @ τ_1) @ τ_2) v) v_2)))
        "map-cons")
   (--> (in-hole E ((hd @ τ) (nil @ τ)))
        "error"
        "hd-err")
   (--> (in-hole E ((tl @ τ) (nil @ τ)))
        "error"
        "tl-err")
   (--> (in-hole E ((+ integer_1) integer_2))
        (in-hole E ,(+ (term integer_1) (term integer_2)))
        "+")))

(define M? (redex-match poly-stlc M))
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

(define x? (redex-match poly-stlc x))
(define-metafunction poly-stlc
  subst : M x M -> M
  [(subst M x N) 
   ,(subst/proc x? (term (x)) (term (N)) (term M))])

(define v? (redex-match? poly-stlc v))
(define τ? (redex-match? poly-stlc τ))
(define/contract (type-check M)
  (-> M? (or/c τ? #f))
  (define M-t (judgment-holds (typeof • ,M τ) τ))
  (cond
    [(null? M-t)
     #f]
    [(null? (cdr M-t))
     (car M-t)]
    [else
     (error 'type-check "non-unique type: ~s : ~s" M M-t)]))

(module+ test
  
  (test-equal (judgment-holds (typeof • 5 τ) τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof • [nil @ int] τ) τ)
              (list (term (list int))))
  (test-equal (judgment-holds (typeof • ([cons @ int] 1) τ) τ)
              (list (term ((list int) → (list int)))))
  (test-equal (judgment-holds (typeof • (([cons @ int] 1) [nil @ int]) τ) τ)
              (list (term (list int))))
  (test-equal (judgment-holds (typeof • (λ (x int) x) τ) τ)
              (list (term (int → int))))
  (test-equal (judgment-holds (typeof • (λ (x (int → int)) (λ (y int) x)) τ) τ)
              (list (term ((int → int) → (int → (int → int))))))
  (test-equal (judgment-holds
               (typeof •
                       ([tl @ int]
                        ([hd @ (list int)]
                         ((λ (l (list (list int)))
                            (([cons @ (list int)] (([cons @ int] 1) [nil @ int]))
                             l))
                          [nil @ (list int)])))
                       τ)
               τ)
              (list (term (list int))))
  (test-equal (judgment-holds
               (typeof •
                       (([[map @ int] @ (list int)]
                         (λ (x int) (([cons @ int] x) [nil @ int])))
                        (([cons @ int] 2)
                         (([cons @ int] 4)
                          [nil @ int])))
                       τ)
               τ)
              (list (term (list (list int)))))
  
  (test-->> red (term ((λ (x int) x) 7)) (term 7))
  (test-->> red (term (((λ (x int) (λ (x int) x)) 2) 1)) (term 1))
  (test-->> red (term (((λ (x int) (λ (y int) x)) 2) 1)) (term 2))
  (test-->> red 
            (term ((λ (x int) (([cons @ int] x) [nil @ int])) 11))
            (term (([cons @ int] 11) [nil @ int])))
  (test-->> red 
            (term ((λ (x int) (([cons @ int] x) [nil @ int])) 11))
            (term (([cons @ int] 11) [nil @ int])))
  (test-->> red 
            (term (([cons @ int] ((λ (x int) x) 11)) [nil @ int]))
            (term (([cons @ int] 11) [nil @ int])))
  (test-->> red
            (term ([cons @ int] ((λ (x int) x) 1)))
            (term ([cons @ int] 1)))
  (test-->> red
            (term (([cons @ int] ((λ (x int) x) 1)) [nil @ int]))
            (term (([cons @ int] 1) [nil @ int])))
  (test-->> red
            (term ([hd @ int] ((λ (x int) (([cons @ int] x) [nil @ int])) 11)))
            (term 11))
  (test-->> red
            (term ([tl @ int] ((λ (x int) (([cons @ int] x) [nil @ int])) 11)))
            (term [nil @ int]))
  (test-->> red
            (term ([tl @ int] [nil @ int]))
            "error")
  (test-->> red
            (term ([hd @ int] [nil @ int]))
            "error")
  (test-->> red
            (term ((λ (f (int → (list int))) (f 3)) ([cons @ int] 1)))
            (term (([cons @ int] 1) 3)))
  (test-->> red
            (term 
             ([tl @ int]
              ([hd @ (list int)]
               ((λ (l (list (list int)))
                  (([cons @ (list int)] (([cons @ int] 1) [nil @ int]))
                   l))
                [nil @ (list int)]))))
            (term [nil @ int]))
  
  (test-->> red
            (term (([[map @ int] @ (list int)]
                    (λ (x int) (([cons @ int] x) [nil @ int])))
                   (([cons @ int] 2)
                    (([cons @ int] 4)
                     [nil @ int]))))
            (term (((cons @ (list int)) (((cons @ int) 2) (nil @ int)))
                   (((cons @ (list int)) (((cons @ int) 4) (nil @ int)))
                    (nil @ (list int))))))
  (test-equal (Eval (term ((λ (x int) x) 3)))
              (term 3))
  
  
  (test-equal (judgment-holds (typeof • ((+ ((+ 1) 2)) ((+ 3) 4)) τ) τ)
              (list (term int)))
  (test-->> red
            (term ((+ 1) ([hd @ int] [nil @ int])))
            "error")
  (test-->> red
            (term ((+ ((+ 1) 2)) ((+ 3) 4)))
            (term 10))
  (test-->> red
            (term ((λ (f (int → int)) (f 3)) (+ 1)))
            (term 4))
  
  (test-results))

