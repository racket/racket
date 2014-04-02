#lang racket/base

(require redex/reduction-semantics
         (only-in redex/private/generate-term pick-an-index)
         racket/bool
         racket/match
         racket/contract
         "tut-subst.rkt")

(provide (all-defined-out))

(define the-error "evaluation isn't allowed on the rhs of applications")

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
     (E M)))

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
  
  ;[(where (∀ α (∀ β γ)) (const-type c))
  ; (where σ (t-subst2 γ α τ_1 β τ_2))
  ; ------------------------------
  ; (typeof-C [[c @ τ_1] @ τ_2] σ)]
  
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

(define-metafunction poly-stlc
  t-subst2 : γ α τ α τ -> γ
  [(t-subst2 int α_1 τ_1 α_2 τ_2)
   int]
  [(t-subst2 α_1 α_1 τ_1 α_2 τ_2)
   τ_1]
  [(t-subst2 α_2 α_1 τ_1 α_2 τ_2)
   τ_2]
  [(t-subst2 β α_1 τ_1 α_2 τ_2)
   β]
  [(t-subst2 (list γ) α_1 τ_1 α_2 τ_2)
   (list (t-subst2 γ α_1 τ_1 α_2 τ_2))]
  [(t-subst2 (γ → γ_2) α_1 τ_1 α_2 τ_2)
   ((t-subst2 γ α_1 τ_1 α_2 τ_2) → (t-subst2 γ_2 α_1 τ_1 α_2 τ_2))])

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

(define (generate-M-term)
  (generate-term poly-stlc M 5))

(define (generate-M-term-from-red)
  (generate-term #:source red 5))

(define (generate-typed-term)
  (match (generate-term poly-stlc #:satisfying (typeof • M τ) 5)
    [`(typeof • ,M ,τ)
     M]
    [#f #f]))

(define (generate-typed-term-from-red)
  (match (case (random 7)
           [(0)
            (generate-term poly-stlc #:satisfying (typeof • ((λ (x τ_x) M) v) τ) 5)]
           [(1)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((hd @ τ_c) (((cons @ τ_c) v_1) v_2)) τ) 5)]
           [(2)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((tl @ τ_c) (((cons @ τ_c) v_1) v_2)) τ) 5)]
           [(3)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((hd @ τ_h) (nil @ τ_h)) τ) 5)]
           [(4)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((tl @ τ_t) (nil @ τ_t)) τ) 5)]
           [(5)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((((map @ τ_1) @ τ_2) v) (nil @ τ_1)) τ) 5)]
           [(6)
            (generate-term poly-stlc #:satisfying 
                           (typeof • ((((map @ τ_1) @ τ_2) v) (((cons @ τ_1) v_1) v_2)) τ) 5)])
    [`(typeof • ,M ,τ)
     M]
    [#f #f]))
(define (typed-generator)
  (let ([g (redex-generator poly-stlc (typeof • M τ) 5)])
    (λ ()
      (match (g)
        [`(typeof • ,M ,τ)
         M]
        [#f #f]))))

;; check : (or/c #f M[from poly-stlc language]) -> boolean[#f = counterexample found!]
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

(define (generate-enum-term [p-value 0.065])
  (generate-term poly-stlc M #:i-th (pick-an-index p-value)))

(define (ordered-enum-generator)
  (let ([index 0])
    (λ ()
      (begin0
        (generate-term poly-stlc M #:i-th index)
        (set! index (add1 index))))))

(define small-counter-example
  (term ((+ 1) ((+ 2) 3))))
(test-equal (check small-counter-example) #f)
