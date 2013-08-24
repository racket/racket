#lang racket

(require redex/pict
         redex/reduction-semantics)

(provide (all-defined-out))

;; This file makes some small changes to the system in
;; typing-rules.rkt (in the same directory) to allow generation
;; of terms that satisfy the "typeof" judgment-form.  Specifically,
;; since generation doesn't yet support ellipses, they have to be
;; eliminated from the judgment-form and the metafunctions it depends on.

(define-language STLC
  (e (λ (x τ) e)
     (e e)
     x
     i
     add1)
  (τ int
     (τ → τ))
  (Γ ([x τ] Γ)
     ())
  (i integer)
  (x variable-not-otherwise-mentioned))

(define-judgment-form STLC
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)
  
  [(typeof ([x τ_1] Γ) e τ_2)
   ------------------------------------
   (typeof Γ (λ (x τ_1) e) (τ_1 → τ_2))]
  
  [(typeof Γ e_1 (τ_2 → τ))
   (typeof Γ e_2 τ_2)
   ------------------------
   (typeof Γ (e_1 e_2) τ)]
  
  [(where τ (lookup Γ x))
   --------------
   (typeof Γ x τ)]
  
  [----------------
   (typeof Γ i int)]
  
  [---------------------------
   (typeof Γ add1 (int → int))])


(define-metafunction STLC
  lookup : Γ x -> τ
  [(lookup ([x τ] Γ) x) 
   τ]
  [(lookup ([x_1 τ] Γ) x_0)
   (lookup Γ x_0)]
  [(lookup () x)
   #f])

(test-equal 
 (judgment-holds 
  (typeof () 
          (λ (x int) 
            (λ (x (int → int))
              (x (add1 7))))
          τ)
  τ)
 (list (term (int → ((int → int) → int)))))
(test-equal 
 (judgment-holds
  (typeof () 
          (λ (x int)
            (λ (x (int → int))
              (add1 x)))
          τ))
 #f)


(define (random-typed-term)
  (generate-term STLC
                 #:satisfying
                 (typeof () e τ)
                 5))

(define (random-typed-terms n)
  (define gen-one (redex-generator STLC (typeof () e τ) 5))
  (for/list ([_ n])
    (extract-term-from-derivation 
     (gen-one))))

(define (extract-term-from-derivation t)
  (match t
    [`(typeof () ,e ,t)
     ;; test to make sure the generator
     ;; generated something that the 
     ;; judgment form actually accepts
     (define types (judgment-holds (typeof () ,e τ) τ))
     (unless (= 1 (length types))
       (error 'typeof "non-unique types: ~s in ~s\n" types e))
     (test-equal (car types) t)
     e]
    [#f #f]))
