#lang racket
(require redex
         "list-machine.rkt")

(provide list-machine-typing
         check-program
         check-blocks
         check-block
         check-instr
         :set :lookup
         ⊂ ⊔ Γ-⊂
         dom l-⊂)

(define-extended-language list-machine-typing list-machine
  (τ nil (list τ) (listcons τ))
  (Γ empty (v : τ Γ))
  (Π empty (l : Γ Π)))

(define-judgment-form list-machine-typing
  #:contract (check-program p Π)
  #:mode (check-program I I)
  [(check-blocks Π p)
   (where #t (l-⊂ (dom Π) (dom p)))
   (:lookup Π l0 (v0 : nil empty))
   -----
   (check-program p Π)])

(define-judgment-form list-machine-typing
  #:contract (Γ-⊂ Γ Γ)
  #:mode (Γ-⊂ I I)
  [-----
   (Γ-⊂ Γ empty)]
  [(:lookup Γ_1 v τ_1) 
   (⊂ τ_1 τ_2)
   (Γ-⊂ Γ_1 Γ_2)
   ----
   (Γ-⊂ Γ_1 (v : τ_2 Γ_2))])

(define-judgment-form list-machine-typing
  #:contract (check-blocks Π p)
  #:mode (check-blocks I I)
  [(:lookup Π l Γ)
   (check-block Π Γ ι)
   (check-blocks Π p)
   -----
   (check-blocks Π (l : ι p)) ]
  [-----
   (check-blocks Π end)])

(define-judgment-form list-machine-typing
  #:contract (check-block Π Γ ι)
  #:mode (check-block I I I)
  [-----
   (check-block Π Γ halt)]
  [(check-instr Π Γ ι_1 Γ_2) 
   (check-block Π Γ_2 ι_2)
   -----
   (check-block Π Γ (begin ι_1 ι_2))]
  [(:lookup Π l Γ_2)
   (Γ-⊂ Γ Γ_2)
   -----
   (check-block Π Γ (jump l))])

(define-judgment-form list-machine-typing
  #:contract (check-instr Π Γ ι Γ)
  #:mode (check-instr I I I O)
  [(check-instr Π Γ ι_1 Γ_1)
   (check-instr Π Γ_1 ι_2 Γ_2)
   -----
   (check-instr Π Γ (begin ι_1 ι_2) Γ_2)]
  [(:lookup Γ v (list τ))
   (:lookup Π l Γ_1)
   (:set Γ v nil Γ_3)
   (Γ-⊂ Γ_3 Γ_1)
   (:set Γ_3 v (listcons τ) Γ_2)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ_2)]
  [(:lookup Γ v (listcons τ))
   (:lookup Π l Γ_1)
   (:set Γ v nil Γ_2)
   (Γ-⊂ Γ_2 Γ_1)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ)]
  [(:lookup Γ v nil)
   (:lookup Π l Γ_1)
   (Γ-⊂ Γ Γ_1)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ)]
  [(:lookup Γ v (listcons τ)) (:set Γ v_2 τ Γ_2)
   -----
   (check-instr Π Γ (fetch-field v 0 v_2) Γ_2)]
  [(:lookup Γ v (listcons τ)) (:set Γ v_2 (list τ) Γ_2)
   -----
   (check-instr Π Γ (fetch-field v 1 v_2) Γ_2)]
  [(:lookup Γ v_0 τ_0) (:lookup Γ v_1 τ_1)
   (⊔ (list τ_0) τ_1 (list τ)) (:set Γ v (listcons τ) Γ_2)
   -----
   (check-instr Π Γ (cons v_0 v_1 v) Γ_2)])
(define-judgment-form list-machine-typing
  #:contract (⊂ τ τ)
  #:mode (⊂ O I)
  [-----
   (⊂ τ τ)]
  [-----
   (⊂ nil (list τ))]
  [(⊂ τ τ_2)
   -----
   (⊂ (list τ) (list τ_2))]
  [(⊂ τ τ_2)
   -----
   (⊂ (listcons τ) (list τ_2))]
  [(⊂ τ τ_2)
   -----
   (⊂ (listcons τ) (listcons τ_2))])

(define-judgment-form list-machine-typing
  #:contract (⊔ τ τ τ)
  #:mode (⊔ I I O)
  [-----
   (⊔ τ τ τ)]
  [-----
   (⊔ (list τ) nil (list τ))]
  [-----
   (⊔ nil (list τ) (list τ))]
  [(⊔ (list τ_1) (list τ_2) τ_3)
   -----
   (⊔ (list τ_1) (listcons τ_2) τ_3)]
  [(⊔ (list τ_1) (list τ_2) τ_3)
   -----
   (⊔ (listcons τ_1) (list τ_2) τ_3)]
  [(⊔ τ_1 τ_2 τ_3)
   -----
   (⊔ (list τ_1) (list τ_2) (list τ_3))]
  [-----
   (⊔ (listcons τ) nil (list τ))]
  [-----
   (⊔ nil (listcons τ) (list τ))]
  [(⊔ τ_1 τ_2 τ_3)
   -----
   (⊔ (listcons τ_1) (listcons τ_2) (listcons τ_3))])

(define-judgment-form list-machine-typing
  #:contract (:lookup any v any)
  #:mode (:lookup I I O)
  [-----
   (:lookup (v : any_τ any_Γ) v any_τ)]
  [(where #t (different v_1 v_2))
   (:lookup any_Γ v_2 any_τ2)
   -----
   (:lookup (v_1 : any_τ1 any_Γ) v_2 any_τ2)])

(define-judgment-form list-machine-typing
  #:contract (:set Γ v τ Γ)
  #:mode (:set I I I O)
  [-----
   (:set (v : any_τ any_Γ) v any_τ2 (v : any_τ2 any_Γ))]
  [(where #t (different v v_2))
   (:set any_Γ v_2 any_τ2 any_Γ2)
   -----
   (:set (v : any_τ any_Γ) v_2 any_τ2 (v : any_τ any_Γ2))]
  [-----
   (:set empty v any_τ (v : any_τ empty))])

(define-metafunction list-machine-typing
  dom : any -> (l ...)
  [(dom (l_1 : any_1 any_2))
   (l_1 l_2 ...)
   (where (l_2 ...) (dom any_2))]
  [(dom any) ()])

(define-metafunction list-machine-typing
  l-⊂ : (l ...) (l ...) -> any
  [(l-⊂ (l_1 ...) (l_2 ...))
   ,(let ([ht (make-hash)])
      (for ([l (in-list (term (l_2 ...)))])
        (hash-set! ht l #t))
      (for/and ([l (in-list (term (l_1 ...)))])
        (hash-ref ht l #f)))])
