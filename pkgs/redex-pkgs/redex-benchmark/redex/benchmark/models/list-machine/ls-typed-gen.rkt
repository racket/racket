#lang racket

(define the-error "no error")

(require redex/reduction-semantics
         racket/list
         racket/match)

(provide (all-defined-out))

(define-language list-machine
  (a nil
     (cons a a))
  (v variable-not-otherwise-mentioned)
  (r empty
     (r v ↦ a))
  (l variable-not-otherwise-mentioned)
  (ι (jump l)
     (branch-if-nil v l)
     (fetch-field v 0 v)
     (fetch-field v 1 v)
     (cons v v v)
     halt
     (begin ι ι))
  (p (l : ι p)
     end))

(define-judgment-form list-machine
  #:contract (var-lookup r v a)
  #:mode (var-lookup I I O)
  [-----
   (var-lookup (r v ↦ a) v a)]
  [(where #t (different v_1 v_2))
   (var-lookup r v_2 a_2)
   -----
   (var-lookup (r v_1 ↦ a_1) v_2 a_2)])

(define-judgment-form list-machine
  #:contract (var-set r v a r)
  #:mode (var-set I I I O)
  [-----
   (var-set (r v ↦ a) v a_2 (r v ↦ a_2))]
  [(where #t (different v v_2))
   (var-set r v_2 a_2 r_2)
   -----
   (var-set (r v ↦ a) v_2 a_2 (r_2 v ↦ a))]
  [-----
   (var-set empty v a (empty v ↦ a))])

(define-judgment-form list-machine
  #:contract (program-lookup p l ι)
  #:mode (program-lookup I I O)
  [-----
   (program-lookup (l : ι p) l ι)]
  [(where #t (different l l_2))
   (program-lookup p l_2 ι_2)
   -----
   (program-lookup (l : ι p) l_2 ι_2)])

(define red 
  (reduction-relation 
   list-machine
   #:domain (p r ι)
   (--> (p r (begin (begin ι_1 ι_2) ι_3))
        (p r (begin ι_1 (begin ι_2 ι_3)))
        "step-seq")
   (--> (p r (begin (fetch-field v 0 v_2) ι))
        (p r_2 ι)
        "step-fetch-field-0"
        (judgment-holds (var-lookup r v (cons a_0 a_1)))
        (judgment-holds (var-set r v_2 a_0 r_2)))
   (--> (p r (begin (fetch-field v 1 v_2) ι))
        (p r_2 ι)
        "step-fetch-field-1"
        (judgment-holds (var-lookup r v (cons a_0 a_1)))
        (judgment-holds (var-set r v_2 a_1 r_2)))
   (--> (p r (begin (cons v_0 v_1 v_2) ι))
        (p r_2 ι)
        "step-cons"
        (judgment-holds (var-lookup r v_0 a_0))
        (judgment-holds (var-lookup r v_1 a_1))
        (judgment-holds (var-set r v_2 (cons a_0 a_1) r_2)))
   (--> (p r (begin (branch-if-nil v l) ι))
        (p r ι)
        "step-branch-not-taken"
        (judgment-holds (var-lookup r v (cons a_0 a_1))))
   (--> (p r (begin (branch-if-nil v l) ι))
        (p r ι_2)
        "step-branch-taken"
        (judgment-holds (var-lookup r v nil))
        (judgment-holds (program-lookup p l ι_2)))
   (--> (p r (jump l))
        (p r ι_2)
        "step-jump"
        (judgment-holds (program-lookup p l ι_2)))))

(define (run-prog p)
  (define r_0 (term (empty v0 ↦ nil)))
  (define ι_0 (car (judgment-holds (program-lookup ,p l0 ι) ι)))
  (apply-reduction-relation* red `(,p ,r_0 ,ι_0)))


(define (check-progress p)
  (define r_0 (term (empty v0 ↦ nil)))
  (define ι_0 (car (judgment-holds (program-lookup ,p l0 ι) ι)))
  (or (equal? ι_0 'halt)
      (and
       (= 1
          (length (apply-reduction-relation 
                   red
                   `(,p ,r_0 ,ι_0))))
       (let ([closure (apply-reduction-relation* 
                       red
                       `(,p ,r_0 ,ι_0)
                       #:stop-when
                       (let ([count 0])
                         (λ (_)
                           (begin0
                             (count . > . 1000)
                             (set! count (add1 count))))))])
         ;; if reduction terminates in less than 1000 steps, check it ends with halt
         ;; (if the #:stop-when condition is true, we get back an empty list, 
         ;; and the same thing for a reduction cycle)
         (or (empty? closure)
             (and (= 1 (length closure))
                  (match (car closure)
                    [`(,p ,r ,ι)
                     (equal? ι 'halt)])))))))

(define (check p)
  (or (not p)
      (check-progress p)))
  
(define-metafunction list-machine
  different : any any -> any
  [(different any_1 any_1)
   #f]
  [(different any_1 any_2)
   #t])

(define-extended-language list-machine-typing list-machine
  (τ nil (list τ) (listcons τ))
  (Γ empty (v : τ Γ))
  (Π empty (l : Γ Π)))

(define-judgment-form list-machine-typing
  #:contract (check-program p Π)
  #:mode (check-program I I)
  [(:lookup-Π Π l0 (v0 : nil empty))
   (labels-distinct (l0 : ι p))
   ;; note : changed from l-⊂
   (d= Π (l0 : ι p))
   (labels-distinct-Π Π)
   (check-blocks Π (l0 : ι p))
   -----
   (check-program (l0 : ι p) Π)])

(define-judgment-form list-machine-typing
  #:contract (Γ-⊂ Γ Γ)
  #:mode (Γ-⊂ I I)
  [-----
   (Γ-⊂ Γ empty)]
  [(:lookup-Γ Γ_1 v τ_1) 
   (⊂ τ_1 τ_2)
   (Γ-⊂ Γ_1 Γ_2)
   ----
   (Γ-⊂ Γ_1 (v : τ_2 Γ_2))])

(define-judgment-form list-machine-typing
  #:contract (check-blocks Π p)
  #:mode (check-blocks I I)
  [(:lookup-Π Π l Γ)
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
  [(:lookup-Π Π l Γ_2)
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
  [(:lookup-Γ Γ v (list τ))
   (:lookup-Π Π l Γ_1)
   (:set Γ v nil Γ_3)
   (Γ-⊂ Γ_3 Γ_1)
   (:set Γ_3 v (listcons τ) Γ_2)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ_2)]
  [(:lookup-Γ Γ v (listcons τ))
   (:lookup-Π Π l Γ_1)
   (:set Γ v nil Γ_2)
   (Γ-⊂ Γ_2 Γ_1)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ)]
  [(:lookup-Γ Γ v nil)
   (:lookup-Π Π l Γ_1)
   (Γ-⊂ Γ Γ_1)
   -----
   (check-instr Π Γ (branch-if-nil v l) Γ)]
  [(:lookup-Γ Γ v (listcons τ)) (:set Γ v_2 τ Γ_2)
   -----
   (check-instr Π Γ (fetch-field v 0 v_2) Γ_2)]
  [(:lookup-Γ Γ v (listcons τ)) (:set Γ v_2 (list τ) Γ_2)
   -----
   (check-instr Π Γ (fetch-field v 1 v_2) Γ_2)]
  [(:lookup-Γ Γ v_0 τ_0) (:lookup-Γ Γ v_1 τ_1)
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
  #:contract (:lookup-Γ Γ v τ)
  #:mode (:lookup-Γ I I O)
  [-----
   (:lookup-Γ (v : τ Γ) v τ)]
  [(where #t (different v_1 v_2))
   (:lookup-Γ Γ v_2 τ_2)
   -----
   (:lookup-Γ (v_1 : τ_1 Γ) v_2 τ_2)])

(define-judgment-form list-machine-typing
  #:contract (:lookup-Π Π l Γ)
  #:mode (:lookup-Π I I O)
  [-----
   (:lookup-Π (l : Γ Π) l Γ)]
  [(where #t (different l_1 l_2))
   (:lookup-Π Π l_2 Γ_2)
   -----
   (:lookup-Π (l_1 : Γ_1 Π) l_2 Γ_2)])

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
  [(dom (l_1 : any_1 any_2))
   (l_1 (dom any_2))]
  [(dom empty) empty])

(define-metafunction list-machine-typing
  [(dom-P (l_1 : ι_1 p))
   (l_1 (dom p))]
  [(dom-P end) empty])

(define-metafunction list-machine-typing
  [(dom-Π (l_1 : Γ_1 Π))
   (l_1 (dom Π))]
  [(dom-Π empty) empty])

#;
(define-metafunction list-machine-typing
  l-⊂ : (l ...) (l ...) -> any
  [(l-⊂ (l_1 ...) (l_2 ...))
   ,(let ([ht (make-hash)])
      (for ([l (in-list (term (l_2 ...)))])
        (hash-set! ht l #t))
      (for/and ([l (in-list (term (l_1 ...)))])
        (hash-ref ht l #f)))])

(define-relation list-machine-typing
  [(l-⊂ (l_1 empty) l_2)
   (where #t (lmem l_1 l_2))]
  [(l-⊂ (l_1 l_2) l_3)
   (l-⊂ l_2 l_3)
   (where #t (lmem l_1 l_3))]
  [(l-⊂ empty any)])

(define-relation list-machine-typing
  [(d= (l_1 : Γ_1 Π) (l_1 : ι p))
   (d= Π p)]
  [(d= empty end)])

(define-relation list-machine-typing
  [(d-⊂ (l_1 : Γ_1 Π) p)
   (has-label p l_1)
   (d-⊂ Π p)]
  [(d-⊂ (l_1 : Γ_1 Π) (l_2 : ι p))
   (d-⊂ (l_1 : Γ_1 Π) p)]
  [(d-⊂ empty p)])

(define-relation list-machine-typing
  [(has-label (l_1 : ι p) l_1)]
  [(has-label (l_1 : ι p) l_2)
   (has-label p l_2)])

(define-relation list-machine-typing
  [(labels-distinct (l_1 : ι p))
   (label-not-in l_1 p)
   (labels-distinct p)]
  [(labels-distinct end)])

(define-relation list-machine-typing
  [(label-not-in l_1 (l_2 : ι p))
   (different l_1 l_2)
   (label-not-in l_1 p)]
  [(label-not-in l_1 end)])

(define-metafunction list-machine-typing
  [(lmem l_1 (l_1 l_2))
   #t]
  [(lmem l_1 (l_2 l_3))
   (lmem l_1 l_3)]
  [(lmem l_1 empty)
   #f])

(define-relation list-machine-typing
  [(labels-distinct-Π (l_1 : Γ Π))
   (label-not-in-Π l_1 Π)
   (labels-distinct-Π Π)]
  [(labels-distinct-Π empty)])

(define-relation list-machine-typing
  [(label-not-in-Π l_1 (l_2 : Γ Π))
   (different l_1 l_2)
   (label-not-in-Π l_1 Π)]
  [(label-not-in-Π l_1 empty)])

(define (generate-M-term)
  (generate-term list-machine-typing (l0 : ι p) 7))

(define (type-check p)
  ;; need to provide a program typing, so generate 10 randomly and
  ;; see if any succeed...
  (let loop ([i 0])
    (cond
      [(i . > . 10) #f]
      [else
       (define guess-Π (generate-term list-machine-typing (l0 : (v0 : nil empty) Π) 7))
       (or (judgment-holds (check-program ,p ,guess-Π))
           (loop (add1 i)))])))

(define (typed-generator)
  (let ([g (redex-generator list-machine-typing 
                            (check-program p Π)
                            7)])
    (λ () 
      (match (g)
        [`(check-program ,p ,Π)
         p]
        [#f #f]))))

(define (generate-typed-term)
  (match (generate-term list-machine-typing
                        #:satisfying
                        (check-program p Π)
                        7)
    [`(check-program ,p ,Π)
     p]
    [#f #f]))