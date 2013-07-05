#lang racket
(require "bitmap-test-util.rkt"
         redex/pict redex/reduction-semantics
         pict)

;; tests: 
;;  - language,
;;  - multi-line non-terminals, single-line non-terminals
(define-language lang
  (e (e e) 
     x
     (λ (x) e)
     number)
  (v number (λ (x) e))
  ((x y) variable-not-otherwise-mentioned))

(btest (render-language lang) "language.png")

(btest (render-language lang #:nts '(e v)) "language-nox.png")

(define-extended-language lang++ lang
  (e .... number (+ e e))
  (v .... number))

(btest (render-language lang++) "extended-language.png")

(define red
  (reduction-relation
   lang
   (--> ((λ (x) e) v) (S x v e))))

;; tests: reduction-relation
(btest (render-reduction-relation red)
       "reduction-relation.png")

(btest (render-reduction-relation 
        (extend-reduction-relation red lang (--> 1 2)))
       "extended-reduction-relation.png")

(btest (render-reduction-relation
        (with-unquote-rewriter
         (let ([once? #f])
           (λ (l)
             (if once?
                 l
                 (begin0
                   (struct-copy lw
                                l
                                [e "a: any"]
                                [unq? #f])
                   (set! once? #t)))))
         (reduction-relation
          lang
          (--> (a any) 1 "a" (computed-name (format "a: ~a" (term any))))
          (--> (b any) 2 "b" (computed-name (format "b: ~a" (term any))))
          (--> (c any) 3 (computed-name (format "c: ~a" (term any)))))))
       "reduction-relation-with-computed-labels.png")

(let ([R (reduction-relation
          lang
          (--> 1 1 "a")
          (--> 2 2 "b" (computed-name "a"))
          (--> 3 3 (computed-name "c")))])
  (btest (parameterize ([render-reduction-relation-rules (remq 'b (reduction-relation->rule-names R))])
           (render-reduction-relation R))
         "reduction-relation-with-computed-labels-and-hiding.png"))

;; this test should fail because it gets the order wrong
;; for the where/side-conditions
(define red2
  (reduction-relation
   lang
   (--> (number_a number_b number_c number_d)
        any_z
        (where (any_x any_y) (number_a number_b))
        (side-condition (= (term number_c) 5))
        (where any_z any_x)
        (side-condition (= (term number_d) 5)))))

(btest (render-reduction-relation red2)
       "red2.png")

(let ()
  (define-judgment-form lang
    #:mode (id I O)
    [(id e e)])
  (btest (render-reduction-relation
          (reduction-relation
           lang
           (--> e_1
                q
                (where (name q e_2) e_1)
                (judgment-holds (id e_2 (name r e_3))))))
         "red-with-where-name.png"))

(define-metafunction lang
  [(S x v e) e])

(btest (render-metafunction S)
       "metafunction.png")

(let ()
  (define-metafunction lang
    [(f (e_1 e_2))
     (e_3 e_4)
     (judgment-holds (J e_1 e_3))
     (judgment-holds (J e_2 e_4))])
  (define-judgment-form lang
    #:mode (J I O)
    [(J e e)])
  (btest (render-metafunction f)
         "metafunction-judgment-holds.png"))

(define-metafunction lang
  [(T x y)
   1
   (side-condition (not (eq? (term x) (term y))))
   (clause-name first-one)]
  [(T x x) 
   (any_1 any_2)
   (where any_1 2)
   (where any_2 2)])

;; in this test, the metafunction has 2 clauses 
;; with a side-condition on the first clause
;; and a 'where' in the second clause
(btest (parameterize ([metafunction-cases '("first-one" 1)])
         (render-metafunction T))
       "metafunction-T.png")

;; in this test, the `x' is italic and the 'z' is sf, since 'x' is in the grammar, and 'z' is not.
(btest (render-lw 
        lang 
        (to-lw ((λ (x) (x x))
                (λ (z) (z z)))))
       "lw.png")

(define-metafunction lang
  [(TL 1) (a
           ,(term-let ((x (term 1)))
                      (term x))
           below-only)]
  [(TL 2) (a
           ,(term-let ((x (term 1)))
                      (term x)) beside
                      below)])

;; this tests that term-let is sucked away properly
;; when the metafunction is rendered
(btest (render-metafunction TL) "metafunction-TL.png")

(define-metafunction lang
  [(Name (name x-arg arg)) 
   ,(term-let ((x-term-let (term 1)))
              (term (x-where x-term-let)))
   (where x-where 2)]
  [(Name number) short])

;; this tests that the three variable bindings
;; (x-arg, x-term-let, and x-where) 
;; all show up in the output.
(btest (render-metafunction Name) "metafunction-Name.png")

;; same as previous, but with vertical organization of the bindings
(btest (parameterize ([metafunction-pict-style 'up-down/vertical-side-conditions])
         (render-metafunction Name))
       "metafunction-Name-vertical.png")

;; compact turns out to be the same, since a line break is needed before
;; each side-condition clause:
(btest (parameterize ([metafunction-pict-style 'up-down/compact-side-conditions])
         (render-metafunction Name))
       "metafunction-Name-vertical.png")

;; in horizontal mode:
(btest (vl-append
        (clip
         (parameterize ([metafunction-pict-style 'left-right])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/vertical-side-conditions])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/compact-side-conditions])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/compact-side-conditions]
                        [linebreaks '(#t #f)])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/compact-side-conditions]
                        [linebreaks '(#f #t)])
           (render-metafunction Name)))
        (clip
         (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions]
                        [linebreaks '(#t #f)])
           (render-metafunction Name))))
       "metafunction-Name-horizontal.png")

;; makes sure that there is no overlap inside or across metafunction calls  
;; or when there are unquotes involved
(define-metafunction lang
  [(multi-arg a
              b
              c)
   ((multi-arg a 
               b 
               c)
    (multi-arg a 
               b 
               c))]
  [(multi-arg unquote-test)
   (,@(term (multi-arg with-unquote))
    ,@(term (multi-arg with-unquote))
    ,@(term (multi-arg with-unquote)))])

(btest (render-metafunction multi-arg) "metafunction-multi-arg.png")

;; makes sure that the LHS and RHS of metafunctions are appropriately
;; rewritten

(define-metafunction lang
  subst : e x e -> e
  [(subst x x e) e]
  [(subst number x e) number]
  [(subst x_1 x_2 e) x_1]
  [(subst (e_1 e_2) x e)
   ((subst e_1 x e) (subst e_2 x e))]
  [(subst (λ (x) e_b) x e)
   (λ (x) e)]
  [(subst (λ (x_f) e_f) x_a e_a)
   (λ (x_f) (subst e_f x_a e_a))])

(define (subst-rw lws)
  (list ""
        (list-ref lws 2)
        "{"
        (list-ref lws 3)
        ":="
        (list-ref lws 4)
        "}"))

(btest (with-atomic-rewriter
        'number "number" ;; this rewriter has no effect; here to test that path in the code
        (with-compound-rewriter
         'subst subst-rw
         (render-metafunction subst)))
       "metafunction-subst.png")


;; make sure two metafunctions simultaneously rewritten line up properly
(btest (render-metafunctions S T TL) "metafunctions-multiple.png")

;; make sure that the ellipses don't have commas before them.
(define-metafunction lang
  rdups : x ... -> (x ...)
  [(rdups x_1 x_2 ... x_1 x_3 ...)
   (rdups x_2 ... x_1 x_3 ...)]
  [(rdups x_1 x_2 ...)
   (x_1 x_3 ...)
   (where (x_3 ...) (rdups x_2 ...))]
  [(rdups) ()])

(btest (render-metafunction rdups) "rdups-delimited.png")
(parameterize ([delimit-ellipsis-arguments? #f])
  (btest (render-metafunction rdups) "rdups-undelimited.png"))

;; Non-terminal superscripts
(btest (render-lw lang (to-lw (x_^abcdef x_q^abcdef)))
       "superscripts.png")

;; `variable-not-in' in `where' RHS rendered as `fresh'
(define-metafunction lang
  [(f (name n 1)) 
   (x x_1 x_2 x_3)
   (where x ,(variable-not-in 'y 'x))
   (where (x_1 x_2) ,(variables-not-in 'z '(x1 x2)))
   (where x_3 (variables-not-in 'z '(x1 x2)))])
(btest (render-metafunction f) "var-not-in.png")
(let ([variable-not-in list])
  (define-metafunction lang
    [(g 1) 
     x
     (where x ,(variable-not-in 'y 'x))])
  (btest (render-metafunction g) "var-not-in-rebound.png"))

;; hidden `where' and `side-condition' clauses
(define-metafunction lang
  [(mf-hidden 1)
   2
   (where/hidden number 7)
   (side-condition/hidden (= 1 2))])
(btest (render-metafunction mf-hidden) "mf-hidden.png")
(btest (render-reduction-relation
        (reduction-relation
         lang
         (--> 1
              2
              (where/hidden number 7)
              (side-condition/hidden (= 1 2)))))
       "rr-hidden.png")

;; holes
(let ()
  (define-language L
    (n (hole x) ; a "named hole" at one time
       hole))
  (btest (render-language L) "holes.png"))

(let ()
  ;; the 'has no lambdas' relation (useful because it has a case with no premises)
  (define-relation lang
    [(r e_1 e_2) (r e_1) (r e_2)]
    [(r x)])
  (btest (render-relation r) "relation.png"))

(let ()
  ;; a relation with a `name' pattern in its conclusion
  (define-relation lang
    [(r (name e (λ (x) x)))
     (r x)])
  (btest (render-relation r) "relation-with-name.png"))    

;; judgment form
(let ()
  (define-language nats
    (n z (s n)))
  
  (define-judgment-form nats
    #:mode (sum I I O)
    [----------- "sumz"
     (sum z n n)]
    [(sum n_1 n_2 n_3)
     ------------------------- "sums"
     (sum (s n_1) n_2 (s n_3))])
  
  (define-judgment-form nats
    #:mode (mfw I O)
    [(mfw n_1 n_2)
     (where n_2 (f n_1))])
  
  (define-metafunction nats
    [(f n) n])
  
  (define-judgment-form nats
    #:mode (nps I O)
    [(nps (name a (s n_1)) n_2)
     (nps z (name n_1 (s (s n_1))))
     (where (name b n_2) z)])
  
  (define-judgment-form nats
    #:mode (lt2 I)
    [(lt2 z)]
    [(lt2 (s z))])
  
  (define-judgment-form nats
    #:mode (uses-ellipses I)
    [(uses-ellipses (n ...))
     (lt2 n) ... (sum z z z)])
  
  (btest (vc-append 
          10
          (render-judgment-form sum)
          
          (with-compound-rewriter
           'sum
           (λ (lws) (list "" (list-ref lws 2) " + " (list-ref lws 3) " = " (list-ref lws 4)))
           (render-judgment-form sum))
          
          (render-judgment-form mfw)
          
          (render-judgment-form nps)
          
          (render-judgment-form uses-ellipses))
         
         "judgment-form-examples.png"))

(let ()
  (define-language STLC
    (e (λ (x : τ) e)
       (e e)
       x)
    (x variable-not-otherwise-mentioned)
    ((τ σ) b
           (τ → τ))
    (Γ ([x τ] ...)))
  
  (define-judgment-form STLC
    #:mode (typeof I I O)
    #:contract (typeof Γ e τ)
    [(typeof Γ (e_1 e_2) τ)
     (typeof Γ e_1 (τ_2 → τ)) (typeof Γ e_2 τ_2)]
    [(typeof Γ (λ (x : τ) e) (τ → σ))
     (typeof (extend Γ x τ) e σ)]
    [(typeof Γ x τ)
     (where τ (lookup Γ x))])
  
  (define-metafunction STLC
    extend : Γ x τ -> Γ
    [(extend ([x_1 τ_1] ...) x_0 τ_0)
     ([x_0 τ_0] [x_1 τ_1] ...)])
  
  (define-metafunction STLC
    lookup : Γ x -> τ
    [(lookup ([x_0 τ_0] ... [x_i τ_i] [x_i+1 τ_i+1] ...)) τ_i])
  
  (define (rewrite-typeof lws)
    (list "" (list-ref lws 2) " ⊢ " (list-ref lws 3) " : " (list-ref lws 4)))
  
  (define (rewrite-extend lws)
    (list "" (list-ref lws 2) ", " (list-ref lws 3) ":" (list-ref lws 4)))
  
  (define (rewrite-lookup lws)
    (list "" (list-ref lws 2) "(" (list-ref lws 3) ")"))
  
  (btest (with-compound-rewriters
          (['typeof rewrite-typeof]
           ['extend rewrite-extend]
           ['lookup rewrite-lookup])
          (render-judgment-form typeof))
         "stlc.png"))  

(printf "bitmap-test.rkt: ")
(done)
