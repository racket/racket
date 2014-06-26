#lang racket/base

(require redex/reduction-semantics
         (only-in redex/private/generate-term pick-an-index)
         racket/match
         racket/list
         racket/contract
         racket/bool racket/set
         (only-in redex/examples/stlc-tests-lib consistent-with?))

(provide (all-defined-out))

(define-language stlc
  (M N ::= 
     (λ x M)
     (M N)
     x
     c
     (let ([x M]) N))
  (Γ (x σ Γ)
     ·)
  (σ τ ::=
     int
     (σ → τ)
     (list τ)
     (ref τ)
     x)
  (c d ::= c0 c1)
  (c0 ::= + integer)
  (c1 ::= cons nil hd tl new get set)
  (x y r ::= variable-not-otherwise-mentioned)
  
  (v (λ x M)
     c
     ((cons v) v)
     (cons v)
     (+ v)
     (set v)
     r)
  (E hole
     (E M)
     (v E)
     (let ([x E]) M))
  (Σ ::= · (r v Σ))
  
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
(define M? (redex-match? stlc M))
(define Σ-M? (redex-match? stlc (Σ M)))

#|

The typing judgment has no rule with multiple 
(self-referential) premises. Instead, it explicitly
manipulates a continuation so that it can, when it
discovers a type equality, simply do the substitution
through the continuation. This also makes it possible
to easily generate fresh variables by picking ones
that aren't in the expression or the continuation.

The 'tc-down' rules recur thru the term to find a type
of the left-most subexpression and the 'tc-up' rules
bring that type back, recurring on the continuation.

|#

(define-judgment-form stlc
  #:mode (typeof I O)
  #:contract (typeof M σ)
  
  [(tc-down · M · σ)
   -------------
   (typeof M σ)])

(define-judgment-form stlc
  #:mode (tc-down I I I O)
  #:contract (tc-down Γ M κ σ)
  
  [(tc-up (const-type0 c0) κ σ_ans)
   ------------------------------
   (tc-down Γ c0 κ σ_ans)]
  
  [(where x ,(variable-not-in (term (Γ κ)) 'γ))
   (tc-up (const-type1 x c1) κ σ_ans)
   ------------------------------
   (tc-down Γ c1 κ σ_ans)]
  
  [(where τ (lookup-Γ Γ x))
   (tc-up τ κ σ_ans)
   ---------------------------
   (tc-down Γ x κ σ_ans)]
  
  [(where y ,(variable-not-in (term (x Γ M κ)) 'α2-))
   (tc-down (x y Γ) M (λ y κ) σ_ans)
   ------------------------------------------------
   (tc-down Γ (λ x M) κ σ_ans)]
  
  [(tc-down Γ M_1 (1 Γ M_2 κ) σ_2)
   --------------------------
   (tc-down Γ (M_1 M_2) κ σ_2)]
  
  [(where N_2 (subst N x v))
   (where y ,(variable-not-in (term N_2) 'l))
   (tc-down Γ ((λ y N_2) v) κ σ_2)
   ------------------------------------------
   (tc-down Γ (let ([x v]) N) κ σ_2)]
  
  [(where #t (not-v? M))
   (tc-down Γ ((λ x N) M) κ σ_2)
   ---------------------------------
   (tc-down Γ (let ([x M]) N) κ σ_2)])

(define-judgment-form stlc
  #:mode (tc-up I I O)
  #:contract (tc-up τ κ σ)
  
  [---------------------
   (tc-up σ_ans · σ_ans)]
  
  [(tc-down Γ M (2 τ κ) σ_ans)
   ---------------------------
   (tc-up τ (1 Γ M κ) σ_ans)]
  
  [(where x ,(variable-not-in (term (τ_1 τ_2 κ)) 'α1-))
   (where G (unify τ_2 (τ_1 → x)))
   (tc-up (apply-subst-τ G x)
          (apply-subst-κ G κ)
          σ_ans)
   ---------------------------------------------------
   (tc-up τ_1 (2 τ_2 κ) σ_ans)]
  
  [(tc-up (τ_1 → τ_2) κ σ_ans)
   ---------------------------
   (tc-up τ_2 (λ τ_1 κ) σ_ans)])

(define-metafunction stlc
  const-type0 : c0 -> σ
  [(const-type0 +) (int → (int → int))]
  [(const-type0 integer) int])

(define-metafunction stlc
  const-type1 : x c1 -> σ
  [(const-type1 x nil) (list x)]
  [(const-type1 x cons) (x → ((list x) → (list x)))]
  [(const-type1 x hd) ((list x) → x)]
  [(const-type1 x tl) ((list x) → (list x))]
  [(const-type1 x new) (x → (ref x))]
  [(const-type1 x get) ((ref x) → x)]
  [(const-type1 x set) ((ref x) → (x → x))])

(define-metafunction stlc
  lookup-Γ : Γ x -> σ or #f
  [(lookup-Γ (x σ Γ) x) σ]
  [(lookup-Γ (x σ Γ) y) (lookup-Γ Γ y)]
  [(lookup-Γ · x) #f])

(define-metafunction stlc
  unify : τ τ -> Gx or ⊥
  [(unify τ σ) (uh (τ σ ·) ·)])

#|

Algorithm copied from Chapter 8 in _Handbook of Automated Reasoning_:
Unification Theory by Franz Baader and Wayne Synder
http://www.cs.bu.edu/~snyder/publications/UnifChapter.pdf

The 'uh' function iterates over a set of equations applying the
rules from the paper, building up the result substitution in G_r.

|#

(define-metafunction stlc
  uh : G Gx -> Gx or ⊥
  
  [(uh · Gx) Gx]
  
  ;; orient
  [(uh (τ x G) Gx) (uh (x τ G) Gx) (where #t (not-var? τ))]
  
  ;; trivial (other cases are covered by decomposition rule)
  [(uh (x x G) Gx) (uh G Gx)]
  
  ;; decomposition
  [(uh ((τ_1 → τ_2) (σ_1 → σ_2) G) Gx) (uh (τ_1 σ_1 (τ_2 σ_2 G)) Gx)]
  [(uh ((list τ)    (list σ)    G) Gx) (uh (τ σ G)               Gx)]
  [(uh ((ref τ)     (ref σ)     G) Gx) (uh (τ σ G)               Gx)]
  [(uh (int         int         G) Gx) (uh G                     Gx)]
  
  ;; symbol clash
  [(uh (τ σ G) Gx) ⊥ (where #t (not-var? τ)) (where #t (not-var? σ))]
  
  ;; occurs check
  [(uh (x τ G) Gx) ⊥ (where #t (in-vars-τ? x τ))]
  
  ;; variable elimination
  [(uh (x τ G) Gx)
   (uh (eliminate-G x τ G) (x τ (eliminate-G x τ Gx)))])

(define-metafunction stlc
  eliminate-G : x τ G -> G
  [(eliminate-G x τ ·) ·]
  [(eliminate-G x τ (σ_1 σ_2 G))
   ((eliminate-τ x τ σ_1) (eliminate-τ x τ σ_2) (eliminate-G x τ G))])

(define-metafunction stlc
  eliminate-τ : x τ σ -> σ
  [(eliminate-τ x τ (σ_1 → σ_2)) ((eliminate-τ x τ σ_1) → (eliminate-τ x τ σ_2))]
  [(eliminate-τ x τ (list σ)) (list (eliminate-τ x τ σ))]
  [(eliminate-τ x τ (ref σ)) (ref (eliminate-τ x τ σ))]
  [(eliminate-τ x τ int) int]
  [(eliminate-τ x τ x) τ]
  [(eliminate-τ x τ y) y])

(define-metafunction stlc
  ∨ : boolean boolean -> boolean
  [(∨ #f #f) #f]
  [(∨ boolean_1 boolean_2) #t])

(define-metafunction stlc
  not-var? : τ -> boolean
  [(not-var? x) #f]
  [(not-var? τ) #t])

(define-metafunction stlc
  in-vars-τ? : x τ -> boolean
  [(in-vars-τ? x (τ_1 → τ_2)) (∨ (in-vars-τ? x τ_1) (in-vars-τ? x τ_2))]
  [(in-vars-τ? x (list τ)) (in-vars-τ? x τ)]
  [(in-vars-τ? x (ref τ)) (in-vars-τ? x τ)]
  [(in-vars-τ? x int) #f]
  [(in-vars-τ? x x) #t]
  [(in-vars-τ? x y) #f])

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
  [(apply-subst-Γ Gx (y σ Γ)) (y (apply-subst-τ Gx σ) (apply-subst-Γ Gx Γ))]
  [(apply-subst-Γ Gx ·) ·])

(define-metafunction stlc
  not-v? : M -> boolean
  [(not-v? v) #f]
  [(not-v? M) #t])

#|

The reduction relation rewrites a pair of
a store and an expression to a new store
and a new expression or into the string
 "error" (for hd and tl of the empty list)

|#

(define red
  (reduction-relation
   stlc
   (--> (Σ (in-hole E ((λ x M) v)))
        (Σ (in-hole E (subst M x v)))
        "βv")
   (--> (Σ (in-hole E (let ([x v]) M)))
        (Σ (in-hole E (subst M x v)))
        "let")
   (--> (Σ (in-hole E (hd ((cons v_1) v_2))))
        (Σ (in-hole E v_1))
        "hd")
   (--> (Σ (in-hole E (tl ((cons v_1) v_2))))
        (Σ (in-hole E v_2))
        "tl")
   (--> (Σ (in-hole E (hd nil)))
        "error"
        "hd-err")
   (--> (Σ (in-hole E (tl nil)))
        "error"
        "tl-err")
   (--> (Σ (in-hole E ((+ integer_1) integer_2)))
        (Σ (in-hole E ,(+ (term integer_1) (term integer_2))))
        "+")
   (--> (Σ (in-hole E (new v)))
        ((r v Σ) (in-hole E r))
        (fresh r)
        "ref")
   (--> (Σ (in-hole E ((set x) v)))
        ((update-Σ Σ x v) (in-hole E (lookup-Σ Σ x)))
        "set")
   (--> (Σ (in-hole E (get x)))
        (Σ (in-hole E (lookup-Σ Σ x)))
        "get")))

#|

Capture avoiding substitution

|#


(define-metafunction stlc
  subst : M x M -> M
  [(subst x x M_x)
   M_x]
  [(subst (λ x M) x M_x)
   (λ x M)]
  [(subst (let ([x M]) N) x M_x)
   (let ([x (subst M x M_x)]) N)]
  [(subst (λ y M) x M_x)
   (λ x_new (subst (replace M y x_new) x M_x))
   (where x_new ,(variable-not-in (term (x y M))
                                  (term y)))]
  [(subst (let ([y N]) M) x M_x)
   (let ([x_new (subst N x M_x)]) (subst (replace M y x_new) x M_x))
   (where x_new ,(variable-not-in (term (x y M))
                                  (term y)))]
  [(subst (M N) x M_x)
   ((subst M x M_x) (subst N x M_x))]
  [(subst M x M_z)
   M])

(define-metafunction stlc
  [(replace x x x_new)
   x_new]
  [(replace (λ x_0 M) x x_new)
   (λ (replace x_0 x x_new) (replace M x x_new))]
  [(replace (let ([x_0 M]) N) x x_new)
   (let ([(replace x_0 x x_new)
          (replace M x x_new)])
     (replace N x x_new))]
  [(replace (M N) x x_new)
   ((replace M x x_new) (replace N x x_new))]
  [(replace M x x_new)
   M])

#;
(define-metafunction stlc
  [(replace (any_1 ...) x_1 x_new)
   ((replace any_1 x_1 x_new) ...)]
  [(replace x_1 x_1 x_new)
   x_new]
  [(replace any_1 x_1 x_new)
   any_1])

(define-metafunction stlc
  lookup-Σ : Σ x -> v
  [(lookup-Σ (x v Σ) x) v]
  [(lookup-Σ (x v Σ) y) (lookup-Σ Σ y)])

(define-metafunction stlc
  update-Σ : Σ x v -> Σ
  [(update-Σ (x v_1 Σ) x v_2) (x v_2 Σ)]
  [(update-Σ (x v_1 Σ) y v_2) (x v_1 (update-Σ Σ y v_2))])

#|

A top-level evaluator

|#

(define/contract (Eval M)
  (-> M? (or/c "error" 'list 'λ 'ref number?))
  (define M-t (type-check M))
  (unless M-t
    (error 'Eval "doesn't typecheck: ~s" M))
  (define res (apply-reduction-relation* red (term (· ,M))))
  (unless (= 1 (length res))
    (error 'Eval "internal error: not exactly 1 result ~s => ~s" M res))
  (define ans (car res))
  (match (car res)
    ["error" "error"]
    [`(,Σ ,N)
     (define ans-t (type-check N Σ))
     (unless (equal? M-t ans-t)
       (error 'Eval "internal error: type soundness fails for ~s" M))
     (match N
       [(? x?) (cond
                 [(term (in-dom? ,Σ ,N))
                  'ref]
                 [else
                  (error 'Eval "internal error: reduced to a free variable ~s"
                         M)])]
       [(or `((cons ,_) ,_) `nil) 'list]
       [(or `(λ ,_ ,_) `(cons ,_) `(set ,_) `(+ ,_)) 'λ]
       [(? number?) N]
       [_ (error 'Eval "internal error: didn't reduce to a value ~s" M)])]))

#|

A type checker; the optional argument is a store to use
for type checking free variables in M.

|#

(define/contract (type-check M [Σ (term ·)])
  (->* (M?) (any/c) (or/c τ? #f))
  (define M-ts (judgment-holds (typeof ,(Σ+M->M Σ M) τ) τ))
  (cond
    [(null? M-ts)
     #f]
    [(null? (cdr M-ts))
     (car M-ts)]
    [else
     (error 'type-check "non-unique type: ~s : ~s" M M-ts)]))

;; building an expression out of a store can be done in this model
;; with just topological sort because there are no recursive types,
;; so the store will not contain any cycles
(define (Σ+M->M Σ M)
  ;; nodes : edges[r -o> v]
  (define nodes (make-hash))
  (define edges (make-hash))
  (let loop ([Σ Σ])
    (match Σ
      [`· (void)]
      [`(,r ,v ,Σ) 
       (hash-set! nodes r v)
       (loop Σ)]))
  
  (for ([(n rhs) (in-hash nodes)]) (hash-set! edges n (set)))
  (for ([(n-src rhs) (in-hash nodes)])
    (for ([(n-dest _) (in-hash nodes)])
      (when (mentions-node? n-dest rhs)
        (hash-set! edges n-src (set-add (hash-ref edges n-src) n-dest)))))
  (define rev-sorted (reverse-topo-sort (for/list ([(k v) (in-hash nodes)]) k)
                                        edges))
  (let loop ([sorted rev-sorted])
    (cond
      [(empty? sorted) M]
      [else
       (define r (car sorted))
       (term (let ([,r (new ,(hash-ref nodes r))])
               ,(loop (cdr sorted))))])))

(define (mentions-node? v r)
  (let loop ([v v])
    (cond
      [(symbol? v) (equal? r v)]
      [(pair? v) (or (loop (car v)) (loop (cdr v)))]
      [else #f])))

#|

The first algorithm from this page:
http://en.wikipedia.org/wiki/Topological_sorting#Algorithms

|#
(define/contract (reverse-topo-sort nodes edges)
  (-> (listof any/c) (hash/c any/c (set/c any/c)) (listof any/c))
  
  (for ([node (in-list nodes)])
    (unless (hash-ref edges node #f)
      (error 'topo-sort "no edge entry for ~s" node)))
  
  (define incoming-counts (build-incoming-counts nodes edges))
  (define (remove-edge src dest)
    (hash-set! edges src (set-remove (hash-ref edges src) dest))
    (hash-set! incoming-counts dest (- (hash-ref incoming-counts dest) 1)))
  
  (define l '())
  (define s (for/set ([(n c) (in-hash incoming-counts)]
                      #:when (zero? c))
              n))
  (let loop ()
    (unless (set-empty? s)
      (define n (set-first s))
      (set! s (set-remove s n))
      (set! l (cons n l))
      (for ([m (in-set (hash-ref edges n))])
        (remove-edge n m)
        (when (zero? (hash-ref incoming-counts m))
          (set! s (set-add s m))))
      (loop)))
  
  l)

(define (build-incoming-counts nodes edges)
  (define incoming-counts (make-hash))
  (for ([n (in-list nodes)]) (hash-set! incoming-counts n 0))
  (for ([(n neighbors) (in-hash edges)])
    (for ([neighbor (in-set neighbors)])
      (hash-set! incoming-counts neighbor (+ (hash-ref incoming-counts neighbor) 1))))
  incoming-counts)

(module+ test
  
  (test-equal (build-incoming-counts '(x y z) 
                                     (make-hash (list (cons 'x (set 'y 'z))
                                                      (cons 'y (set 'z))
                                                      (cons 'z (set)))))
              (make-hash (list (cons 'x 0)
                               (cons 'y 1)
                               (cons 'z 2))))
  (test-equal (reverse-topo-sort '() (make-hash)) '())
  (test-equal (reverse-topo-sort '(x) 
                                 (make-hash (list (cons 'x (set)))))
              '(x))
  (test-equal (reverse-topo-sort '(x y) 
                                 (make-hash (list (cons 'y (set))
                                                  (cons 'x (set 'y)))))
              '(y x))
  (test-equal (reverse-topo-sort '(y x) 
                                 (make-hash (list (cons 'y (set))
                                                  (cons 'x (set 'y)))))
              '(y x))
  (test-equal (reverse-topo-sort '(x y z) 
                                 (make-hash (list (cons 'x (set 'y))
                                                  (cons 'y (set 'z))
                                                  (cons 'z (set)))))
              '(z y x))
  (test-equal (reverse-topo-sort '(x y z) 
                                 (make-hash (list (cons 'x (set 'y 'z))
                                                  (cons 'y (set 'z))
                                                  (cons 'z (set)))))
              '(z y x))
  (define (one-of? a . bs) (for/or ([b (in-list bs)]) (equal? a b)))
  (test-equal (one-of? (reverse-topo-sort '(x y z) 
                                          (make-hash (list (cons 'x (set 'z))
                                                           (cons 'y (set 'z))
                                                           (cons 'z (set)))))
                       '(z x y)
                       '(z y x))
              #t)
  
  (test-equal (Σ+M->M (term (r1 r2 (r2 1 ·))) (term 2))
              (term (let ([r2 (new 1)]) (let ([r1 (new r2)]) 2))))
  (test-equal (Σ+M->M (term (r2 1 (r1 r2 ·))) (term 2))
              (term (let ([r2 (new 1)]) (let ([r1 (new r2)]) 2))))
  
  (test-equal (term (subst ((+ 1) 1) x 2))
              (term ((+ 1) 1)))
  (test-equal (term (subst ((+ x) x) x 2))
              (term ((+ 2) 2)))
  (test-equal (term (subst ((+ y) x) x 2))
              (term ((+ y) 2)))
  (test-equal (term (subst ((+ y) z) x 2))
              (term ((+ y) z)))
  (test-equal (term (subst ((λ x x) x) x 2))
              (term ((λ x x) 2)))
  (test-equal (consistent-with? (term (subst ((λ y x) x) x 2))
                                (term ((λ y 2) 2)))
              #t)
  (test-equal (consistent-with? (term (subst ((λ y x) x) x (λ q z)))
                                (term ((λ y (λ q z)) (λ q z))))
              #t)
  (test-equal (consistent-with? (term (subst ((λ y x) x) x (λ q y)))
                                (term ((λ y2 (λ q y)) (λ q y))))
              #t)
  (test-equal (consistent-with? (term (subst (λ z (λ z1 z)) q 1))
                                (term (λ z (λ z1 z))))
              #t)
  
  (test-equal (consistent-with? (term (subst (let ([y x]) x) x 2))
                                (term (let ([y 2]) 2)))
              #t)
  (test-equal (consistent-with? (term (subst (let ([y x]) x) x (λ q z)))
                                (term (let ([y (λ q z)]) (λ q z))))
              #t)
  (test-equal (consistent-with? (term (subst (let ([y x]) x) x (λ q y)))
                                (term (let ([y (λ q y)]) (λ q y))))
              #t)
  (test-equal (consistent-with? (term (subst (let ([z 11]) (let ([z1 12]) z)) q 1))
                                (term (let ([z 11]) (let ([z1 12]) z))))
              #t)
  (test-equal (consistent-with? (term (subst (let ((|| +)) ||) |1| (λ x1 x1)))
                                (term (let ((|| +)) ||)))
              #t)
  
  (test-equal (term (unify x int))
              (term (x int ·)))
  (test-equal (term (unify int x))
              (term (x int ·)))
  (test-equal (term (unify int (list int)))
              (term ⊥))
  (test-equal (term (unify int int))
              (term ·))
  (test-equal (term (unify (list int) (list int)))
              (term ·))
  (test-equal (term (unify (int → x) (y → (list int))))
              (term (x (list int) (y int ·))))
  (test-equal (term (unify (int → x) (x → (list int))))
              (term ⊥))
  (test-equal (term (unify (x   → (y          → x))
                           (int → ((list int) → y))))
              (term ⊥))
  (test-equal (term (unify (x   → (y          → x))
                           (int → ((list int) → z))))
              (term (z int (y (list int) (x int ·)))))
  (test-equal (term (unify (x   → (y          → z))
                           (int → ((list int) → x))))
              (term (z int (y (list int) (x int ·)))))
  (test-equal (term (unify (x   → (y   → z))
                           (y   → (z   → int))))
              (term (z int (y int (x int ·)))))
  (test-equal (term (unify x (x → y)))
              (term ⊥))
  
  (test-equal (judgment-holds (typeof 5 τ) τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof nil τ) τ)
              (list (term (list γ))))
  (test-equal (judgment-holds (typeof (cons 1) τ) τ)
              (list (term ((list int) → (list int)))))
  (test-equal (judgment-holds (typeof ((cons 1) nil) τ) τ)
              (list (term (list int))))
  (test-equal (consistent-with? (judgment-holds (typeof (λ x x) τ) τ)
                                (list (term (α → α))))
              #t)
  (test-equal (consistent-with? (judgment-holds (typeof (λ x (λ y x)) τ) τ)
                                (list (term (α → (α1 → α)))))
              #t)
  (test-equal (consistent-with? (judgment-holds (typeof (λ f (λ x (f ((+ x) 1)))) τ) τ)
                                (list (term ((int → α) → (int → α)))))
              #t)
  (test-equal (judgment-holds (typeof (λ f (λ x ((+ (f ((+ x) 1))) 2))) τ) τ)
              (list (term ((int → int) → (int → int)))))
  (test-equal (judgment-holds (typeof (λ f (λ x ((+ x) (f 1)))) τ) τ)
              (list (term ((int → int) → (int → int)))))
  (test-equal (judgment-holds (typeof (λ x (x x)) τ) τ)
              (list))
  (test-equal (judgment-holds (typeof ((+ ((+ 1) 2)) ((+ 3) 4)) τ) τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof ((cons ((cons 1) nil)) nil) τ) τ)
              (list (term (list (list int)))))
  (test-equal (judgment-holds (typeof ((cons nil) nil) τ) τ)
              (list (term (list (list γ1)))))
  (test-equal (judgment-holds (typeof ((set (new 1)) 2) τ) τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof (get (new 1)) τ) τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof (let ([id (λ y y)])
                                        ((id id) 1))
                                      τ)
                              τ)
              (list (term int)))
  (test-equal (judgment-holds (typeof (let ([r (new nil)])
                                        (let ([n ((set r) ((cons 5) nil))])
                                          ((hd (get r)) 1)))
                                      τ)
                              τ)
              (list))
  
  (test-->> red (term (· ((λ x x) 7))) (term (· 7)))
  (test-->> red (term (· (((λ x (λ x x)) 2) 1))) (term (· 1)))
  (test-->> red (term (· (((λ x (λ y x)) 2) 1))) (term (· 2)))
  (test-->> red 
            (term (· ((λ x ((cons x) nil)) 11)))
            (term (· ((cons 11) nil))))
  (test-->> red 
            (term (· ((λ x ((cons x) nil)) 11)))
            (term (· ((cons 11) nil))))
  (test-->> red 
            (term (· ((cons ((λ x x) 11)) nil)))
            (term (· ((cons 11) nil))))
  (test-->> red
            (term (· (cons ((λ x x) 1))))
            (term (· (cons 1))))
  (test-->> red
            (term (· ((cons ((λ x x) 1)) nil)))
            (term (· ((cons 1) nil))))
  (test-->> red
            (term (· (hd ((λ x ((cons x) nil)) 11))))
            (term (· 11)))
  (test-->> red
            (term (· (tl ((λ x ((cons x) nil)) 11))))
            (term (· nil)))
  (test-->> red
            (term (· (tl nil)))
            "error")
  (test-->> red
            (term (· (hd nil)))
            "error")
  (test-->> red
            (term (· ((+ 1) (hd nil))))
            "error")
  (test-->> red
            (term (· ((+ ((+ 1) 2)) ((+ 3) 4))))
            (term (· 10)))
  (test-->> red
            (term (· ((λ f (f 3)) (cons 1))))
            (term (· ((cons 1) 3))))
  (test-->> red
            (term (· ((λ f (f 3)) (+ 1))))
            (term (· 4)))
  (test-->> red
            (term (· (let ([f (+ 2)]) ((+ (f 3)) (f 4)))))
            (term (· 11)))
  (test-->> red
            (term (· (get (new 1))))
            (term ((r 1 ·) 1)))
  (test-->> red
            (term (· ((set (new 1)) 2)))
            (term ((r 2 ·) 1)))
  (test-->> red
            #:equiv consistent-with?
            (term (· (let ([r (new 1)])
                       (let ([o ((set r) 2)])
                         (get r)))))
            (term ((r 2 ·) 2)))
  (test-->> red
            #:equiv consistent-with?
            (term (· (let ([r (new nil)])
                       (let ([n ((set r) ((cons 5) nil))])
                         ((hd (get r)) 1)))))
            (term ((r ((cons 5) nil) ·) (5 1))))
  
  (test-equal (Eval (term ((λ x x) 3)))
              3)
  (test-equal (Eval (term ((cons 1) nil)))
              'list)
  (test-equal (Eval (term (cons 1)))
              'λ)
  (test-equal (Eval (term (λ x x)))
              'λ)
  (test-equal (type-check (term 5))
              (term int))
  (test-equal (type-check (term (5 5)))
              #f)
  (test-equal (type-check (term r1) (term (r1 r (r 1 ·))))
              (term (ref (ref int))))
  (test-equal (type-check (term r1) (term (r 1 (r1 r ·))))
              (term (ref (ref int)))))
