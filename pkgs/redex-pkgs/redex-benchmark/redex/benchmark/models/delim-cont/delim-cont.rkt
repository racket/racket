#lang racket

(require redex/reduction-semantics
         (only-in redex/private/generate-term pick-an-index))

(provide (all-defined-out))

(define-language abort-core-lang

  ;; programs
  (P (<> e σ))

  ;; store
  (σ · (key σ) (tag σ))

  ;; expressions
  (e x v (e e) (if e e e) (μ (x : t) e)
     (unop e) (binop e e) (cons e e)
     (case e (null = e) ((cons x x) = e))
     (make-prompt-tag t t) (make-cm-key t)
     (% e e v) (abort t e e) ; again with the type
     (wcm w e) (ccm e t)
     (call/comp e e) (call/cm e e e)
     (seq (update mk e) e) ; for doing wcm updates -- should track label
     (error))

  (v b (λ (x : t) e) pt mk
     (cons v v) (null t)
     call/comp call/cm)

  (w ((mk v) w)
     ·)
  
  (W ((mk v) W)
     hole)

  (t B (→ t t) (Prompt t t)
     (Mark t) (List t))
  (B Num Bool Unit)

  ;(x variable-not-otherwise-mentioned)
  (x (variable-prefix var:))

  (b n s bool unit)
  (n number)
  (bool #t #f)

  ;; using variables for allocated tags/keys
  (pt tag)
  (mk key)
  (tag (variable-prefix tag))
  (key (variable-prefix key))

  (binop + - *)
  (unop - not zero? number? unit? boolean?)

  ;; still need this wcm non-adjacency here
  (E M (wcm w M))
  (M hole (if E e e) (E e) (v E)
     (unop E) (binop E e) (binop v E)
     (case E (null = e) ((cons x x) = e))
     (cons E e) (cons v E)
     (seq (update mk E) e)
     (% e E v) (% E pt v)
     (abort t E e) (abort t v E)
     (ccm E t)
     (call/comp E e) (call/comp v E)
     (call/cm E e e) (call/cm v E e)))

;; define this in two stages for paper typesetting
(define-extended-language abort-lang abort-core-lang
  (e ....
     (monitor ctc e l l l)
     (ctc-error l l)
     (check e v l l))
  (pt ....
      (PG ctc ctc v l l l))
  (mk ....
      (MG ctc mk l l l))
  (ctc (flat (λ (x : t) e))
       (-> ctc ctc)
       (prompt-tag/c ctc ctc t t)
       (mark/c ctc t)
       (list/c ctc))
  (t ....
     (Con t))
  ;; blame labels
  ((l k j) string)
  (M ....
     (monitor ctc E l l l)
     (check E v l l)))


(define-extended-language abort+Γ-lang abort-lang
  (Γ · (x : t Γ))
  (Σ · (v : t Σ))
  
  (Pt (<> e Σ)))


(define abort-red
  (reduction-relation abort+Γ-lang
   ;; domain changed to any for the test suite so we catch errors
   ;; with type soundness, not a contract exception
   #:domain any #:arrow ==>

   ;; primitives
   (--> (binop v_1 v_2) (delta_bin binop v_1 v_2) binop)
   (--> (unop v) (delta_un unop v) unop)

   ;; lists
   (--> (case (cons v_1 v_2)
          (null = e_1)
          ((cons x_1 x_2) = e_2))
        (subst (subst e_2 x_1 v_1) x_2 v_2)
        case/cons)
   (--> (case (null t)
          (null = e_1)
          ((cons x_1 x_2) = e_2))
        e_1
        case/null)

   ;; conditional
   (--> (if #t e_1 e_2) e_1 if-true)
   (--> (if #f e_1 e_2) e_2 if-false)

   ;; lambda application
   (--> ((λ (x : t) e) v)
        (subst e x v)
        β)

   ;; recursion
   (--> (μ (x : t) e)
        (subst e x (μ (x : t) e))
        μ)

   ;; call/comp
   (--> (% (in-hole E_pt (wcm w (call/comp v pt)))
           pt_1 v_h)
        (% (in-hole E_pt (wcm w (v (λ (x : t) e_k))))
           pt_1 v_h)
        (where/hidden (λ (x : t) e) v)
        (where e_k (wrap-c- pt (wrap-c+ pt_1 (in-hole E_pt x))))
        (side-condition/hidden (term (no-match E_pt pt)))
        (side-condition (term (same-prompt-tag? pt pt_1)))
        call/comp)

   ;; prompt & prompt tags
   ;; E[(make-prompt-tag)]  |-->  E[x]  where x \not\in FV(E)
   (==> (<> (in-hole E (make-prompt-tag t_1 t_2)) Σ)
        (<> (in-hole E tag) (tag : (Prompt t_1 t_2) Σ))
        (where/hidden tag ,(variable-not-in (term Σ) 'tag))
        (side-condition (variable-not-in (term Σ) 'tag))
        prompt-tag)

   ;; cont mark key
   (==> (<> (in-hole E (make-cm-key t)) Σ)
        (<> (in-hole E key) (key : (Mark t) Σ))
        (where/hidden key ,(variable-not-in (term Σ) 'key))
        (side-condition (variable-not-in (term Σ) 'key))
        mark-key)

   ;; no control effect
   (--> (% v_1 pt v_2) v_1
        prompt)

   ;; abort (everything in one!)
   (--> (% (in-hole E_pt (abort t pt v)) pt_1 v_h)
        (v_h (in-hole E_+ (in-hole E_- v)))
        (where E_+ (wrap+ pt_1))
        (where E_- (wrap- pt hole))
        (side-condition/hidden (term (no-match E_pt pt)))
        (side-condition (term (same-prompt-tag? pt pt_1)))
        abort)

   ;; flat contracts
   ;; MF: your flat syntax allows only a value i.e. (flat v)
   ;; which is good !!!
   ;; and this suggests that you meant 'monitor' not 'contract' here
   ;; 
   ;; Rule #14 (had to remove names for typesetting)
   (--> (monitor (flat v_f) v k l j)
        (check (v_f v) v k j))

   (--> (check #t v l j) v)
   (--> (check #f v l j) (ctc-error l j))

   ;; ->
   (--> (monitor (-> ctc_a ctc_r) (name v (λ (x : t) e)) k l j)
        (λ (x_1 : t)
           ((λ (x_2 : t) (monitor ctc_r (v x_2) k l j))
            (monitor ctc_a x_1 l k j)))
        ;;added this (10/7/14):
        (where/hidden (x_1 x_2) 
                      ,(variables-not-in (term (ctc_a ctc_r v))
                                         '(var: var:))))
   ;; prompt-tag/c
   (--> (monitor (prompt-tag/c ctc_1 ctc_2 t_1 t_2) v_p k l j)
        (PG ctc_1 ctc_2 v_p k l j))

   ;; mark/c
   (--> (monitor (mark/c ctc t) v_m k l j)
        (MG ctc v_m k l j))

   ;; list/c
   (--> (monitor (list/c ctc) (null t) k l j)
        (null t))
   (--> (monitor (list/c ctc) (cons v_1 v_2) k l j)
        (cons (monitor ctc v_1 k l j) (monitor (list/c ctc) v_2 k l j)))

   ;; just a value
   (--> (wcm w v) v wcm/v)

   ;; wcm merging
   (--> (wcm w_1 (wcm w_2 e))
        (wcm (merge w_1 w_2) e)
        wcm/merge)

   ;; get a mark
   (==> (<> (in-hole E (ccm key t)) Σ)
        (<> (in-hole E (marks E key (null t))) Σ)
        ccm)

   ;; get mark values with a guarded key
   (--> (ccm (MG ctc mk k l j) t)
        (monitor (list/c ctc) (ccm mk t) k l j)
        ccm/guard)

   ;; turn a call/cm into an update
   (--> (wcm w (call/cm mk v e))
        (wcm w (seq (update mk_1 e_1) e))
        (where (mk_1 e_1) (push mk v))
        call/cm)

   ;; special mark update
   (--> (wcm (in-hole W ((key v_1) w))
             (seq (update key v_2) e))
        (wcm (in-hole W ((key v_2) w))
             e)
        wcm/set)

   ;; add a mark
   (--> (wcm (in-hole W ·)
             (seq (update key v) e))
        (wcm (in-hole W ((key v) ·))
             e)
        (side-condition (term (not-in-W key W)))
        wcm/add)

   ;; introduce mark environment
   (==> (<> (in-hole E (call/cm v_1 v_2 e)) Σ)
        (<> (in-hole E (wcm · (call/cm v_1 v_2 e))) Σ)
        (side-condition (term (not-wcm E)))
        wcm/intro/cm)

   (==> (<> (in-hole E (call/comp v_1 v_2)) Σ)
        (<> (in-hole E (wcm · (call/comp v_1 v_2))) Σ)
        (side-condition (term (not-wcm E)))
        wcm/intro/comp)

   ;; propagate error
   (==> (<> (in-hole E (ctc-error l j)) Σ)
        (<> (ctc-error l j) Σ)
        (side-condition/hidden (not (equal? (term E) (term hole))))
        ctc-error)

   (==> (<> (in-hole E (error)) Σ)
        (<> (error) Σ)
        (side-condition (not (equal? (term E) (term hole)))))

   with
   [(==> (<> (in-hole E a) Σ) (<> (in-hole E b) Σ))
    (--> a b)]))

;; wrapping contracts
(define-metafunction abort-lang
  wrap+ : pt -> E
  [(wrap+ (PG ctc_1 ctc_2 pt k l j))
   (monitor ctc_1 (wrap+ pt) k l j)]
  [(wrap+ tag) hole])

(define-metafunction abort-lang
  wrap- : pt E -> E
  [(wrap- (PG ctc_1 ctc_2 pt k l j) E)
   (wrap- pt (monitor ctc_1 E l k j))]
  [(wrap- tag E) E])

(define-metafunction abort-lang
  wrap-c+ : pt e -> e
  [(wrap-c+ (PG ctc_1 ctc_2 pt k l j) e)
   (monitor ctc_2 (wrap-c+ pt e) l k j) ]
  [(wrap-c+ tag e) e])

(define-metafunction abort-lang
  wrap-c- : pt e -> e
  [(wrap-c- (PG ctc_1 ctc_2 pt k l j) e)
   (wrap-c- pt (monitor ctc_2 e k l j))]
  [(wrap-c- tag e) e])

;; extract true prompt tag out
(define-metafunction abort-lang
  extract : pt -> pt
  [(extract (PG ctc_1 ctc_2 pt k l j)) (extract pt)]
  [(extract tag) tag])

;; push value into mark guards
(define-metafunction abort-lang
  push : mk e -> (mk e)
  [(push (MG ctc mk k l j) e)
   (push mk (monitor ctc e l k j))]
  [(push key e) (key e)])

;; check if this mark key is in the current w
(define-metafunction abort-lang
  not-in-W : mk W -> #t or #f
  [(not-in-W mk_1 ((mk_1 v_1) W))
   #f]
  [(not-in-W mk_1 ((mk_2 v_1) W))
   (not-in-W mk_1 W)]
  [(not-in-W mk_1 hole)
   #t])

;; check that wcm doesn't wrap the hole
(define-metafunction abort-lang
  not-wcm : E -> #t or #f
  [(not-wcm (in-hole E (wcm w hole))) #f]
  [(not-wcm E) #t])

;; wcm merging, second takes precedence
(define-metafunction abort-lang
  merge : w w -> w
  [(merge · w)
   w]
  [(merge ((v_1 v_2) w_1)
          (in-hole W ((v_1 v_3) w_2)))
   (merge w_1
          (in-hole W ((v_1 v_3) w_2)))]
  [(merge ((v_1 v_2) w_1)
          (in-hole W ·))
   (merge w_1
          (in-hole W ((v_1 v_2) ·)))])


;; just for convenience
(define-metafunction abort-lang
  [(let ([(x : t) e_1]) e_2) ((λ (x : t) e_2) e_1)])

;; type checking
(define-judgment-form
  abort+Γ-lang
  #:mode (tc I I I O)
  #:contract (tc Γ Σ any t)

  [(tc Γ Σ e_1 (→ t_2 t_3))
   (tc Γ Σ e_2 t_2)
   ------------------------ TApp
   (tc Γ Σ (e_1 e_2) t_3)]

  [(tc (x : t_1 Γ) Σ e t_2)
   ------------------------------------ TLam
   (tc Γ Σ (λ (x : t_1) e) (→ t_1 t_2))]

  [(tc (x : t Γ) Σ e t)
   ------------------------ TMu
   (tc Γ Σ (μ (x : t) e) t)]

  [(tc Γ Σ e_1 (List t_1))
   (tc Γ Σ e_2 t_2)
   (tc (x_2 : (List t_1) (x_1 : t_1 Γ)) Σ e_3 t_2)
   ----------------------------------------------------------- TCase
   (tc Γ Σ (case e_1 (null = e_2) ((cons x_1 x_2) = e_3)) t_2)]

  [-------------------------- TNull
   (tc Γ Σ (null t) (List t))]

  [(tc Γ Σ v_1 t)
   (tc Γ Σ v_2 (List t))
   -------------------------------- TCons
   (tc Γ Σ (cons v_1 v_2) (List t))]

  [(where t (∈-Γ x Γ))
   ----------------- TVar
   (tc Γ Σ x t)]

  [(tc Γ Σ e_1 Num)
   (tc Γ Σ e_2 Num)
   ------------------------------------ TBinOp
   (tc Γ Σ (binop e_1 e_2) Num)]
  
  [(tc Γ Σ e Num)
   ---------------------------------- TUnOp-
   (tc Γ Σ (- e) Num)]
  
  [(tc Γ Σ e Bool)
   ---------------------------------- TUnOp-not
   (tc Γ Σ (not e) Bool)]
  
  [(tc Γ Σ e Num)
   ---------------------------------- TUnOp-zero?
   (tc Γ Σ (zero? e) Bool)]
  
  [(tc Γ Σ e t)
   ---------------------------------- TUnOp-number?
   (tc Γ Σ (number? e) Bool)]
  
  [(tc Γ Σ e t)
   ---------------------------------- TUnOp-unit?
   (tc Γ Σ (unit? e) Bool)]
  
  [(tc Γ Σ e t)
   ---------------------------------- TUnOp-boolean?
   (tc Γ Σ (boolean? e) Bool)]

  [-------------- TNum
   (tc Γ Σ n Num)]

  [---------------- TTrue
   (tc Γ Σ #t Bool)]

  [---------------- TFalse
   (tc Γ Σ #f Bool)]

  [------------------ TUnit
   (tc Γ Σ unit Unit)]

  [(tc Γ Σ e_1 Bool)
   (tc Γ Σ e_2 t)
   (tc Γ Σ e_3 t)
   --------------------------- TIf
   (tc Γ Σ (if e_1 e_2 e_3) t)]

  ;; Control
  [--------------------------------------------------- TMakePrompt
   (tc Γ Σ (make-prompt-tag t_1 t_2) (Prompt t_1 t_2))]

  [(where (Prompt t_1 t_2) (∈-Σ tag Σ))
   ---------------------------------- TPromptTag
   (tc Γ Σ tag (Prompt t_1 t_2))]

  ;; ???
  ;; should this be:
  ;; (this version satisfies type soundness)
  [(tc Γ Σ v (Prompt t_1 t_2))
   (tcc Γ Σ ctc_1 (Con t_1))
   (tcc Γ Σ ctc_2 (Con t_2))
   -------------------------------------------------- TPromptGuard
   (tc Γ Σ (PG ctc_1 ctc_2 v k l j) (Prompt t_1 t_2))]
  
#| previous version:
  [(tc Γ Σ v (Prompt t_1 t_2))
   (tcc Γ Σ ctc_1 (Con (Prompt t_1 t_2)))
   -------------------------------------------------- TPromptGuard
   (tc Γ Σ (PG ctc_1 ctc_2 v k l j) (Prompt t_1 t_2))]|#
  
  [(tc Γ Σ e_1 t_2)
   (tc Γ Σ v (→ t_1 t_2))
   (tc Γ Σ e_2 (Prompt t_1 t_2))
   ----------------------------- TPrompt
   (tc Γ Σ (% e_1 e_2 v) t_2)]

  [(tc Γ Σ e_2 t_1)
   (tc Γ Σ e_1 (Prompt t_1 t_2))
   ----------------------------- TAbort
   (tc Γ Σ (abort t e_1 e_2) t)]

  [(tc Γ Σ e_1 (→ (→ t_3 t_2) t_3))
   (tc Γ Σ e_2 (Prompt t_1 t_2))
   -------------------------------- TCallComp
   (tc Γ Σ (call/comp e_1 e_2) t_3)]

  ;; Marks
  [(tc Γ Σ e_1 (Mark t_1))
   (tc Γ Σ e_2 t_1) (tc Γ Σ e_3 t_2)
   ---------------------------------- TCallCM
   (tc Γ Σ (call/cm e_1 e_2 e_3) t_2)]
  
  [(tc Γ Σ e t)
   (tc-w Γ Σ w)
   --------------------------------------------- TWCM
   (tc Γ Σ (wcm w e) t)]

  [(tc Γ Σ e (Mark t))
   --------------------------- TCCM
   (tc Γ Σ (ccm e t) (List t))]

  [--------------------------------- TMakeKey
   (tc Γ Σ (make-cm-key t) (Mark t))]

  [(where (Mark t) (∈-Σ key Σ))
   -------------------------- TKey
   (tc Γ Σ key (Mark t))]  

  [(tcc Γ Σ ctc (Con t))
   (tc Γ Σ v (Mark t))
   ---------------------------------- TKeyGuard
   (tc Γ Σ (MG ctc v k l j) (Mark t))]

  [(tcc Γ Σ ctc (Con t)) (tc Γ Σ e t)
   --------------------------------- TMon
   (tc Γ Σ (monitor ctc e k l j) t)])

;; pulled out to tighten the tc contract
(define-judgment-form abort+Γ-lang
  #:mode (tcc I I I O)
  [(tc Γ Σ (λ (x : B) e) (→ B Bool))
   ---------------------------------------- TConFlat
   (tcc Γ Σ (flat (λ (x : B) e)) (Con B))]

  [(tcc Γ Σ ctc_1 (Con t_1)) (tcc Γ Σ ctc_2 (Con t_2))
   ------------------------------------------------- TConFun
   (tcc Γ Σ (-> ctc_1 ctc_2) (Con (→ t_1 t_2)))]

  [(tcc Γ Σ ctc_1 (Con t_1)) (tcc Γ Σ ctc_2 (Con t_2))
   ----------------------------------------------------------------- TConPrompt
   (tcc Γ Σ (prompt-tag/c ctc_1 ctc_2 t_1 t_2) (Con (Prompt t_1 t_2)))]

  [(tcc Γ Σ ctc (Con t))
   -------------------------------------- TConMark
   (tcc Γ Σ (mark/c ctc t) (Con (Mark t)))]

  [(tcc Γ Σ ctc (Con t))
   ------------------------------------ TConList
   (tcc Γ Σ (list/c ctc) (Con (List t)))])

(define-judgment-form abort+Γ-lang
  #:mode (tc-w I I I)
  [(tc Γ Σ v t)
   (tc Γ Σ mk (Mark t))
   (tc-w Γ Σ w)
   --------------------------------- TCW-cons
   (tc-w Γ Σ ((mk v) w))]
  [--------------------------------- TCW-nil
   (tc-w Γ Σ ·)]) 

(define-metafunction abort+Γ-lang
  ∈-Γ : any any -> t or #f
  [(∈-Γ x (x : t Γ)) t]
  [(∈-Γ x_2 (x_1 : t_1 Γ))
   (∈-Γ x_2 Γ)]
  [(∈-Γ any ·) #f])

(define-metafunction abort+Γ-lang
  ∈-Σ : any any -> t or #f
  [(∈-Σ v (v : t Σ)) t]
  [(∈-Σ v_2 (v_1 : t_1 Σ))
   (∈-Σ v_2 Σ)]
  [(∈-Σ any ·) #f])

(define-metafunction abort+Γ-lang
  [(different any_1 any_1) #f]
  [(different any_1 any_2) #t])


;; encoding for the paper
(define-metafunction abort-lang
  [(call/cc (name v (λ (x_1 : (→ t_1 t_2)) e_1)) e)
   (call/comp
    (λ (var:kont : (→ t_1 t_2))
      (v (λ (var:x : t_1)
           (abort t_2 e (λ (var:y : Unit) (var:kont var:x))))))
    e)])

(define-metafunction abort-lang
  subst-n : e (x ...) (e ...) -> e
  [(subst-n e () ()) e]
  [(subst-n e (x_1 x_2 ...) (e_1 e_2 ...))
   (subst-n (subst e x_1 e_1) (x_2 ...) (e_2 ...))])

(define-metafunction abort-lang
  subst : e x e -> e
  ;; base cases
  [(subst n x e) n]
  [(subst b x e) b]
  [(subst s x e) s]
  [(subst unit x e) unit]
  [(subst call/comp x e) call/comp]
  [(subst call/cm x e) call/cm]
  [(subst binop x e) binop]
  [(subst unop x e) unop]
  ;; expressions
  [(subst (binop e_1 e_2) x e_3)
   (binop (subst e_1 x e_3) (subst e_2 x e_3))]
  [(subst (unop e) x e_1)
   (unop (subst e x e_1))]
  [(subst (e_1 e_2 ...) x e_3)
   ((subst e_1 x e_3) (subst e_2 x e_3) ...)]
  [(subst (if e_1 e_2 e_3) x e_4)
   (if (subst e_1 x e_4) (subst e_2 x e_4) (subst e_3 x e_4))]
  ;; this might be simplifiable
  [(subst (case e_1 (null = e_2) ((cons x x_2) = e_3)) x e_4)
   (case (subst e_1 x e_4)
     (null = (subst e_2 x e_4))
     ((cons x x_2) = e_3))]
  [(subst (case e_1 (null = e_2) ((cons x_1 x) = e_3)) x e_4)
   (case (subst e_1 x e_4)
     (null = (subst e_2 x e_4))
     ((cons x_1 x) = e_3))]
  [(subst (case e_1 (null = e_2) ((cons x_1 x_2) = e_3)) x e_4)
   (case (subst e_1 x e_4)
     (null = (subst e_2 x e_4))
     ((cons x_1 x_2) = (subst e_3 x e_4)))]
  [(subst (cons e_1 e_2) x e)
   (cons (subst e_1 x e) (subst e_2 x e))]
  [(subst (% e_1 e_2 e_3) x e_4)
   (% (subst e_1 x e_4) (subst e_2 x e_4) (subst e_3 x e_4))]
  [(subst (abort t e_1 e_2) x e_4)
   (abort t (subst e_1 x e_4) (subst e_2 x e_4))]
  [(subst (make-prompt-tag t_1 t_2) x e)
   (make-prompt-tag t_1 t_2)]
  [(subst (call/comp v_1 e_1) x e_2)
   (call/comp (subst v_1 x e_2) (subst e_1 x e_2))]
  [(subst (ccm e t) x e_1) (ccm (subst e x e_1) t)]
  [(subst (wcm w e) x e_1) (wcm w (subst e x e_1))]
  [(subst (error) x e) (error)]
  [(subst (ctc-error l j) x e) (ctc-error l j)]
  ;; variables
  [(subst x x e) e]
  [(subst x_1 x_2 e) x_1]
  ;; prompt tags (this has to be after var cases)
  [(subst (PG ctc_1 ctc_2 pt k l j) x e)
   (PG (subst-ctc ctc_1 x e)
       (subst-ctc ctc_2 x e)
       (subst pt x e) k l j)]
  [(subst tag x e) tag]
  ;; mark keys
  [(subst (MG ctc mk k l j) x e)
   (MG (subst-ctc ctc x e) (subst mk x e) k l j)]
  [(subst key x e) key]
  ;; lambda
  [(subst (λ (x : t) e) x e_1)
   (λ (x : t) e)]
  [(subst (λ (x_1 : t_1) e) x_2 e_1)
   (λ (x_1 : t_1) (subst e x_2 e_1))]
  ;; mu
  [(subst (μ (x : t) e) x e_1)
   (μ (x : t) e)]
  [(subst (μ (x_1 : t_1) e) x_2 e_1)
   (μ (x_1 : t_1) (subst e x_2 e_1))]
  ;; contracts
  [(subst (monitor ctc e k l j) x e_1)
   (monitor (subst-ctc ctc x e_1) (subst e x e_1) k l j)]
  [(subst ctc x e_1)
   (subst-ctc ctc x e_1)]
  ;; additional ...
  [(subst (make-cm-key t) x e)
   (make-cm-key t)]
  [(subst (null t) x e)
   (null t)])

(define-metafunction abort-lang
  subst-ctc : ctc x e -> ctc
  [(subst-ctc (flat e) x e_1)
   (flat (subst e x e_1))]
  [(subst-ctc (-> ctc_a ctc_r) x e)
   (-> (subst-ctc ctc_a x e) (subst-ctc ctc_r x e))]
  [(subst-ctc (prompt-tag/c ctc_1 ctc_2 t_1 t_2) x e)
   (prompt-tag/c (subst-ctc ctc_1 x e)
                 (subst-ctc ctc_2 x e)
                 t_1 t_2)]
  [(subst-ctc (mark/c ctc t) x e)
   (mark/c (subst-ctc ctc x e) t)]
  [(subst-ctc (list/c ctc) x e)
   (list/c (subst-ctc ctc x e))])

;; MF: you do keep the rest of the language total. Cmp above with 'stuck'.
(define-metafunction abort-lang
  delta_bin : binop v v -> e
  [(delta_bin + n_1 n_2)
   ,(+ (term n_1) (term n_2))]
  [(delta_bin - n_1 n_2)
   ,(- (term n_1) (term n_2))]
  [(delta_bin * n_1 n_2)
   ,(* (term n_1) (term n_2))]
  [(delta_bin binop v_1 v_2) (error)])

(define-metafunction abort-lang
  delta_un : unop v -> e
  [(delta_un - n_1)
   ,(- (term n_1))]
  [(delta_un not v_1)
   ,(not (term v_1))]
  [(delta_un zero? n_1)
   ,(zero? (term n_1))]
  [(delta_un number? v_1)
   ,(number? (term v_1))]
  [(delta_un boolean? v_1)
   ,(boolean? (term v_1))]
  [(delta_un unit? v_1)
   ,(eq? 'unit (term v_1))]
  [(delta_un procedure? (λ ([x : t] ...) e)) #t]
  [(delta_un procedure? (flat v)) #t]
  [(delta_un procedure? v) #f]
  [(delta_un prompt-tag? pt) #t]
  [(delta_un prompt-tag? v) #f]
  [(delta_un unop v) (error)])

;; used to make sure a given context is the closest one with the
;; matching prompt tag
(define-metafunction abort-lang
  no-match : E pt -> #t or #f
  [(no-match hole pt) #t]
  [(no-match (% e E v) pt) (no-match E pt)]
  [(no-match (% E pt_0 v) pt_1)
   #f
   (side-condition (term (same-prompt-tag? pt_0 pt_1)))]
  [(no-match (% E pt_0 v) pt_1)
   (no-match E pt_1)
   (side-condition (not (term (same-prompt-tag? pt_0 pt_1))))]
  [(no-match (binop E e) pt) (no-match E pt)]
  [(no-match (binop v E) pt) (no-match E pt)]
  [(no-match (unop E) pt) (no-match E pt)]
  [(no-match (E e) pt) (no-match E pt)]
  [(no-match (v E) pt) (no-match E pt)]
  [(no-match (if E e_1 e_2) pt) (no-match E pt)]
  [(no-match (case E (null = e_1) ((cons x_1 x_2) = e_2)) pt)
   (no-match E pt)]
  [(no-match (abort t E e) pt) (no-match E pt)]
  [(no-match (abort t v E) pt) (no-match E pt)]
  [(no-match (wcm w E) pt) (no-match E pt)]
  [(no-match (monitor ctc E k l j) pt) (no-match E pt)]
  [(no-match (call/comp E e) pt) (no-match E pt)]
  [(no-match (call/comp v E) pt) (no-match E pt)]
  [(no-match (call/cm E e_1 e_2) pt) (no-match E pt)]
  [(no-match (call/cm v E e) pt) (no-match E pt)]
  [(no-match (ccm E t) pt) (no-match E pt)]
  [(no-match (check E v l_1 l_2) pt) (no-match E pt)])

(define-metafunction abort-lang
  same-prompt-tag? : pt pt -> #t or #f
  [(same-prompt-tag? pt pt) #t]
  [(same-prompt-tag? (PG ctc_1 ctc_2 pt_1 k l j) pt_2)
   (same-prompt-tag? pt_1 pt_2)]
  [(same-prompt-tag? pt_1 (PG ctc_1 ctc_2 pt_2 k l j))
   (same-prompt-tag? pt_1 pt_2)]
  [(same-prompt-tag? tag_1 tag_2) #f])


;; for continuation marks
(define-metafunction abort-lang
  marks : E v v -> v
  [(marks hole v v_1) v_1]
  [(marks (wcm w_1 E) v_1 v)
   (marks E v_1 (cons v_2 v))
   (where (in-hole W ((v_1 v_2) w_2)) w_1)]
  [(marks (wcm w E) v v_1)
   (marks E v v_1)
   (where (in-hole W ·) w)
   (side-condition/hidden (term (not-in-W v W)))]
  [(marks (E e_1) v v_1) (marks E v v_1)]
  [(marks (v_1 E) v_2 v_3) (marks E v_2 v_3)]
  [(marks (binop E e) v_2 v_1) (marks E v_2 v_1)]
  [(marks (binop v E) v_2 v_1) (marks E v_2 v_1)]
  [(marks (unop E) v_2 v_1) (marks E v_2 v_1)]
  [(marks (if E e_1 e_2) v_2 v_1) (marks E v_2 v_1)]
  [(marks (case E (null = e_1) ((cons x_1 x_2) = e_2)) v_2 v_1)
   (marks E v_2 v_1)]
  [(marks (% e E v) v_2 v_1) (marks E v_2 v_1)]
  [(marks (% E pt v) v_2 v_1) (marks E v_2 v_1)]
  [(marks (monitor ctc E k l j) v_2 v_1) (marks E v_2 v_1)]
  [(marks (abort t E e) v_2 v_1) (marks E v_2 v_1)]
  [(marks (abort t v E) v_2 v_1) (marks E v_2 v_1)]
  [(marks (call/comp E e) v_2 v_1) (marks E v_2 v_1)]
  [(marks (call/comp v E) v_2 v_1) (marks E v_2 v_1)]
  [(marks (call/cm E e_1 e_2) v_2 v_1) (marks E v_2 v_1)]
  [(marks (call/cm v_3 E e_2) v_2 v_1) (marks E v_2 v_1)]
  [(marks (ccm E t) v_1 v_2) (marks E v_1 v_2)]
  [(marks (check E v l_1 l_2) v_1 v_2) (marks E v_1 v_2)])

;; evaluator
;; term [store] -> result
(define (abort-eval t #:init-store [store (term ·)])
  (define state (term (<> ,t ,store)))
  (define steps 0)
  (define exceeded-max-steps #f)
  (define results 
    (apply-reduction-relation* abort-red state
                               #:stop-when
                               (λ (_)
                                 (set! steps (add1 steps))
                                 ;; treat 100 steps as non-terminating
                                 (and (steps . > . 100)
                                      (set! exceeded-max-steps #t)))))
  (cond [(or (empty? results)
             exceeded-max-steps)
         'non-terminating]
        [else
         (define result (cadr (car results)))
         (match result
           [(? number? n) n]
           [(? boolean? b) b]
           ['unit 'unit]
           [`(cons ,v_1 ,v_2) `(cons ,v_1 ,v_2)]
           [`(null ,t) 'null]
           ['key 'mark-key]
           [`(MG ,e ...) 'mark-key]
           ['tag 'prompt-tag]
           [`(PG ,e ...) 'prompt-tag]
           [`(λ ,e ...) 'procedure]
           [`(-> ,e_1 ,e_2) 'contract]
           [`(prompt-tag/c ,e ...) 'contract]
           [`(mark/c ,e ...) 'contract]
           ;; flat contracts are 'procedure even though they could be
           ;; 'contract (this works better since all unary procedures are
           ;;  contracts in Racket)
           [`(flat ,e) 'procedure]
           [`(ctc-error ,e_1 ,e_2) `(ctc-error ,e_1 ,e_2)]
           [`(error) 'error]
           [_
            (if (e-missing-prompt? result)
                'missing-prompt
                (error (format "Got stuck: ~a" result)))])]))

(define (e-missing-prompt? e)
  (or (redex-match abort-lang
                   (side-condition
                    (in-hole E_pt
                             (call/comp v pt))
                    (term (no-match E_pt pt)))
                   e)
      (redex-match abort-lang
                   (side-condition
                    (in-hole E_pt
                             (abort t pt v))
                    (term (no-match E_pt pt)))
                   e)))




(define (random-exp depth)
  (match
      (generate-term
       abort+Γ-lang
       #:satisfying
       (tc · Σ e t)
       depth)
    [#f #f]
    [`(tc · ,Σ ,e ,t) 
     (list Σ e)]))

(define (eval-random-exps n [depth 4])
  (for ([_ n])
    (define e (random-exp depth))
    (when e
      (pretty-write e)
      (abort-eval e))))

(define ccmg-e '(μ
 (Bxf : (List (Prompt Num Num)))
 (ccm
  (monitor
   (mark/c
    (prompt-tag/c (flat (λ (F : Num) #t)) (flat (λ (Q : Num) #t)) Num Num)
    (Prompt Num Num))
   (make-cm-key (Prompt Num Num))
   "LxkZYC"
   "r"
   "I")
  (Prompt Num Num))))

(define (progress-holds? e)
  (or (redex-match abort-lang v e)
      (e-missing-prompt? e)
      (= (length (apply-reduction-relation abort-red `(<> ,e ·)))
         1)))

(define (preservation-holds? e)
  (define t (judgment-holds (tc · · ,e t) t))
  (or (redex-match abort-lang v e)
      (e-missing-prompt? e)
      (match (apply-reduction-relation abort-red `(<> ,e ·))
        [`((<> ,e* ,st*))
         (equal? (judgment-holds (tc · ,st* ,e* t) t)
                 t)])))

(define (is-error? e)
  (or (redex-match abort-lang (in-hole E (ctc-error l_1 l_2)) e)
      (redex-match abort-lang (in-hole E (error)) e)))

;; artifacts of the reduction relation that can't
;; be typed checked
(define (in-between-state? exp)
  (or (redex-match abort-lang (in-hole E (seq (update mk e_1) e_2)) exp)
      (redex-match abort-lang (in-hole E (check e v l_1 l_2)) exp)))

(define (soundness-holds? e [inp-Σ '·])
  (define t (judgment-holds (tc · ,inp-Σ ,e t) t))
  (define exceeded-max-steps #f)
  (define steps 0)
  (or (not t)
      (match (apply-reduction-relation* 
              abort-red `(<> ,e ,inp-Σ)
              #:stop-when
              (λ (_)
                (set! steps (add1 steps))
                ;; treat 40 steps as non-terminating
                ;; larger examples tend to be useless...
                (and (steps . > . 40)
                     (set! exceeded-max-steps #t))))
        ['() #t] ;; looping reduction graph
        [`((<> ,e* ,st*))
         (or (is-error? e*)
             (and (or exceeded-max-steps
                      (redex-match abort-lang v e*)
                      (e-missing-prompt? e*))
                  (or (in-between-state? e*)
                      (equal? (judgment-holds (tc · ,st* ,e* t) t)
                              t))))]
        [_
         (if exceeded-max-steps
             #t
             (error 'soundness "multiple reductions found for ~s" e))])))

(define (check-random-exps n [depth 4] #:verbose [verbose? #f])
  (for/and ([_ n])
    (define e (random-exp depth))
    (or (not e)
        (match-let* ([(list s e) e]
                     [ok? (soundness-holds? e s)])
          (when verbose?
            (pretty-write e))
          (unless ok?
            (error 'check "soundness failed for: ~s\n~s" s e))
          ok?))))

(define (type-check e Σ)
  (judgment-holds (tc · ,Σ ,e t)))

