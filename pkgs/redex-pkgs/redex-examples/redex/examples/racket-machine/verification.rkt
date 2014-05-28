#lang racket

(require redex/reduction-semantics)
(require redex/examples/racket-machine/grammar 
         redex/examples/racket-machine/util)

(define (bytecode-ok? e)
  (not (eq? 'invalid (car (term (verify ,e () 0 #f () () ∅))))))

(define-extended-language verification 
  bytecode
  (s (ṽ ...) invalid)
  (ṽ uninit imm box imm-nc box-nc not)
  (γ ((n ṽ) ...))
  (η (n ...))
  (f (n n (ṽ ...)) ∅)
  (m n ?))

(define-metafunction verification
  verify : e s n b γ η f -> (s γ η)
  
  ; localrefs
  [(verify (loc n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l #f γ η f)
   ((ṽ_0 ... ṽ_n ṽ_n+1 ...) γ η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(imm imm-nc)))]
  [(verify (loc n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l #t γ η f)
   ((ṽ_0 ... ṽ_n ṽ_n+1 ...) γ η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(imm imm-nc box box-nc)))]
  [(verify (loc-box n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l b γ η f)
   ((ṽ_0 ... ṽ_n ṽ_n+1 ...) γ η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(box box-nc)))]
  
  [(verify (loc-noclr n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l #f γ η f) 
   ((ṽ_0 ... (nc ṽ_n) ṽ_n+1 ...) γ (log-noclr n n_l ṽ_n η))
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(imm imm-nc)))]
  [(verify (loc-noclr n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l #t γ η f) 
   ((ṽ_0 ... (nc ṽ_n) ṽ_n+1 ...) γ (log-noclr n n_l ṽ_n η))
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(imm imm-nc box box-nc)))]
  [(verify (loc-box-noclr n) (ṽ_0 ... ṽ_n ṽ_n+1 ...) n_l b γ η f) 
   ((ṽ_0 ... box-nc ṽ_n+1 ...) γ (log-noclr n n_l ṽ_n η))
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(box box-nc)))]
  
  [(verify (loc-clr n) (name s (ṽ_0 ... imm ṽ_n+1 ...)) n_l #f γ η f)
   ((ṽ_0 ... not ṽ_n+1 ...) (log-clr n s n_l γ) η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))]
  [(verify (loc-clr n) (name s (ṽ_0 ... ṽ_n ṽ_n+1 ...)) n_l #t γ η f)
   ((ṽ_0 ... not ṽ_n+1 ...) (log-clr n s n_l γ) η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (memq (term ṽ_n) '(imm box)))]
  [(verify (loc-box-clr n) (name s (ṽ_0 ... box ṽ_n+1 ...)) n_l b γ η f) 
   ((ṽ_0 ... not ṽ_n+1 ...) (log-clr n s n_l γ) η)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))]
  
  ; branch
  [(verify (branch e_c e_t e_e) s n_l b γ η f)
   ((redo-clrs γ_2 (trim s_3 s)) (concat γ_2 γ_3) η_3)
   (where (s_1 γ_1 η_1) (verify e_c s n_l #f γ η ∅))
   (where (s_2 γ_2 η_2) (verify e_t (trim s_1 s) 0 b () () f))
   (where (s_3 γ_3 η_3) (verify e_e (undo-noclrs η_2 (undo-clrs γ_2 (trim s_2 s))) 0 b γ_1 η_1 f))]
  
  ; let-one
  [(verify (let-one e_r e_b) (ṽ_1 ...) n_l b γ η f)
   (verify e_b (imm ṽ_1* ...) ,(+ (term n_l) (term 1)) b γ_1 η_1 
           (shift 1 f))
   (where s_0 (uninit ṽ_1 ...))
   (where (s_1 γ_1 η_1) (verify e_r s_0 ,(add1 (term n_l)) #f γ η ∅))
   (where (uninit ṽ_1* ...) (trim s_1 s_0))]
  
  ; seq
  [(verify (seq e_0 ... e_n) s n_l b γ η f)
   (verify e_n s_1 n_l b γ_1 η_1 f)
   (where (s_1 γ_1 η_1) (verify* (e_0 ...) s n_l #t γ η))]
  
  ; application
  [(verify (application (name e_0 (loc-noclr n)) e_1 ...) s n_l b_i γ η (n_f n_s (ṽ ...)))
   (verify-self-app (application e_0 e_1 ...) s n_l γ η (n_f n_s (ṽ ...)))
   (side-condition (= (term n) (+ (term n_f) (length (term (e_1 ...))))))]
  [(verify (application (lam (τ_0 ...) (n_0 ...) e) e_0 ...) (name s (ṽ_0 ...)) n_l b γ η f)
   (verify*-ref (e_0 ...) (τ_0 ...) s_1 n_l* γ η)
   (where n ,(length (term (e_0 ...))))
   (where n_l* ,(+ (term n) (term n_l)))
   (where s_1 (abs-push n not s))
   (side-condition (term (lam-verified? (lam (τ_0 ...) (n_0 ...) e) s_1 ?)))]
  [(verify (application (proc-const (τ_0 ...) e) e_0 ...) s n_l b γ η f)
   (verify (application (lam (τ_0 ...) () e) e_0 ...) s n_l b γ η f)]
  [(verify (application e_0 e_1 ...) (name s (ṽ_0 ...)) n_l b γ η f)
   (verify* (e_0 e_1 ...) (abs-push n not s) n_l* #f γ η)
   (where n ,(length (term (e_1 ...))))
   (where n_l* ,(+ (term n) (term n_l)))]
  
  ; let-void
  [(verify (let-void n e) (name s (ṽ_0 ...)) n_l b_i γ η f)
   (verify e (abs-push n uninit s) ,(+ (term n) (term n_l)) 
           b_i γ η (shift n f))]
  [(verify (let-void-box n e) (name s (ṽ_0 ...)) n_l b_i γ η f)
   (verify e (abs-push n box s) ,(+ (term n) (term n_l))
           b_i γ η (shift n f))]
  
  ; procedures in arbitrary context
  [(verify (lam ((name τ val) ...) (n_0 ...) e) s n_l b γ η f)
   (s γ η)
   (side-condition (term (lam-verified? (lam (τ ...) (n_0 ...) e) s ?)))]
  [(verify (proc-const ((name τ val) ...) e) s n_l b γ η f)
   (verify (lam (τ ...) () e) s n_l b γ η f)]
  
  ; case-lam
  [(verify (case-lam (name l (lam (val ...) (n ...) e)) ...) s n_l b γ η f)
   (s γ η)
   (side-condition (term (AND (lam-verified? l s ?) ...)))]
  
  ; literals
  [(verify number s n_l b γ η f) (s γ η)]
  [(verify b s n_l b_i γ η f) (s γ η)]
  [(verify (quote variable) s n_l b γ η f) (s γ η)]
  [(verify void s n_l b γ η f) (s γ η)]
  
  ; install-value
  [(verify (install-value n e_r e_b) s n_l b γ η f)
   (verify e_b (set imm n s_2) n_l b γ η f)
   (side-condition (< (term n) (term n_l)))
   (where (s_1 γ_1 η_1) (verify e_r s n_l #f γ η ∅))
   (where s_2 (trim s_1 s))
   (side-condition (term (valid? s_2)))
   (where uninit (stack-ref n s_2))]
  [(verify (install-value-box n e_r e_b) (name s (ṽ_0 ...)) n_l b γ η f)
   (verify e_b s_2 n_l b γ_1 η_1 f)
   (side-condition (< (term n) (length (term s))))
   (where (s_1 γ_1 η_1) (verify e_r s n_l #f γ η ∅))
   (where s_2 (trim s_1 s))
   (side-condition (term (valid? s_2)))
   (side-condition (memq (term (stack-ref n s_2)) '(box box-nc)))]
  
  ; boxenv
  [(verify (boxenv n_p e) (ṽ_0 ... imm ṽ_n+1 ...) n_l b γ η f)
   (verify e (ṽ_0 ... box ṽ_n+1 ...) n_l b γ η f)
   (side-condition (= (length (term (ṽ_0 ...))) (term n_p)))
   (side-condition (< (term n_p) (term n_l)))]
  
  ; let-rec
  [(verify (let-rec ((name l (lam ((name v val) ...) (n_0 n_1 ...) e_0)) ...) e) (ṽ_0 ... ṽ_n ...) n_l b γ η f)
   (verify e s_1 n_l b γ η f)
   (where n ,(length (term (l ...))))
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))
   (side-condition (term (AND (same? ṽ_0 uninit) ...)))
   (side-condition (<= (term n) (term n_l)))
   (where s_1 (abs-push n imm (ṽ_n ...)))
   (where (n_i ...) (count-up ,(length (term (l ...)))))
   (side-condition (term (AND (lam-verified? l s_1 n_i) ...)))]
  
  ; indirect
  [(verify (indirect x) s n_l b γ η f) (s γ η)]
  
  ; else
  [(verify e s n_l b γ η f) (invalid γ η)])

(define-metafunction verification
  verify* : (e ...) s n_l b γ η -> (s γ η)
  [(verify* () s n_l b γ η) (s γ η)]
  [(verify* (e_0 e_1 ...) s n_l b γ η)
   (verify* (e_1 ...) (trim s_1 s) n_l b γ_1 η_1)
   (where (s_1 γ_1 η_1) (verify e_0 s n_l b γ η ∅))])

(define-metafunction verification
  verify*-ref : (e ...) (τ ...) s n_l γ η -> (s γ η)
  [(verify*-ref () () s n_l γ η) (s γ η)]
  [(verify*-ref (e_0 e_1 ...) (val τ_1 ...) s n_l γ η)
   (verify*-ref (e_1 ...) (τ_1 ...)
                (trim s_1 s) n_l γ_1 η_1)
   (where (s_1 γ_1 η_1) (verify e_0 s n_l #f γ η ∅))]
  [(verify*-ref (e_0 e_1 ...) () s n_l γ η)
   (verify* (e_0 e_1 ...) s n_l #f γ η)]
  [(verify*-ref () (τ_0 τ_1 ...) s n_l γ η) (s γ η)]
  [(verify*-ref ((loc n) e_1 ...) (ref τ_1 ...) s n_l γ η)
   (verify*-ref (e_1 ...) (τ_1 ...) s_1 n_l γ_1 η_1)
   ; Require the reference to load a box.
   (where (s_1 γ_1 η_1) (verify (loc-box n) s n_l #f γ η ∅))]
  [(verify*-ref ((loc-noclr n) e_1 ...) (ref τ_1 ...) s n_l γ η)
   (verify*-ref (e_1 ...) (τ_1 ...) s_1 n_l γ_1 η_1)
   ; Require the reference to load a box.
   (where (s_1 γ_1 η_1) (verify (loc-box-noclr n) s n_l #f γ η ∅))]
  [(verify*-ref ((loc-clr n) e_1 ...) (ref τ_1 ...) s n_l γ η)
   (verify*-ref (e_1 ...) (τ_1 ...) s_1 n_l γ_1 η_1)
   ; Require the reference to load a box.
   (where (s_1 γ_1 η_1) (verify (loc-box-clr n) s n_l #f γ η ∅))]
  [(verify*-ref (e ...) (τ ...) s n_l γ η) (invalid γ η)])

(define-metafunction verification
  [(lam-verified? (lam (τ_0 ...) (n_0 ...) e) (name s (ṽ_0 ...)) m)
   (valid? s_1)
   (where n_d ,(length (term s)))
   (where n_d* ,(+ (length (term (τ_0 ...))) (length (term (n_0 ...)))))
   (side-condition (term (AND (less-than? n_0 n_d) ...)))
   (side-condition (term (AND (not-member? (stack-ref n_0 s) (uninit not)) ...)))
   (where (ṽ ...) ((drop-noclr (stack-ref n_0 s)) ...))
   (where f (extract-self m (n_0 ...) (τ_0 ...) (ṽ ...)))
   (where (s_1 γ_1 η_1) (verify e (ṽ ... (arg τ_0) ...) n_d* #f () () f))]
  [(lam-verified? l s m) #f])

(define-metafunction verification
  [(extract-self ? (n_0 ...) (τ_0 ...) (ṽ_0 ...)) ∅]
  [(extract-self n_i (n_0 ... n_i n_i+1 ...) (τ_0 ...) (ṽ_0 ...))
   (,(length (term (n_0 ...))) ,(length (term (τ_0 ...))) (ṽ_0 ...))
   ; When a closure captures itself multiple times, only the last 
   ; occurrence is considered a self-reference.
   (side-condition (term (not-member? n_i (n_i+1 ...))))]
  [(extract-self n (n_0 ...) (τ_0 ...) (ṽ_0 ...)) ∅])

(define-metafunction verification
  drop-noclr : ṽ -> ṽ
  [(drop-noclr imm-nc) imm]
  [(drop-noclr box-nc) box]
  [(drop-noclr ṽ) ṽ])

(define-metafunction verification
  [(verify-self-app (application e_0 e_1 ...) (name s (ṽ_0 ...)) n_l γ η (n_f n_s (ṽ_j ...)))
   (s_1 γ_1 η_1)
   (where n ,(length (term (e_1 ...))))
   (where n_l* ,(+ (term n) (term n_l)))
   (where (s_1 γ_1 η_1) (verify* (e_0 e_1 ...) (abs-push n not s) n_l* #f γ η))
   (side-condition (term (valid? s_1)))
   (where (n_j ...) (count-up ,(length (term (ṽ_j ...)))))
   (side-condition (term (closure-intact? ((stack-ref (plus n_j n_s) s_1) ...) (ṽ_j ...))))]
  [(verify-self-app e s n_l γ η f) (invalid γ η)])

(define-metafunction verification
  closure-intact? : (ṽ ...) (ṽ ...) -> b 
  [(closure-intact? () ()) #t]
  [(closure-intact? (imm-nc ṽ_1 ...) (imm ṽ_2 ...))
   (closure-intact? (ṽ_1 ...) (ṽ_2 ...))]
  [(closure-intact? (box-nc ṽ_1 ...) (box ṽ_2 ...))
   (closure-intact? (ṽ_1 ...) (ṽ_2 ...))]
  [(closure-intact? (ṽ ṽ_1 ...) (ṽ ṽ_2 ...))
   (closure-intact? (ṽ_1 ...) (ṽ_2 ...))]
  [(closure-intact? (ṽ_1 ...) (ṽ_2 ...)) #f])

(define-metafunction verification
  shift : n f -> f
  [(shift n ∅) ∅]
  [(shift n (n_f n_s (ṽ ...))) 
   (,(+ (term n) (term n_f)) ,(+ (term n) (term n_s)) (ṽ ...))])

(define-metafunction verification
  arg : τ -> ṽ
  [(arg val) imm]
  [(arg ref) box])

(define-metafunction verification
  abs-push : n ṽ (ṽ ...) -> s
  [(abs-push 0 ṽ (ṽ_0 ...)) (ṽ_0 ...)]
  [(abs-push n ṽ (ṽ_0 ...))
   (abs-push ,(sub1 (term n)) ṽ (ṽ ṽ_0 ...))])

(define-metafunction verification
  stack-ref : n (ṽ ...) -> ṽ
  [(stack-ref n (ṽ_0 ... ṽ_n ṽ_n+1 ...))
   ṽ_n
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))])

(define-metafunction verification
  set : ṽ n (ṽ ...) -> (ṽ ...)
  [(set ṽ n (ṽ_0 ... ṽ_n ṽ_n+1 ...))
   (ṽ_0 ... ṽ ṽ_n+1 ...)
   (side-condition (= (length (term (ṽ_0 ...))) (term n)))])

(define-metafunction verification
  nc : ṽ -> ṽ
  [(nc imm) imm-nc]
  [(nc imm-nc) imm-nc]
  [(nc box) box-nc]
  [(nc box-nc) box-nc])

(define-metafunction verification
  log-noclr : n n ṽ η -> η
  [(log-noclr n_p n_l ṽ_p (n_0 ...))
   (,(- (term n_p) (term n_l)) n_0 ...)
   (side-condition (>= (term n_p) (term n_l)))
   (side-condition (memq (term ṽ_p) '(imm box)))]
  [(log-noclr n_p n_l ṽ_p η) η])

(define-metafunction verification
  undo-noclrs : η s -> s
  [(undo-noclrs η invalid) invalid]
  [(undo-noclrs () s) s]
  [(undo-noclrs (n_0 n_1 ...) (ṽ_0 ... imm-nc ṽ_i ...))
   (undo-noclrs (n_1 ...) (ṽ_0 ... imm ṽ_i ...))
   (side-condition (= (length (term (ṽ_0 ...))) (term n_0)))]
  [(undo-noclrs (n_0 n_1 ...) (ṽ_0 ... box-nc ṽ_i ...))
   (undo-noclrs (n_1 ...) (ṽ_0 ... box ṽ_i ...))
   (side-condition (= (length (term (ṽ_0 ...))) (term n_0)))]
  [(undo-noclrs (n_0 n_1 ...) s)
   (undo-noclrs (n_1 ...) s)])

(define-metafunction verification
  log-clr : n s n γ -> γ
  [(log-clr n_p (name s (ṽ_0 ... ṽ_p ṽ_p+1 ...)) n_l ((n_i ṽ_i) ...))
   ((,(- (- (term n_h) (term n_p)) (term 1)) ṽ_p) (n_i ṽ_i) ...)
   (where n_h ,(length (term s)))
   (side-condition (>= (term n_p) (term n_l)))
   (side-condition (= (length (term (ṽ_0 ...))) (term n_p)))]
  [(log-clr n_p s n_l γ) γ])

(define-metafunction verification
  undo-clrs : γ s -> s
  [(undo-clrs γ invalid) invalid]
  [(undo-clrs () s) s]
  [(undo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ...) s)
   (undo-clrs ((n_1 ṽ_1) ...) (set ṽ_0 ,(- (- (term n_h) (term n_0)) (term 1)) s))
   (where n_h ,(length (term s)))
   (side-condition (< (term n_0) (term n_h)))]
  [(undo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ...) s)
   (undo-clrs ((n_1 ṽ_1) ...) s)])

(define-metafunction verification
  redo-clrs : γ s -> s
  [(redo-clrs γ invalid) invalid]
  [(redo-clrs () s) s]
  [(redo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ...) s) 
   (redo-clrs ((n_1 ṽ_1) ...) (set not ,(- (- (term n_h) (term n_0)) (term 1)) s))
   (where n_h ,(length (term s)))
   (side-condition (< (term n_0) (term n_h)))]
  [(redo-clrs ((n_0 ṽ_0) (n_1 ṽ_1) ...) s) 
   (redo-clrs ((n_1 ṽ_1) ...) s)])

(define-metafunction verification
  trim : s s -> s
  [(trim invalid s) invalid]
  [(trim (ṽ_1 ... ṽ_2 ...) (ṽ_3 ...))
   (ṽ_2 ...)
   (side-condition (= (length (term (ṽ_2 ...))) (length (term (ṽ_3 ...)))))])

(define-metafunction verification
  [(valid? invalid) #f]
  [(valid? (ṽ ...)) #t])


;; Typsetting tricks:

(define-metafunction verification
  [(AND) #t]
  [(AND #t any_1 ...) (AND any_1 ...)]
  [(AND any_0 any_1 ...) #f])

(define-metafunction verification
  [(same? any_1 any_1) #t]
  [(same? any_1 any_2) #f])

(define-metafunction verification
  [(less-than? n_1 n_2) ,(< (term n_1) (term n_2))])

(define-metafunction verification
  [(plus n_1 n_2) ,(+ (term n_1) (term n_2))])

(define-metafunction verification
  [(not-member? any_1 (any_2 ...))
   ,(not (member (term any_1) (term (any_2 ...))))])

(provide (all-defined-out))
