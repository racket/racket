#lang scheme

(require redex/reduction-semantics)
(require "grammar.ss")

(define-extended-language runtime bytecode
  (p (V S H T C) error)
  
  (V v uninit (box x))
  
  (S (u ... s))
  (s ε S)
  (u v uninit (box x))
  
  (H ((x h) ...))
  (h v ((clos n (u ...) x) ...))
  
  (T ((x e) ...))
  
  (C (i ...))
  
  (i e 
     (swap n) (reorder i (e m) ...)
     (set n) (set-box n)
     (branch e e)
     framepop framepush
     (call n) (self-call x))
  
  (l (lam n (n ...) x))
  (v .... 
     undefined
     (clos x))
  (e ....
     (self-app x e_0 e_1 ...))
  (m n ?))

(define-metafunction bytecode
  [(count-up number)
   ,(build-list (term number) (λ (x) x))])

(define procedure-rules
  (reduction-relation
   runtime
   (--> (V S ((x_0 h_0) ...) T ((lam n (n_0 ...) x_i) i ...))
        ((clos x) S ((x ((clos n ((stack-ref n_0 S) ...) x_i))) (x_0 h_0) ...) T (i ...))
        (fresh x)
        "lam")
   (--> (V S ((x_0 h_0) ...) T ((case-lam (lam n (n_0 ...) x_i) ...) i ...))
        ((clos x) S ((x ((clos n ((stack-ref n_0 S) ...) x_i) ...)) (x_0 h_0) ...) T (i ...))
        (fresh x)
        "case-lam")
   (--> (V S ((x_0 h_0) ...) T ((let-rec ((name l_0 (lam n_0 (n_00 ...) y_0)) ...) e) i ...))
        (V S_* ((x_0 h_0) ... (x ((clos n_0 ((stack-ref n_00 S_*) ...) y_0))) ...) T (e i ...))
        (fresh ((x ...) (l_0 ...)))
        (where (n ...) (count-up ,(length (term (l_0 ...)))))
        (where S_* (stack-set* ((clos x) n) ... S))
        "let-rec")))

;; hide the 'apply append' in a metafunction 
(define-metafunction runtime
  [(flatten ((any ...) ...))
   (any ... ...)])

(define application-rules
  (reduction-relation
   runtime
   (--> (V S H T ((application e_0 e_1 ...) i ...))
        (V (push-uninit n S) H T ((reorder (call n) (e_0 ?) (e_1 n_1) ...) i ...))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "application")
   (--> (V S H T ((self-app x e_0 e_1 ...) i ...))
        (V S H T ((application e_0 e_1 ...) i ...))
        "self-app")
   (--> (V S H T ((self-app x e_0 e_1 ...) i ...))
        (V (push-uninit n S) H T ((reorder (self-call x) (e_1 n_1) ...) i ...))
        (where n ,(length (term (e_1 ...))))
        (where (n_1 ...) (count-up n))
        "self-app-opt")
   (--> (V S H T ((reorder i_r (e_0 m_1) ... ((loc-noclr n) m_i) (e_i+1 m_i+1) (e_i+2 m_i+2) ...) i ...))
        (V S H T ((reorder i_r (e_0 m_1) ... (e_i+1 m_i+1) (e_i+2 m_i+2) ... ((loc-noclr n) m_i)) i ...))
        "reorder")
   (--> (V S H T ((reorder (call n) (e_0 n_0) ... (e_i ?) (e_i+1 n_i+1) ... (e_j n_j)) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  framepush e_i framepop (set n_j)
                  ,@(term (flatten ((framepush e_i+1 framepop (set n_i+1)) ...)))
                  framepush e_j framepop
                  (swap n_j) (call n) i ...))
        "finalize-app-not-last")
   (--> (V S H T ((reorder (call n) (e_0 n_0) ... (e_n ?)) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  framepush e_n framepop (call n) i ...))
        "finalize-app-is-last")
   (--> (V S H T ((reorder (self-call x) (e_0 n_0) ...) i ...))
        (V S H T (,@(term (flatten ((framepush e_0 framepop (set n_0)) ...)))
                  (self-call x) i ...))
        "finalize-self-app")
   (--> (V (u_0 ... u_i ... (u_j ... (u_k ... s))) H (name T ((x_0 e_0) ... (x_i e_i) (x_i+1 e_i+1) ...)) ((self-call x_i) i ...))
        (V ((u_j ... (u_0 ... s))) H T (e_i i ...))
        (side-condition (= (length (term (u_0 ...))) (length (term (u_k ...)))))
        "self-call")
   (--> ((clos x_i) (u_1 ... u_n+1 ... (u_m ... (u_k ... s))) (name H ((x_0 h_0) ...
                                                                       (x_i ((clos n_0 (u_0 ...) y_0) ...
                                                                             (clos n_i (u_i ...) y_i) 
                                                                             (clos n_i+1 (u_i+1 ...) y_i+1) ...))
                                                                       (x_i+1 h_i+1) ...)) (name T ((y_j e_j) ... (y_i e_i) (y_k e_k) ...)) ((call n_i) i ...))
        ((clos x_i) ((u_i ... (u_1 ... s))) H T (e_i i ...))
        (side-condition (not (memq (term n_i) (term (n_0 ...)))))
        (side-condition (= (term n_i) (length (term (u_1 ...)))))
        "call")
   (--> (v S H T ((call n) i ...))
        error
        "non-closure"
        (side-condition (not (clos? (term v)))))
   (--> ((clos x_i) 
         S
         ((x_0 h_0) ... (x_i ((clos n_0 (u_0 ...) y_0) ...)) (x_i+1 h_i+1) ...)
         T
         ((call n) i ...))
        error
        (side-condition (not (memq (term n) (term (n_0 ...)))))
        "app-arity")))

(define stack-ref-rules
  (reduction-relation
   runtime
   (--> (V S H T ((loc n) i ...))
        ((stack-ref n S) S H T (i ...))
        "loc")
   (--> (V S H T ((loc-noclr n) i ...))
        ((stack-ref n S) S H T (i ...))
        "loc-noclr")
   (--> (V S H T ((loc-clr n) i ...))
        ((stack-ref n S) (stack-set uninit n S) H T (i ...))
        "loc-clr")
   
   (--> (V S H T ((loc-box n) i ...))
        ((heap-ref (stack-ref n S) H) S H T (i ...))
        "loc-box")
   (--> (V S H T ((loc-box-noclr n) i ...))
        ((heap-ref (stack-ref n S) H) S H T (i ...))
        "loc-box-noclr")
   (--> (V S H T ((loc-box-clr n) i ...))
        ((heap-ref (stack-ref n S) H) (stack-set uninit n S) H T (i ...))
        "loc-box-clr")))

(define stack-instructions
  (reduction-relation
   runtime
   (--> (V S H T ((set n) i ...))
        (V (stack-set V n S) H T (i ...))
        "set")
   (--> (v S H T ((set-box n) i ...))
        (v S (heap-set v (stack-ref n S) H) T (i ...))
        "set-box")
   (--> (V S H T ((swap n) i ...))
        ((stack-ref n S) (stack-set V n S) H T (i ...))
        "swap")
   (--> (V (u_0 ... (u_i ... (u_j ... s))) H T (framepop i ...))
        (V s H T (i ...))
        "framepop")
   (--> (V S H T (framepush i ...))
        (V (((S))) H T (i ...))
        "framepush")))

(define stack-change-rules
  (reduction-relation
   runtime
   (--> (V S H T ((install-value n e_r e_b) i ...))
        (V S H T (framepush e_r framepop (set n) e_b i ...))
        "install-value")
   (--> (V S H T ((install-value-box n e_r e_b) i ...))
        (V S H T (framepush e_r framepop (set-box n) e_b i ...))
        "install-value-box")
   (--> (V S ((x_0 h_0) ...) T ((boxenv n e) i ...))
        (V (stack-set (box x) n S) ((x v) (x_0 h_0) ...) T (e i ...))
        (fresh x)
        (where v (stack-ref n S))
        "boxenv")))

(define stack-push-rules
  (reduction-relation
   runtime
   (--> (V S H T ((let-one e_r e_b) i ...))
        (V (push-uninit 1 S) H T (framepush e_r framepop (set 0) e_b i ...))
        "let-one")
   (--> (V S H T ((let-void n e) i ...))
        (V (push-uninit n S) H T (e i ...))
        "let-void")
   (--> (V S ((x_0 h_0) ...) T ((let-void-box n e) i ...))
        (V (push ((box x_n) ...) S) ((x_n undefined) ... (x_0 h_0) ...) T (e i ...))
        (where (x_n ...) (n-gensyms n))
        "let-void-box")))

(define-metafunction runtime
  [(n-gensyms n)
   ,(build-list (term n) (λ (_) (gensym 'x)))])

(define miscellaneous-rules
  (reduction-relation
   runtime
   (--> (V S H T (v i ...))
        (v S H T (i ...))
        "value")
   (--> (V S H T ((branch e_c e_t e_f) i ...))
        (V S H T (framepush e_c framepop (branch e_t e_f) i ...))
        "branch")
   (--> (v S H T ((branch e_t e_f) i ...))
        (v S H T (e_t i ...))
        (side-condition (≠ (term v) (term #f)))
        "branch-true")
   (--> (#f S H T ((branch e_t e_f) i ...))
        (#f S H T (e_f i ...))
        "branch-false")
   (--> (V S H T ((seq e_1 e_2 e_3 e_4 ...) i ...))
        (V S H T (framepush e_1 framepop (seq e_2 e_3 e_4 ...) i ...))
        "seq-many")
   (--> (V S H T ((seq e_1 e_2) i ...))
        (V S H T (framepush e_1 framepop e_2 i ...))
        "seq-two")
   (--> (V S H (name T ((x_0 e_0) ... (x_i e_i) (x_i+1 e_i+1) ...)) ((indirect x_i) i ...))
        (V S H T (e_i i ...))
        "indirect")))

(define ->
  (union-reduction-relations
   stack-ref-rules
   stack-instructions
   stack-push-rules
   stack-change-rules
   procedure-rules
   application-rules
   miscellaneous-rules))

(define (≠ a b) (not (equal? a b)))

(define clos? 
  (redex-match runtime (clos x)))

(define-extended-language loader bytecode
  (φ - (n n x))
  (e any)
  (H any)
  (h any)
  (T any)
  (l any))

(define-metafunction loader
  concat : (any ...) ... -> (any ...)
  [(concat any ...) ,(apply append (term (any ...)))])

(define-metafunction loader
  [(load e ((x_0 e_0) ...))
   (uninit 
    (((ε))) 
    (concat H H_0 ...)
    (concat T ((x_0 e_0*) ...) T_0 ...)
    (e_*))
   (where (e_* H T) (load* e -))
   (where ((e_0* H_0 T_0) ...) ((load* e_0 -) ...))])

(define-metafunction loader
  [(φ+ - n) -]
  [(φ+ (n_p n_a x) n) (,(+ (term n) (term n_p)) n_a x)])

(define-metafunction loader
  [(load-lam-rec (lam (τ_0 ...) (n_0 ... n_i n_i+1 ...) e) n_i)
   ; When a closure captures itself multiple times, only the last 
   ; occurrence is considered a self-reference.
   ((lam n (n_0 ... n_i n_i+1 ...) x) H ((x e_*) (x_0 e_0) ...))
   (where n ,(length (term (τ_0 ...))))
   (where x ,(gensym 'x))
   (where (e_* H ((x_0 e_0) ...))
          (load* e (,(length (term (n_0 ...))) n x)))
   (side-condition (not (memq (term n_i) (term (n_i+1 ...)))))]
  [(load-lam-rec l n_j) (load* l -)])

(define-metafunction loader
  [(load* (application (loc-noclr n) e_1 ...) (n_p n_a x))
   ((self-app x (loc-noclr n) e_1* ...)
    (concat H_1 ...)
    (concat T_1 ...))
   (side-condition (= (term n) (+ (term n_p) (length (term (e_1 ...))))))
   (side-condition (= (term n_a) (length (term (e_1 ...)))))
   (where ((e_1* H_1 T_1) ...) ((load* e_1 -) ...))]
  
  [(load* (let-rec (l_0 ...) e) φ)
   ((let-rec (l_0* ...) e_*)
    (concat H H_0 ...)
    (concat T T_0 ...))
   (where (e_* H T) (load* e φ))
   (where (n_0 ...) (count-up ,(length (term (l_0 ...)))))
   (where ((l_0* H_0 T_0) ...) ((load-lam-rec l_0 n_0) ...))]
  
  [(load* (application e_0 e_1 ...) φ)
   ((application e_0* ...)
    (concat H_0 ...)
    (concat T_0 ...))
   (where ((e_0* H_0 T_0) ...) ((load* e_0 -) (load* e_1 -) ...))]
  
  [(load* (let-one e_r e_b) φ)
   ((let-one e_r* e_b*) (concat H_r H_b) (concat T_r T_b))
   (where (e_r* H_r T_r) (load* e_r -))
   (where (e_b* H_b T_b) (load* e_b (φ+ φ 1)))]
  
  [(load* (let-void n e) φ)
   ((let-void n e_*) H T)
   (where (e_* H T) (load* e (φ+ φ n)))]
  [(load* (let-void-box n e) φ)
   ((let-void-box n e_*) H T)
   (where (e_* H T) (load* e (φ+ φ n)))]
  
  [(load* (boxenv n e) φ)
   ((boxenv n e_*) H T)
   (where (e_* H T) (load* e φ))]
  
  [(load* (install-value n e_r e_b) φ)
   ((install-value n e_r* e_b*)
    (concat H_r H_b)
    (concat T_r T_b))
   (where (e_r* H_r T_r) (load* e_r -))
   (where (e_b* H_b T_b) (load* e_b φ))]
  [(load* (install-value-box n e_r e_b) φ)
   ((install-value-box n e_r* e_b*)
    (concat H_r H_b) 
    (concat T_r T_b))
   (where (e_r* H_r T_r) (load* e_r -))
   (where (e_b* H_b T_b) (load* e_b φ))]
  
  [(load* (seq e_0 ... e_n) φ)
   ((seq e_0* ... e_n*) 
    (concat H_0 ... H_n)
    (concat T_0 ... T_n))
   (where ((e_0* H_0 T_0) ...) ((load* e_0 -) ...))
   (where (e_n* H_n T_n) (load* e_n φ))]
  
  [(load* (branch e_c e_t e_f) φ)
   ((branch e_c* e_t* e_f*)
    (concat H_c H_t H_f)
    (concat T_c T_t T_f ))
   (where (e_c* H_c T_c) (load* e_c -))
   (where (e_t* H_t T_t) (load* e_t φ))
   (where (e_f* H_f T_f) (load* e_f φ))]
  
  [(load* (lam (τ_0 ...) (n_0 ...) e) φ)
   ((lam n (n_0 ...) x) H ((x e_*) (x_0 e_0) ...))
   (where x ,(gensym 'x))
   (where n ,(length (term (τ_0 ...))))
   (where (e_* H ((x_0 e_0) ...)) (load* e -))]
  
  [(load* (proc-const (τ_0 ...) e) φ)
   ((clos x) 
    ((x ((clos n () x_*))) (x_0 h_0) ...)
    ((x_* e_*) (x_i e_i) ...))
   (where x ,(gensym 'x))
   (where x_* ,(gensym 'x))
   (where n ,(length (term (τ_0 ...))))
   (where (e_* ((x_0 h_0) ...) ((x_i e_i) ...)) (load* e -))]
  
  [(load* (case-lam l_0 ...) φ)
   ((case-lam l_0* ...) (concat H_0 ...) (concat T_0 ...))
   (where ((l_0* H_0 T_0) ...) ((load* l_0 φ) ...))]
  
  [(load* e φ) (e () ())])

(define-metafunction
  runtime
  heap-ref : (box x) H -> h
  [(heap-ref (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...)) h_i])

(define-metafunction
  runtime
  heap-set : h (box x) H -> H
  [(heap-set h (box x_i) ((x_0 h_0) ... (x_i h_i) (x_i+1 h_i+1) ...))
   ((x_0 h_0) ... (x_i h) (x_i+1 h_i+1) ...)])

(define-metafunction
  runtime
  push : (u ...) (u ... s) -> (u ... s)
  [(push (u_0 ...) (u_i ... s))
   (u_0 ... u_i ... s)])

(define-metafunction
  runtime
  push-uninit : n (u ... s) -> (uninit ... u ... s)
  [(push-uninit 0 S) S]
  [(push-uninit n (u ... s))
   (push-uninit ,(- (term n) (term 1)) (uninit u ... s))])

(define-metafunction
  runtime
  stack-ref : n S -> u
  [(stack-ref 0 (v u ... s)) v]
  [(stack-ref 0 ((box x) u ... s)) (box x)]
  [(stack-ref n (u_0 u_1 ... s)) 
   (stack-ref ,(- (term n) (term 1)) (u_1 ... s))
   (side-condition (> (term n) (term 0)))]
  [(stack-ref n ((u ... s)))
   (stack-ref n (u ... s))])

(define-metafunction
  runtime
  stack-set : u n S -> S
  [(stack-set u n (u_0 ... u_n u_n+1 ... s))
   (u_0 ... u u_n+1 ... s)
   (side-condition 
    (= (term n) (length (term (u_0 ...)))))]
  [(stack-set u n (u_0 ... s))
   (u_0 ... (stack-set u ,(- (term n) (length (term (u_0 ...)))) s))])

(define-metafunction
  runtime
  stack-set* : (u n) ... S -> S
  [(stack-set* S) S]
  [(stack-set* (u_0 n_0) (u_1 n_1) ... S)
   (stack-set* (u_1 n_1) ... (stack-set u_0 n_0 S))])

(provide (all-defined-out))
