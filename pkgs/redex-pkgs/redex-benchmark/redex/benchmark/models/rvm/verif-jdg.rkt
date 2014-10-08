#lang racket

(require redex/reduction-semantics 
         "jdg-grammar.rkt")

(provide (all-defined-out))


(define (bytecode-ok? e)
  (not (eq? 'invalid (car (term (verify ,e () 0 #f () () ∅))))))

(define (bytecode-ok/V? e)
  (judgment-holds
   (V ,e • O #f • • ∅ s_1 γ_1 η_1)))

(define-extended-language verification 
  bytecode
  (s (ṽ ...) invalid)
  (ṽ uninit imm box imm-nc box-nc not)
  (γ ((n ṽ) ...))
  (η (n ...))
  (f (n n (ṽ ...)) ∅)
  (m n ?))

(define-language bl
  (e (loc n)
     (loc-noclr n)
     (loc-clr n)
     (loc-box n)
     (loc-box-noclr n)
     (loc-box-clr n)
     
     (let-one e e)
     (let-void n e)
     (let-void-box n e)
     
     (boxenv n e)
     (install-value n e e)
     (install-value-box n e e)
     
     (application e el)
     ;; change to (seq el) ?
     (seq e e)
     (branch e e e)
     (let-rec ll e)
     (indirect x)
     (proc-const τl e)
     (case-lam ll)
     l
     v)
  
  (el (e el) •)
  
  (l (lam τl nl e))
  
  (ll (l ll) •)
  
  (τl (τ τl) •)
  
  (nl (n nl) •)
  
  (v n
     void
     'variable
     b)
  
  (τ val ref)
  (n O (S n))
  (b boolean)
  ((x y) variable))

(define-metafunction bl
  [(trans-e (application e el))
   (application (trans-e e) (trans-e e_2) ...)
   (where (e_2 ...) (cns->lst el))]
  [(trans-e (proc-const τl e))
   (proc-const (cns->lst τl) (trans-e e))]
  [(trans-e (case-lam el))
   (case-lam any ...)
   (where (e ...) (cns->lst el))
   (where (any ...) ((trans-e e) ...))]
  [(trans-e (any n e))
   (any (trans-n n) (trans-e e))
   (side-condition (memq (term any) '(let-void let-void-box)))]
  [(trans-e (seq e_1 e_2))
   (seq any_3 ... any_4 ...)
   (where (any_3 ...) (un-seq e_1))
   (where (any_4 ...) (un-seq e_2))]
  [(trans-e (branch e_1 e_2 e_3))
   (branch (trans-e e_1) (trans-e e_2) (trans-e e_3))]
  [(trans-e (let-one e_1 e_2))
   (let-one (trans-e e_1) (trans-e e_2))]
  [(trans-e (boxenv n e))
   (boxenv (trans-n n) (trans-e e))]
  [(trans-e (any n e_1 e_2))
   (any (trans-n n) (trans-e e_1) (trans-e e_2))
   (side-condition (memq (term any) '(install-value install-value-box)))]
  [(trans-e (proc-const τl e))
   (proc-const (cns->lst τl) (trans-e e))]
  [(trans-e (let-rec ll e))
   (let-rec ((trans-l any_l) ...) (trans-e e))
   (where (any_l ...) (cns->lst ll))]
  [(trans-e (any n))
   (any (trans-n n))
   (side-condition (memq (term any) '(loc loc-noclr loc-clr loc-box loc-box-clr loc-box-noclr)))]
  [(trans-e l_1)
   (trans-l l_1)]
  [(trans-e n)
   (trans-n n)]
  [(trans-e any)
   any])

(define-metafunction bl
  [(trans-l (lam τl nl e))
   (lam (cns->lst τl) (number_2 ...) (trans-e e))
   (where (n_1 ...) (cns->lst nl))
   (where (number_2 ...) ((trans-n n_1) ...))])

(define-metafunction bl
  [(un-seq e_1)
   (any_2 ...)
   (where (seq any_2 ...) (trans-e e_1))]
  [(un-seq e)
   ((trans-e e))])

(define-metafunction bl
  [(trans-n O)
   0]
  [(trans-n (S n))
   ,(add1 (term (trans-n n)))])

(define-extended-language vl
  bl
  (sl (ν sl) •)
  (s sl invalid)
  (ν uninit imm box imm-nc box-nc not)
  (γ ((n ν) γ) •)
  (η (n η) •)
  (f (n n sl) ∅)
  (m n ?))


(define (check-V-res v-res)
  ;(displayln (list 'check-V-res v-res))
  (match v-res
    [`(V ,e ,s1 ,n ,b ,γ1 ,η1 ,f ,s2 ,γ2 ,η2)
     (unless
         (equal?
          (term
           (verify
            (trans-e ,e)
            (trans-s ,s1)
            (trans-n ,n)
            ,b
            (trans-γ ,γ1)
            (trans-η ,η1)
            (trans-f ,f)))
          (term
           ((trans-s ,s2)
            (trans-γ ,γ2)
            (trans-η ,η2))))
       (error 'check-V-res "failed on ~s " e))]))

(define (check-Vs n #:generator [g #f])
  (define gen (redex-generator vl
                               (V e • O #f • • ∅ s_1 γ_1 η_1)
                               6))
  (for ([_ (in-range n)])
    (define maybe-V 
      (if g 
          (gen)
          (generate-term
           vl
           #:satisfying
           ;(V e s n b γ η f s_1 γ_1 η_1)
           (V e • O #f • • ∅ s_1 γ_1 η_1)
           5)))
    (match maybe-V
      [#f
       (display ".")]
      [`(V ,e ,s ,n ,b ,γ ,η ,f ,s_1 ,γ_1 ,η_1)
       (displayln (term (trans-e ,e)))
       (check-V-res maybe-V)])))

(define-metafunction vl
  [(trans-γ ((n ν) γ))
   (((trans-n n) ν) any ...)
   (where (any ...) (trans-γ γ))]
  [(trans-γ •)
   ()])

(define-metafunction vl
  [(trans-η (n η))
   ((trans-n n) any ...)
   (where (any ...) (trans-η η))]
  [(trans-η •)
   ()])
         
(define-metafunction vl
  [(trans-f ∅)
   ∅]
  [(trans-f (n_1 n_2 sl))
   ((trans-n n_1) (trans-n n_2) (cns->lst sl))])

(define-metafunction vl
  [(trans-s invalid)
   invalid]
  [(trans-s sl)
   (cns->lst sl)])
         
(define-metafunction vl
  [(cns->lst (any_1 any_2))
   (any_1 any_3 ...)
   (where (any_3 ...) (cns->lst any_2))]
  [(cns->lst •)
   ()])


;; verification judgment -----------------------------------------------


(define-judgment-form vl
  #:mode (V I I I I I I I O O O)
  #:contract (V e s n b γ η f s γ η)
  
  ;localrefs
  [(V (loc n) s n_l #f γ η f s γ η)
   (lmem (sref n s) (imm (imm-nc •)))]
  [(V (loc n) s n_l #t γ η f s γ η)
   (lmem (sref n s) (imm (imm-nc (box (box-nc •)))))]
  [(V (loc-box n) s n_l b γ η f s γ η)
   (lmem (sref n s) (box (box-nc •)))]
  
  [(V (loc-noclr n) s n_l #f γ η f (supdt n (nc ν_n) s) γ η_l)
   (where ν_n (sref n s))
   (log-noclr n n_l ν_n η η_l)
   (lmem ν_n (imm (imm-nc •)))]
  [(V (loc-noclr n) s n_l #t γ η f (supdt n (nc ν_n) s) γ η_l)
   (where ν_n (sref n s))
   (log-noclr n n_l ν_n η η_l)
   (lmem ν_n (imm (imm-nc (box (box-nc •)))))]
  [(V (loc-box-noclr n) s n_l b γ η f (supdt n box-nc s) γ η_l)
   (where ν_n (sref n s))
   (log-noclr n n_l ν_n η η_l)
   (lmem ν_n (box (box-nc •)))]
  
  
  [(V (loc-clr n) s n_l #f γ η f (supdt n not s) γ_l η)
   (where imm (sref n s))
   (log-clr n s n_l γ γ_l)]
  [(V (loc-clr n) s n_l #t γ η f (supdt n not s) γ_l η)
   (lmem (sref n s) (imm (box •)))
   (log-clr n s n_l γ γ_l)]
  [(V (loc-box-clr n) s n_l b γ η f (supdt n not s) γ_l η)
   (where box (sref n s))
   (log-clr n s n_l γ γ_l)]

  ;branch
  [(V (branch e_c e_t e_e) s n_l b γ η f s_n (concat γ_2 γ_3) η_3)
   (V e_c s n_l #f γ η ∅ s_1 γ_1 η_1)
   (V e_t (trim s_1 s) O b • • f s_2 γ_2 η_2)
   (undo-clrs γ_2 (trim s_2 s) s_21)
   (undo-noclrs η_2 s_21 s_22)
   (V e_e s_22 O b γ_1 η_1 f s_3 γ_3 η_3)
   (redo-clrs γ_2 (trim s_3 s) s_n)]
  
  ;let-one
  [(V (let-one e_r e_b) sl n_l b γ η f s_2 γ_2 η_2)
   (where sl_0 (uninit sl))
   (V e_r sl_0 (n+ n_l (S O)) #f γ η ∅ sl_1 γ_1 η_1)
   (where (uninit sl_1*) (trim sl_1 sl_0))
   (V e_b (imm sl_1*) (n+ n_l (S O)) b γ_1 η_1 (shift (S O) f) s_2 γ_2 η_2)]
  
  ;seq
  [(V (seq e_0 e_1) s n_l b γ η f s_2 γ_2 η_2)
   (V e_0 s n_l #t γ η ∅ s_1 γ_1 η_1)
   (V e_1 (trim s_1 s) n_l b γ_1 η_1 f s_2 γ_2 η_2)]
  
  ;application
  [(V (application (loc-noclr n) el) s n_l b_i γ η (n_f n_s sl_f) s_2 γ_2 η_2)
   (n= n (n+ n_f (len el)))
   (V-self-app (application (loc-noclr n) el) s n_l γ η (n_f n_s sl_f) s_2 γ_2 η_2)]
  [(V (application (lam τl nl e) el) s n_l b γ η f s_2 γ_2 η_2)
   (where n (len el))
   (where n_l* (n+ n n_l))
   (where s_1 (abs-push n not s))
   (V*-ref el τl s_1 n_l* γ η s_2 γ_2 η_2)
   (lam-verified? (lam τl nl e) s_1 ?)]
  [(V (application (proc-const τl e) el) s n_l b γ η f s_2 γ_2 η_2)
   (V (application (lam τl • e) el) s n_l b γ η f s_2 γ_2 η_2)]
  [(V (application e_0 el) s n_l b γ η f s_2 γ_2 η_2)
   ;; the only place where cases might overlap, so exclude that explicitly
   (not-self-app (application e_0 el) s f)
   (where n (len el))
   (where n_l* (n+ n n_l))
   (V* (e_0 el) (abs-push n not s) n_l* #f γ η s_2 γ_2 η_2)]
  
  ; literals
  [(V n s n_l b γ η f s γ η)]
  [(V b s n_l b_i γ η f s γ η)]
  [(V (quote variable) s n_l b γ η f s γ η)]
  [(V void s n_l b γ η f s γ η)]
  
  ; install-value
  [(V (install-value n e_r e_b) s n_l b γ η f s_3 γ_3 η_3)
   (n< n n_l)
   (V e_r s n_l #f γ η ∅ s_1 γ_1 η_1)
   (where s_2 (trim s_1 s))
   (where uninit (sref n s_2))
   (V e_b (supdt n imm s_2) n_l b γ η f s_3 γ_3 η_3)]
  [(V (install-value-box n e_r e_b) s n_l b γ η f s_3 γ_3 η_3)
   (n< n (len s))
   (V e_r s n_l #f γ η ∅ s_1 γ_1 η_1)
   (where s_2 (trim s_1 s))
   (lmem (sref n s_2) (box (box-nc •)))
   (V e_b s_2 n_l b γ_1 η_1 f s_3 γ_3 η_3)]
  
  ; boxenv
  [(V (boxenv n_p e) s n_l b γ η f s_2 γ_2 η_2)
   (where imm (sref n_p s))
   (V e (supdt n_p box s) n_l b γ η f s_2 γ_2 η_2)
   (n< n_p n_l)]
  
  ; indirect
  [(V (indirect x) s n_l b γ η f s γ η)]
  
  ; let-void
  [(V (let-void n e) s n_l b_i γ η f s_1 γ_1 η_1)
   (V e (abs-push n uninit s) (n+ n n_l) b_i γ η (shift n f) s_1 γ_1 η_1)]
  [(V (let-void-box n e) s n_l b_i γ η f s_1 γ_1 η_1)
   (V e (abs-push n box s) (n+ n n_l) b_i γ η (shift n f) s_1 γ_1 η_1)]
  
   ; procedures in arbitrary context
  [(V (lam τl nl e) s n_l b γ η f s γ η)
   (vals (val τl))
   (lam-verified? (lam τl nl e) s ?)]
  [(V (proc-const τl e) s n_l b γ η f s_1 γ_1 η_1)
   (vals τl)
   (V (lam τl • e) s n_l b γ η f s_1 γ_1 η_1)]
  
  ; case-lam
  [(V (case-lam el) s n_l b γ η f s γ η)
   (lam-verified?* el s ?)]
  
   ; let-rec
  [(V (let-rec ll e) s n_l b γ η f s_2 γ_2 η_2)
   (where n (len ll))
   (n<=  n n_l)
   (lsplit n s s_a s_b)
   (uninits s_a)
   (where s_1 (abs-push n imm s_b))
   (verify-ll O s_1 ll)
   (V e s_1 n_l b γ η f s_2 γ_2 η_2)])

(define-judgment-form vl
  #:mode (verify-ll I I I)
  [(verify-ll n s •)]
  [(verify-ll n s ((lam τl (n_l nl) e) ll))
   (vals τl)
   (lam-verified? (lam τl (n_l nl) e) s n)
   (verify-ll (S n) s ll)])

(define-judgment-form vl
  #:mode (V* I I I I I I O O O)
  #:contract (V* el s n b γ η s γ η)
  [(V* • s n_l b γ η s γ η)]
  [(V* (e_0 el) s n_l b γ η s_2 γ_2 η_2)
   (V e_0 s n_l b γ η ∅ s_1 γ_1 η_1)
   (V* el (trim s_1 s) n_l b γ_1 η_1 s_2 γ_2 η_2)])

(define-judgment-form vl
  #:mode (V-self-app I I I I I I O O O)
  #:contract (V-self-app e s n γ η f s γ η)
  [(V-self-app (application e_0 el) sl n_l γ η (n_f n_s sl_f) sl_1 γ_1 η_1)
   (where n (len el))
   (where n_l* (n+ n n_l))
   (V* (e_0 el) (abs-push n not sl) n_l* #f γ η sl_1 γ_1 η_1)
   (closure-intact (ssblst n_s (slen sl_f) sl_1) sl_f)])

(define-judgment-form vl
  #:mode (V*-ref I I I I I I O O O)
  [(V*-ref • • s n_l γ η s γ η)]
  [(V*-ref (e_0 el) (val τl) s n_l γ η s_2 γ_2 η_2)
   (V e_0 s n_l #f γ η ∅ s_1 γ_1 η_1)
   (V*-ref el τl (trim s_1 s) n_l γ_1 η_1 s_2 γ_2 η_2)]
  [(V*-ref (e_0 el) • s n_l γ η s_2 γ_2 η_2)
   (V* (e_0 el) s n_l #f γ η s_2 γ_2 η_2)]
  [(V*-ref • (τ_0 τl) s n_l γ η s γ η)]
  [(V*-ref ((loc n) el) (ref τl) s n_l γ η s_2 γ_2 η_2)
   ; Require the reference to load a box.
   (V (loc-box n) s n_l #f γ η ∅ s_1 γ_1 η_1)
   (V*-ref el τl s_1 n_l γ_1 η_1 s_2 γ_2 η_2)]
  [(V*-ref ((loc-noclr n) el) (ref τl) s n_l γ η s_2 γ_2 η_2)
   ; Require the reference to load a box.
   (V (loc-box-noclr n) s n_l #f γ η ∅ s_1 γ_1 η_1)
   (V*-ref el τl s_1 n_l γ_1 η_1 s_2 γ_2 η_2)]
  [(V*-ref ((loc-clr n) el) (ref τl) s n_l γ η s_2 γ_2 η_2)
   ; Require the reference to load a box.
   (V (loc-box-clr n) s n_l #f γ η ∅ s_1 γ_1 η_1)
   (V*-ref el τl s_1 n_l γ_1 η_1 s_2 γ_2 η_2)])
 
(define-relation vl 
  [(closure-intact • •)]
  [(closure-intact (imm-nc sl_1) (imm sl_2))
   (closure-intact sl_1 sl_2)]
  [(closure-intact (box-nc sl_1) (box sl_2))
   (closure-intact sl_1 sl_2)]
  [(closure-intact (ν sl_1) (ν sl_2))
   (closure-intact sl_1 sl_2)])

(define-relation vl
  [(vals (val τl))
   (vals τl)]
  [(vals •)])

(define-relation vl
  [(uninits (uninit sl))
   (uninits sl)]
  [(uninits •)])

(define-judgment-form vl
  #:mode (lam-verified? I I I)
  [(lam-verified? (lam τl nl e) sl m)
   ;(where n_d (len sl))
   ;(lmax nl n_m)
   ;(n< n_m n_d)
   (where n_d* (n+ (len nl) (len τl)))
   (where sl_0 (collate-refs nl sl))
   (not-lmem uninit sl_0)
   (not-lmem not sl_0)
   (where s_d (drop-noclrs sl_0))
   (extract-self m nl τl s_d f)
   (V e (concat s_d (arg τl)) n_d* #f • • f sl_1 γ_1 η_1)])

(define-judgment-form vl
  #:mode (lam-verified?* I I I)
  [(lam-verified?* • sl m)]
  [(lam-verified?* ((lam τl nl e) el) sl m)
   (vals τl)
   (lam-verified? (lam τl nl e) sl m)
   (lam-verified?* el sl m)])

;; suffers from ?/n confusion
;; fixable by transforming to a metafunction
;; but need to add judgment-holds support 

(define-judgment-form vl
  #:mode (extract-self I I I I O)
  [(extract-self ? nl τl sl ∅)]
  [(extract-self n nl τl sl ∅)
   (not-lmem n nl)]
  [(extract-self n_i (n_i nl) τl sl (O (len τl) sl))
   (not-lmem n_i nl)]
  [(extract-self n_i (n_0 nl) τl sl ((S n_n) n_τ sl))
   (extract-self n_i nl τl sl (n_n n_τ sl))
   (nlmem n_i nl)])

(define-metafunction vl
  [(loc-noclr? (loc-noclr e))
   #t]
  [(loc-noclr? e)
   #f])

(define-relation vl
  [(not-self-app (application e el) s ∅)]
  [(not-self-app (application e el) s f)
   (where #f (loc-noclr? e))]
  [(not-self-app (application (loc-noclr n) el) s (n_f n_s s_f))
   (n≠ n (n+ n_f (len el)))])

(define-relation vl
  [(n≠ O (S n))]
  [(n≠ (S n) O)]
  [(n≠ (S n_1) (S n_2))
   (n≠ n_1 n_2)])

#;
(define-metafunction vl
  [(es ? nl τl sl)
   ∅]
  [(extract-self n nl τl sl)
   ∅
   (judgment-holds (not-lmem n nl))]
  [(extract-self n_i (n_i nl) (τ_0 τl) sl (O O sl))
   (not-lmem n_i nl)]
  [(extract-self n_i (n_0 nl) (τ_0 τl) sl ((S n_n) (S n_τ) sl))
   (extract-self n_i nl τl sl (n_n n_τ sl))
   (nlmem n_i nl)])

(define-metafunction vl
  [(ssblst O O sl)
   •]
  [(ssblst O (S n) (ν sl))
   (ν (ssblst O n sl))]
  [(ssblst (S n_1) n_2 (ν sl))
   (ssblst n_1 n_2 sl)])
  
(define-relation vl
  [(lmem ν (ν sl))]
  [(lmem ν (ν_1 sl))
   (lmem ν sl)])

(define-relation vl
  [(nlmem n (n nl))]
  [(nlmem n (n_1 nl))
   (nlmem n nl)])

(define-relation vl
  [(not-lmem any_1 (any_2 any_3))
   (not-lmem any_1 any_3)
   (where (any_!_4 any_!_4) (any_1 any_2))]
  [(not-lmem any_1 •)])

(define-judgment-form vl
  #:mode (lmax I O)
  [(lmax • O)]
  [(lmax (n nl) n)
   (lmax nl n_m)
   (n< n_m n)]
  [(lmax (n nl) n_m)
   (lmax nl n_m)
   (n< n n_m)])

(define-metafunction vl
  [(sref O (ν sl))
   ν]
  [(sref (S n) (ν_1 sl))
   (sref n sl)]
  [(sref n •)
   #f])

(define-metafunction vl
  #;[(supdt O ν_1 •)
   (ν_1 •)]
  [(supdt O ν_1 (ν_2 sl))
   (ν_1 sl)]
  [(supdt (S n) ν_1 (ν_2 sl))
   (ν_2 (supdt n ν_1 sl))])

(define-metafunction vl
  [(n- n_1 O)
   n_1]
  [(n- (S n_1) (S n_2))
   (n- n_1 n_2)]
  #;
  [(n- O (S n))
   #f])

(define-metafunction vl
  [(n+ O n)
   n]
  [(n+ (S n_1) n_2)
   (n+ n_1 (S n_2))])

(define-metafunction vl
  [(slen •)
   O]
  [(slen (ν sl))
   (S (slen sl))])

(define-metafunction vl
  [(len •)
   O]
  [(len (any_1 any_2))
   (S (len any_2))])

(define-relation vl
  [(n< O (S n))]
  [(n< (S n_1) (S n_2))
   (n< n_1 n_2)])

(define-relation vl
  [(n<= O n)]
  [(n<= (S n_1) (S n_2))
   (n<= n_1 n_2)])
  
(define-relation vl
  [(n= O O)]
  [(n= (S n_1) (S n_2))
   (n= n_1 n_2)])

(define-judgment-form vl
  #:mode (lsplit I I O O)
  [(lsplit O any_1 • any_1)]
  [(lsplit (S n) (any_1 any_2) (any_1 any_3) any_4)
   (lsplit n any_2 any_3 any_4)])
   

(define-metafunction vl
  shift : n f -> f
  [(shift n ∅) ∅]
  [(shift n (n_f n_s sl)) 
   ((n+ n n_f) (n+ n n_s) sl)])

(define-metafunction vl
  abs-push : n ν sl -> sl
  [(abs-push O ν sl) sl]
  [(abs-push (S n) ν sl)
   (abs-push n ν (ν sl))])

;; note: could turn this back into a metafunction
;; if the restriction on relations in term positions
;; is removed....
;; or support judgment-holds, maybe that is better
;; naturally a metafunction in any case...
(define-judgment-form vl
  #:contract (log-noclr n n ν η η)
  #:mode (log-noclr I I I I O)
  [(log-noclr n_p n_l ν_p η ((n- n_p n_l) η))
   (n<= n_l n_p)
   (lmem ν_p (imm (box •)))]
  [(log-noclr n_p n_l ν_p η η)
   (lmem ν_p (imm-nc (box-nc (uninit (not •)))))]
  [(log-noclr n_p n_l ν_p η η)
   (n< n_p n_l)])


(define-metafunction vl
  nc : ν -> ν
  [(nc imm) imm-nc]
  [(nc imm-nc) imm-nc]
  [(nc box) box-nc]
  [(nc box-nc) box-nc])

(define-judgment-form vl
  #:contract (log-clr n s n γ γ)
  #:mode (log-clr I I I I O)
  [(log-clr n_p s n_l γ (((n- (n- (slen s) n_p) (S O)) ν_np) γ))
   (where ν_np (sref n_p s))
   (n<= n_l n_p)]
  [(log-clr n_p s n_l γ γ)
   (n< n_p n_l)])

;; need more specific nt types in
;; relations like this or there can be problems
;; satisfying constraings (i.e. trying to satisfy an sl with anys)
(define-metafunction vl
  [(concat • any_1)
   any_1]
  [(concat (any_1 any_2) any_3)
   (any_1 (concat any_2 any_3))])


(define-judgment-form vl
  #:mode (undo-clrs I I O)
  [(undo-clrs γ invalid invalid)]
  [(undo-clrs • s s)]
  [(undo-clrs ((n ν) γ) s (supdt (n- (n- (slen s) n) (S O)) ν s_2))
   (n< n (slen s))
   (n≠ (n- (slen s) n) O)
   (undo-clrs γ s s_2)]
  [(undo-clrs ((n ν) γ) s s)
   (n<= (slen s) n)])

(define-judgment-form vl
  #:mode (undo-noclrs I I O)
  [(undo-noclrs η invalid invalid)]
  [(undo-noclrs • s s)]
  [(undo-noclrs (n η) s (supdt n imm s_2))
   (where imm-nc (sref n s))
   (undo-noclrs η s s_2)]
  [(undo-noclrs (n η) s (supdt n box s_2))
   (where box-nc (sref n s))
   (undo-noclrs η s s_2)]
  [(undo-noclrs (n η) s s_2)
   (undo-noclrs η s s_2)
   ;; Bug 1
   ;; (lmem (sref n s) (uninit (imm (imm-nc (boc-nc (box (not •)))))))
   (lmem (sref n s) (uninit (imm (box (not •)))))])

(define-judgment-form vl
  #:mode (redo-clrs I I O)
  [(redo-clrs γ invalid invalid)]
  [(redo-clrs • s s)]
  [(redo-clrs ((n ν) γ) s (supdt (n- (n- (slen s) n) (S O)) not s_2))
   (n< n (slen s))
   (n≠ (n- (slen s) n) O)
   (redo-clrs γ s s_2)]
  [(redo-clrs ((n ν) γ) s s)
   (n<= (slen s) n)])

(define-metafunction vl
  [(collate-refs • sl)
   •]
  [(collate-refs (n nl) sl)
   ((sref n sl) (collate-refs nl sl))])

(define-metafunction vl
  [(drop-noclrs (imm-nc sl))
   (imm (drop-noclrs sl))]
  [(drop-noclrs (box-nc sl))
   (box sl)]
  [(drop-noclrs (ν sl))
   (ν (drop-noclrs sl))]
  [(drop-noclrs •)
   •])

;; had to make both of these
;; j-forms for the below reasons...
#;
(define-metafunction vl
  undo-clrs : γ s -> s
  [(undo-clrs γ invalid) invalid]
  [(undo-clrs • s) s]
  [(undo-clrs ((n ν) γ) s)
   (undo-clrs γ (supdt (n- (n- (slen s) n) (S O)) ν s))]
  ;; --> (slen s) < n
  ;; but generation doesn't support this fall-through!!
  [(undo-clrs ((n ν) γ) s)
   (undo-clrs γ s)])
#;
(define-metafunction vl
  redo-clrs : γ s -> s
  [(redo-clrs γ invalid) invalid]
  [(redo-clrs • s) s]
  [(redo-clrs ((n ν) γ) s) 
   (redo-clrs γ (supdt (n- (n- (slen s) n) (S O)) not s))]
  ;; --> (slen s) < n
  ;; but generation doesn't support this fall-through!!
  [(redo-clrs ((n ν) γ) s)
   (redo-clrs γ s)])

(define-metafunction vl
  trim : s s -> s
  [(trim invalid s) invalid]
  [(trim s_1 s_2)
   (sdrp (n- (slen s_1) (slen s_2)) s_1)])

(define-metafunction vl
  [(sdrp O sl)
   sl]
  [(sdrp (S n) (ν sl))
   (sdrp n sl)]
  [(sdrp n •)
   •])

(define-metafunction vl
  [(valid? invalid) #f]
  [(valid? sl) #t])

(define-metafunction vl
  [(arg •)
   •]
  [(arg (val τl))
   (imm (arg τl))]
  [(arg (ref τl))
   (box (arg τl))])


(provide (all-defined-out))