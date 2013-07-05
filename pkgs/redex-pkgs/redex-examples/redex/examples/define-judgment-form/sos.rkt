#lang racket

(require redex)


;                                                                        
;                                                                        
;                   ;;;    ;             ;             ;                 
;   ;  ;           ;                           ;                         
;   ;   ;   ;;;   ;;;;;  ;;;   ;; ;;   ;;;    ;;;;;  ;;;     ;;;  ;; ;;  
;   ;   ;  ;   ;   ;       ;    ;;  ;    ;     ;       ;    ;   ;  ;;  ; 
;   ;   ;  ;;;;;   ;       ;    ;   ;    ;     ;       ;    ;   ;  ;   ; 
;   ;   ;  ;       ;       ;    ;   ;    ;     ;       ;    ;   ;  ;   ; 
;   ;  ;   ;       ;       ;    ;   ;    ;     ;   ;   ;    ;   ;  ;   ; 
;  ;;;;     ;;;;  ;;;;;  ;;;;; ;;; ;;; ;;;;;    ;;;  ;;;;;   ;;;  ;;; ;;;
;                                                                        
;                                                                        
;                                                                        
;                                                                        


(define-language λv
  (e (e e)
     x
     v)
  (v (λ (x) e)
     i
     add1)
  (i integer)
  (x variable-not-otherwise-mentioned))

(define-judgment-form λv
  #:mode (small-step I O)
  #:contract (small-step e e)
  [(small-step ((λ (x) e) v) (subst e x v))]
  [(small-step (add1 i) (δ add1 i))]
  [(small-step (e_1 e_2) (e_1′ e_2))
   (small-step e_1 e_1′)]
  [(small-step (v e_2) (v e_2′))
   (small-step e_2 e_2′)])

(define-judgment-form λv
  #:mode (small-step* I O)
  #:contract (small-step* e e)
  [(small-step* e e)]
  [(small-step* e_1 e_3)
   (small-step e_1 e_2)
   (small-step* e_2 e_3)])

(define-judgment-form λv
  #:mode (eval I O)
  #:contract (eval e e)
  [(eval e v)
   (small-step* e v)])

(define-metafunction λv
  [(δ add1 i) ,(add1 (term i))])

(define-metafunction λv
  subst : e x v -> e
  [(subst (e_1 e_2) x v)
   ((subst e_1 x v) (subst e_2 x v))]
  [(subst x x v) v]
  [(subst x_1 x_2 v) x_1]
  [(subst (λ (x) e) x v)
   (λ (x) e)]
  [(subst (λ (x_1) e) x_2 v)
   ; capture shmapture...
   (λ (x_1) (subst e x_2 v))]
  [(subst i x v) i]
  [(subst add1 x v) add1])

(test-equal (judgment-holds (eval (add1 (add1 (add1 0))) v) v) 
            (list (term 3)))
(test-equal (judgment-holds 
             (eval (((λ (x) (λ (x) (x (add1 7)))) 0) add1)
                   v)
             v)
            (list (term 9)))


;                                                                               
;                                                                               
;                                                             ;                 
;  ;  ;  ;                                     ;      ;                         
;     ;   ;;; ;;;;; ;;    ;;;    ;;;;   ;;;   ;;;;;  ;;;;;  ;;;   ;; ;;    ;; ;;
;     ;    ;   ;  ;;  ;  ;   ;  ;   ;  ;   ;   ;      ;       ;    ;;  ;  ;  ;; 
;     ;     ;  ;  ;   ;  ;;;;;   ;;;   ;;;;;   ;      ;       ;    ;   ;  ;   ; 
;     ;     ; ;   ;   ;  ;          ;  ;       ;      ;       ;    ;   ;  ;   ; 
;     ;      ;;   ;   ;  ;      ;   ;  ;       ;   ;  ;   ;   ;    ;   ;  ;   ; 
;    ;;;     ;    ;;;;    ;;;;  ;;;;    ;;;;    ;;;    ;;;  ;;;;; ;;; ;;;  ;;;; 
;            ;    ;                                                           ; 
;          ;;;;  ;;;                                                       ;;;  
;                                                                               
;                                                                               


(define rewrite-subst
  (match-lambda
    [(list _ _ e x v _)
     (list "" e "{" x ":=" v "}")]))
(define rewrite-small-step
  (match-lambda
    [(list _ _ e1 e2 _)
     (list "" e1 " → " e2)]))

(with-compound-rewriters
 (['small-step rewrite-small-step]
  ['subst rewrite-subst])
 (render-judgment-form small-step))


;                                                   
;                                                   
;                                 ;                 
;  ;  ;  ;                                          
;     ;    ;; ;;   ;;;    ;;;;  ;;;   ;; ;;    ;; ;;
;     ;     ;;    ;   ;  ;   ;    ;    ;;  ;  ;  ;; 
;     ;     ;      ;;;;  ;        ;    ;   ;  ;   ; 
;     ;     ;     ;   ;  ;        ;    ;   ;  ;   ; 
;     ;     ;     ;   ;  ;   ;    ;    ;   ;  ;   ; 
;    ;;;   ;;;;;   ;;;;;  ;;;   ;;;;; ;;; ;;;  ;;;; 
;                                                 ; 
;                                              ;;;  
;                                                   
;                                                   

;; Relations defined with `define-judgment-form' do 
;; not work directly with `traces', but they can be
;; embedded within reduction relations.
(define small-step-rr
  (reduction-relation
   λv
   (--> e_1 e_2
        (judgment-holds (small-step e_1 e_2)))))

(define (trace-λv expr)
  (traces small-step-rr expr))
