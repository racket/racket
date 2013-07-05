#lang racket

(require redex/pict
         redex/reduction-semantics)


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


(define-language STLC
  (e (λ (x τ) e)
     (e e)
     x
     i
     add1)
  (τ int
     (τ → τ))
  (Γ ([x τ] ...))
  (i integer)
  (x variable-not-otherwise-mentioned))

(define-judgment-form STLC
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)
  [(typeof Γ (λ (x τ_1) e) (τ_1 → τ_2))
   (typeof (extend Γ x τ_1) e τ_2)]
  [(typeof Γ (e_1 e_2) τ)
   (typeof Γ e_1 (τ_2 → τ))
   (typeof Γ e_2 τ_2)]
  [(typeof Γ x τ)
   (where τ (lookup Γ x))]
  [(typeof Γ i int)]
  [(typeof Γ add1 (int → int))])

(define-metafunction STLC
  extend : Γ x τ -> Γ
  [(extend ([x_0 τ_0] ... [x_i τ_i] [x_i+1 τ_i+1] ...) x_i τ)
   ([x_0 τ_0] ... [x_i τ] [x_i+1 τ_i+1] ...)]
  [(extend ([x_1 τ_1] ...) x_0 τ_0)
   ([x_0 τ_0] [x_1 τ_1] ...)])

(define-metafunction STLC
  lookup : Γ x -> τ
  [(lookup ([x_0 τ_0] ... [x_i τ_i] [x_i+1 τ_i+1] ...) x_i) 
   τ_i])

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


(define rewrite-typeof
  (match-lambda
    [(list _ _ Γ e τ _)
     (list "" Γ " ⊢ " e " : " τ)]))
(define rewrite-extend
  (match-lambda
    [(list _ _ Γ x τ _)
     (list "" Γ ", " x ":" τ)]))
(define rewrite-lookup
  (match-lambda
    [(list _ _ Γ x _)
     (list "" Γ "(" x ")")]))

(with-compound-rewriters 
 (['typeof rewrite-typeof]
  ['extend rewrite-extend]
  ['lookup rewrite-lookup])
 (render-judgment-form typeof))
