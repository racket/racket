#lang racket

(require redex/reduction-semantics
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution:

(define-metafunction grammar
  [(subst x_1 x_1 e_1) ; shortcut
   e_1]
  [(subst x_1 e_1 (λ (x_2 ... x_1 x_3 ...) e_2))
   (λ (x_2 ... x_1 x_3 ...) e_2)]
  [(subst x_1 x_2 (λ (x_3 ...) e_1)) ; shortcut; x_1 != any x_3
   (λ (x_3 ...) (subst x_1 x_2 e_1))]
  [(subst x_1 e_1 (λ (x_2 ...) e_2)) ; x_1 != any x_2
   ,(term-let ([(x_new ...) (variables-not-in (term (x_1 e_1 e_2)) (term (x_2 ...)))])
              (term (λ (x_new ...) 
                      (subst x_1 e_1 (subst* (x_2 ...) (x_new ...) e_2)))))]
  [(subst x_1 e_1 x_1) e_1]
  [(subst x_1 e x_2) x_2] ; x_1 != x_2, since previous didn't match
  [(subst x_1 e_1 v_1) v_1] ; all other values are atomic
  [(subst x_1 e_1 (list v_1 ...)) (list (subst x_1 e_1 v_1) ...)]
  [(subst x_1 e_1 (e_2 ...))
   ((subst x_1 e_1 e_2) ...)]
  [(subst x_1 e_1 (if e_2 e_3 e_4))
   (if (subst x_1 e_1 e_2) 
       (subst x_1 e_1 e_3)
       (subst x_1 e_1 e_4))]
  [(subst x_1 e_1 (begin e_2 e_3))
   (begin (subst x_1 e_1 e_2) 
          (subst x_1 e_1 e_3))]
  [(subst x_1 e_1 (set! x_2 e_2))
   (set! x_2 (subst x_1 e_1 e_2))]
  [(subst x_1 e_1 (% e_2 e_3 e_4))
   (% (subst x_1 e_1 e_2) 
      (subst x_1 e_1 e_3) 
      (subst x_1 e_1 e_4))]    
  [(subst x_1 e_1 (wcm ((v_1 v_2) ...) e_2))
   (wcm (((subst x_1 e_1 v_1)
          (subst x_1 e_1 v_2)) ...)
        (subst x_1 e_1 e_2))]
  [(subst x_1 e_1 (dw x_2 e_2 e_3 e_4))
   (dw x_2
       (subst x_1 e_1 e_2) 
       (subst x_1 e_1 e_3) 
       (subst x_1 e_1 e_4))])

(define-metafunction grammar
  [(subst* () () e_1) e_1]
  [(subst* (x_1 x_2 ...) (e_1 e_2 ...) e_3)
   (subst* (x_2 ...) (e_2 ...) (subst x_1 e_1 e_3))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other meta-functions:

(define-metafunction grammar
  [(noPrompt v_1 (% v_1 E v)) #f]
  [(noPrompt v_1 (% v_2 E_1 v)) (noPrompt v_1 E_1)]
  [(noPrompt v hole) #t]
  [(noPrompt v_1 (v ... E_1 e ...)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (if E_1 e_2 e_3)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (begin E_1 e_2)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (set! x E_1)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (wcm w E_1)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (dw x e_0 E_1 e_1)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (% v_2 e E_1)) (noPrompt v_1 E_1)]
  [(noPrompt v_1 (% E_1 e_1 e_2)) (noPrompt v_1 E_1)])

(define-metafunction grammar
  [(get-marks-core (in-hole hole hole) v e_2) e_2]
  [(get-marks-core (wcm (name w_1 ((v_4 v_5) ... (v_1 v_3) (v_6 v_7) ...)) E_1) v_1 e_2) (get-marks E_1 v_1 (cons v_3 e_2))]
  [(get-marks-core (wcm w_1 E_1) v_1 e_2) (get-marks E_1 v_1 e_2) (side-condition (term (notInDom v_1 w_1)))]
  [(get-marks-core (v ... E_1 e ...) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (if E_1 e_1 e_3) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (begin E_1 e) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (% v_2 E_1 v_3) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (% v_2 e_1 E_1) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (% E_1 e_1 e_3) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks-core (dw x e_1 E_1 e_3) v_1 e_2) (get-marks E_1 v_1 e_2)])

(define-metafunction grammar
  [(get-marks (if E_1 e e) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks (set! x E_1) v_1 e_2) (get-marks E_1 v_1 e_2)]
  [(get-marks E_1 v_1 e_2) (get-marks-core E_1 v_1 e_2)])

(define-metafunction grammar
  [(expose-wcm e_1) e_1 (side-condition (term (noMatchWCM e_1 (wcm w e))))]
  [(expose-wcm (wcm () e_1)) e_1]
  [(expose-wcm (wcm ((v_1 v_2) (v_3 v_4) ...) e_1))
   (call/cm v_1 v_2 (λ () (expose-wcm (wcm ((v_3 v_4) ...) e_1))))])

(define-metafunction grammar
  [(sameDWs W_1 W_2) (True)]
  [(sameDWs (in-hole W_1 (dw x_1 e_1 E_1 e_2)) 
            (in-hole W_2 (dw x_1 e_1 E_2 e_2)))
   (sameDWs E_1 E_2)]
  [(sameDWs any_1 any_2) (False)])

(define-metafunction grammar
  [(noShared (in-hole W_1 (dw x_1 e_1 E_1 e_2))
             (in-hole W_2 (dw x_1 e_1 E_2 e_2)))
   (False)]
  [(noShared any_1 any_2) (True)])

(define-metafunction grammar
  [(nonWCM (in-hole E (wcm w hole))) #f]
  [(nonWCM any) #t])

;; The rest are helpers that are typeset specially in the paper

(define-metafunction grammar
  [(noMatch E_1 any (% v_1 any_1 any_2)) (noPrompt v_1 E_1)]
  [(noMatch E_1 any (wcm any_1 any_2)) (nonWCM E_1)])

(define-metafunction grammar
  [(noMatchWCM (wcm w e_1) any) #f]
  [(noMatchWCM any_1 any_2) #t])

(define-metafunction grammar
  [(notIn v_1 (v_2 ...)) ,(not (member (term v_1) (term (v_2 ...))))])

(define-metafunction grammar
  [(notInDom v_1 ((v_2  v_3) ...)) (notIn v_1 (v_2 ...))])

(define-metafunction grammar [(True) #t])
(define-metafunction grammar [(False) #f])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide subst subst*
         noPrompt
         get-marks get-marks-core
         expose-wcm
         sameDWs
         noShared
         nonWCM
         noMatch
         noMatchWCM
         notIn
         notInDom)
