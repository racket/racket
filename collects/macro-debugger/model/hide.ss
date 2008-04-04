
#lang scheme/base
(require scheme/match
         scheme/list
         "deriv.ss"
         "deriv-util.ss"
         "synth-engine.ss"
         "synth-derivs.ss"
         "stx-util.ss"
         "context.ss"
         "seek.ss")

(provide hide/policy
         hide*/policy
         macro-policy
         force-letrec-transformation
         current-hiding-warning-handler
         (struct-out hiding-failure)
         (struct-out nonlinearity)
         (struct-out localactions)
         (struct-out hidden-lift-site))

;; hide/policy : WDeriv (identifier -> boolean) -> WDeriv
(define (hide/policy deriv show-macro?)
  (let-values ([(d s) (hide*/policy deriv show-macro?)])
    d))

;; hide*/policy : WDeriv (identifier -> boolean) -> (values WDeriv syntax)
(define (hide*/policy deriv show-macro?)
  (parameterize ((macro-policy show-macro?)
                 (current-seek-processor hide/deriv))
    (hide deriv)))


;                                               
;                                               
;                       ;                       
;                       ;                       
;   ;; ;;;     ;;;    ;;;;;;     ;;;;    ;;;;;  
;    ;;  ;    ;   ;     ;       ;   ;   ;;   ;  
;    ;   ;   ;;   ;;    ;      ;;   ;;  ;;      
;    ;   ;   ;;   ;;    ;      ;;;;;;;   ;;;    
;    ;   ;   ;;   ;;    ;      ;           ;;;; 
;    ;   ;   ;;   ;;    ;      ;;       ;    ;; 
;    ;   ;    ;   ;     ;;      ;;      ;    ;; 
;   ;;; ;;;    ;;;       ;;;     ;;;;   ;;;;;;  
;                                               
;                                               


;; The real goal is to implement a macro-hiding facility that preserves
;; as much of the real module-body and block pass separation as possible.
;; After attempts to do that, I've decided to scale back to a one-pass
;; simplification that should be much easier, get that out for people
;; to use, and then finish the two-pass solution.

;; 2-Pass method of handling blocks
;;   1 Normalize block derivation
;;   2 For each element in the normalized block derivation:
;;     a Recur on the pass1 derivation
;;     b Synth on the pass2 derivation using the pass1 result syntax
;;   3 Recombine into block derivation
;;   ? (Optionally) un-normalize block derivation again
;; Unimplemented

;; Question: which way to match subterms with subderivations
;;   1 Gather subterms into table and recur on subderivations
;;   2 Gather subderivations into table and recur on subterms
;; Benefits of 1:
;;   Preserves order of expansion, even if macro reorders (so effects happen right)
;;   May be easier to deal with marking/renaming
;;   Easier to deal with lifting (lifts get seen in correct order)
;;   Gives finer control over handling of blocks (joining pass1 and pass2 expansions)
;; Drawbacks of 1:
;;   Need to process results more to find final syntax & nonlinear subterms
;; Benefits of 2:
;;   (Already written that way once...)
;;   Nonlinear subterms fairly obvious
;;   Computing final syntax fits naturally into recursion
;; I will try 1.

;                                      
;               ;        ;;;           
;   ;;          ;;         ;           
;    ;                     ;           
;    ;                     ;           
;    ; ;;;   ;;;;       ;;;;     ;;;;  
;    ;;  ;      ;      ;   ;    ;   ;  
;    ;   ;      ;     ;;   ;   ;;   ;; 
;    ;   ;      ;     ;;   ;   ;;;;;;; 
;    ;   ;      ;     ;;   ;   ;       
;    ;   ;      ;     ;;   ;   ;;      
;    ;   ;      ;     ;;  ;;;   ;;     
;   ;;; ;;;  ;;;;;;;   ;;; ;;    ;;;;  
;                                      
;                                      


;; Macro hiding:
;; The derivation is "visible" or "active" by default, 
;; but pieces of it may need to be hidden.

;; hide/deriv : WDeriv -> WDeriv
(define (hide/deriv d)
  (let-values ([(d s) (hide d)])
    d))

;; hide : WDeriv -> (values WDeriv syntax)
(define (hide deriv)
  (for-deriv deriv))

;; for-deriv : WDeriv -> (values WDeriv syntax)
(define (for-deriv d)
  (match d
    ;; Primitives
    [(Wrap p:variable (e1 e2 rs ?1))
     (values d e2)]
    [(Wrap p:module (e1 e2 rs ?1 ?2 tag rename check tag2 ?3 body shift))
     (let ([show-k
            (lambda ()
              (>>Prim d e1 #t (p:module ?2 tag rename check tag2 ?3 body shift)
                      (module name lang . _BODY)
                      (module name lang BODY)
                      ([for-deriv BODY body])))])
       (if (or (show-macro? #'module))
           (show-k)
           (with-handlers ([hiding-failure?
                            (lambda (failure)
                              (handle-hiding-failure d failure)
                              (show-k))])
             (seek/deriv d))))]
    #; ;; OLD CODE
    [(Wrap p:module (e1 e2 rs ?1 #f #f #f body))
     (let ([show-k
            (lambda ()
              (>>Prim d e1 #t (p:module #f #f #f body)
                      (module name lang . _BODY)
                      (module name lang BODY)
                      ([for-deriv BODY body])))])
       (if (or (show-macro? #'module))
           (show-k)
           (with-handlers ([hiding-failure?
                            (lambda (failure)
                              (handle-hiding-failure d failure)
                              (show-k))])
             (seek/deriv d))))]
    #; ;; OLD CODE
    [(Wrap p:module (e1 e2 rs ?1 #t mb ?2 body))
     (let ([show-k
            (lambda ()
              (>>Prim d e1 #t (p:module #t mb ?2 body)
                      (module name lang BODY)
                      (module name lang BODY)
                      ([for-deriv _BODY mb]
                       [for-deriv BODY body])))])
       (if (or (show-macro? #'module))
           (show-k)
           (with-handlers ([hiding-failure?
                            (lambda (failure)
                              (handle-hiding-failure d failure)
                              (show-k))])
             (seek/deriv d))))]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 me pass1 pass2 ?2))
     (let ([lderiv (module-begin->lderiv d)])
       (recv [(lderiv es2) (for-lderiv lderiv)]
             [(d) (lderiv->module-begin lderiv e1 rs)]
             (values d (wderiv-e2 d))))]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
     (>>P d (p:define-syntaxes rhs ?2)
          (define-syntaxes variables RHS)
          ([for-deriv/phase-up RHS rhs]))]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (>>P d (p:define-values rhs)
          (define-values variables RHS)
          ([for-deriv RHS rhs]))]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner))
     (>>P d (p:#%expression inner)
          (#%expression INNER)
          ([for-deriv INNER inner]))]
    [(Wrap p:if (e1 e2 rs ?1 test then else))
     (>>P d (p:if test then else)
          (if TEST THEN ELSE)
          ([for-deriv TEST test]
           [for-deriv THEN then]
           [for-deriv ELSE else]))]
    [(Wrap p:wcm (e1 e2 rs ?1 key mark body))
     (>>P d (p:wcm key mark body)
          (wcm KEY MARK BODY)
          ([for-deriv KEY key]
           [for-deriv MARK mark]
           [for-deriv BODY body]))]
    [(Wrap p:set! (e1 e2 rs ?1 id-resolves rhs))
     (>>P d (p:set! id-resolves rhs)
          (set! id RHS)
          ([for-deriv RHS rhs]))]
    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (>>Pn d (p:set!-macro deriv)
           INNER
           ([for-deriv INNER deriv]))]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (>>P d (p:begin lderiv)
          (begin . LDERIV)
          ([for-lderiv LDERIV lderiv]))]
    [(Wrap p:begin0 (e1 e2 rs ?1 first lderiv))
     (>>P d (p:begin0 first lderiv)
          (begin0 FIRST . LDERIV)
          ([for-deriv FIRST first]
           [for-lderiv LDERIV lderiv]))]
    [(Wrap p:#%app (e1 e2 rs ?1 ld))
     (>>P d (p:#%app ld)
          (#%app . LDERIV)
          ([for-lderiv LDERIV ld]))]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (>>P d (p:lambda renames body)
          (lambda FORMALS . BODY)
          ([for-rename (FORMALS . _B) renames]
           [for-bderiv BODY body]))]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (>>P d (p:case-lambda clauses)
          (case-lambda . ?clauses)
          ([for-case-lambda-clauses ?clauses clauses]))]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (let ([var-renames (map stx-car (stx->list (stx-car renames)))])
       (>>P d (p:let-values renames rhss body)
            (let-values ([VARS RHS] ...) . BODY)
            ([for-renames (VARS ...) var-renames]
             [for-derivs (RHS ...) rhss] 
             [for-bderiv BODY body])))]
    
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (let ([var-renames (if renames (map stx-car (stx->list (stx-car renames))) null)])
       (>>P d (p:letrec-values renames rhss body)
            (letrec-values ([VARS RHS] ...) . BODY)
            ([for-renames (VARS ...) var-renames]
             [for-derivs (RHS ...) rhss]
             [for-bderiv BODY body])))]
    
    [(Wrap p:letrec-syntaxes+values (e1 e2 rs ?1 srenames srhss vrenames vrhss body))
     (let ([svar-renames
            (if srenames (map stx-car (stx->list (stx-car srenames))) null)]
           [vvar-renames
            (if vrenames (map stx-car (stx->list (stx-car vrenames))) null)])
       (>>Pn d (p:letrec-syntaxes+values srenames srhss vrenames vrhss body)
             (letrec-syntaxes+values ([SVARS SRHS] ...) ([VVARS VRHS] ...) . BODY)
             ([for-renames (SVARS ...) svar-renames]
              [for-renames (VVARS ...) vvar-renames]
              [for-bind-syntaxess (SRHS ...) srhss]
              [for-derivs (VRHS ...) vrhss]
              [for-bderiv BODY body])))]
    [(Wrap p:#%datum (e1 e2 rs ?1))
     (let ([show-k (lambda () (values d e2))])
       (if (ormap show-macro? rs)
           (show-k)
           (seek/deriv/on-fail d show-k)))]
    [(Wrap p:#%top (e1 e2 rs ?1))
     (values d e2)]
    [(Wrap p::STOP (e1 e2 rs ?1))
     (values d e2)]
    [(Wrap p:rename (e1 e2 rs ?1 rename inner))
     (>>P d (p:rename rename inner)
          INNER
          ([for-deriv INNER inner]))]

    ;; Macros

    [(Wrap mrule (e1 e2 tx next))
     (let ([show-k
            (lambda ()
              (recv [(tx) (for-transformation tx)]
                    [(next e2) (for-deriv next)]
                    (values (make mrule e1 e2 tx next)
                            e2)))])
       (if (show-transformation? tx)
           (show-k)
           (seek/deriv/on-fail d show-k)))]

    [(Wrap tagrule (e1 e2 tagged-stx next))
     (let ([show-k
            (lambda ()
              (recv [(next e2) (for-deriv next)]
                    (values (make tagrule e1 e2 tagged-stx next)
                            e2)))])
       (if (show-macro? (stx-car tagged-stx))
           (show-k)
           (seek/deriv/on-fail d show-k)))]

    ;; Lift
    ;; Shaky invariant:
    ;; Only lift-exprs occur in first... no lift-end-module-decls
    ;; They occur in reverse order.
    ;; PROBLEM: Hiding process may disturb order lifts are seen.
    [(Wrap lift-deriv (e1 e2 first lifted-stx second))
     ;; Option 2: Hide first, show *all* lifted expressions,
     ;; and hide second (lifted defs only; replace last expr with first-e2)
     (DEBUG-LIFTS
      (printf "lift-deriv:\n~s\n\n" d))
     (DEBUG-LIFTS
      (printf "lift-deriv: lifted-stx\n~s\n\n" (syntax->datum lifted-stx)))
     (let* ([begin-stx (stx-car lifted-stx)]
            [lifted-def-stxs
             ;; lifted-stx has form (begin lift-n ... lift-1 orig-expr)
             (cdr (reverse (stx->list (stx-cdr lifted-stx))))]
            [_ (DEBUG-LIFTS
                (printf "lifted-def-stxs:\n~s\n\n"
                        (syntax->datum #`#,lifted-def-stxs)))]
            [second-derivs
             (match second
               [(Wrap p:begin (_ _ _ ?1 (Wrap lderiv (_ _ ?2 inners))))
                inners])]
            [lift-derivs/0
             (reverse (take-if-possible second-derivs (length lifted-def-stxs)))]
            [_ (DEBUG-LIFTS
                (printf "lift-derivs/0:\n~s\n\n" lift-derivs/0))]
            )
       (define-values (first-d first-e2 lift-derivs)
         ;; Note: lift-derivs are back in reverse order from current-unvisited-lifts
         (parameterize ((current-unvisited-lifts lift-derivs/0)
                        (current-unhidden-lifts null))
           (DEBUG-LIFTS
            (printf "locally setting current-unvisited-lifts: ~s~n"
                    (length lift-derivs/0)))
           (let-values ([(d e2) (for-deriv first)])
             (when (pair? (current-unvisited-lifts))
               (error 'hide:lift-deriv "missed ~s lift-expressions: ~s"
                      (length (current-unvisited-lifts))
                      (current-unvisited-lifts)))
             (values d e2 (current-unhidden-lifts)))))
       (define main-deriv (make p:stop first-e2 first-e2 null #f))
       (define lift-stxs (map wderiv-e1 lift-derivs))
       (define lift-es2 (wderivlist-es2 lift-derivs)) ;; #f if interrupted
       ;; If no lifted syntaxes remain, then simplify:
       (if (null? lift-derivs)
           (values first-d first-e2)
           (let ()
             (define lifted-stx*
               (datum->syntax lifted-stx
                                     `(,begin-stx ,@lift-stxs ,first-e2)
                                     lifted-stx
                                     lifted-stx))
             (define inner-derivs
               ;; If interrupted, then main-expr deriv will not be in list
               (if lift-es2
                   lift-derivs
                   (append lift-derivs (list main-deriv))))
             (define lderiv*
               (make lderiv (map wderiv-e1 inner-derivs)
                     (wderivlist-es2 inner-derivs)
                     #f
                     inner-derivs))
             (define-values (lderiv** es2**) (for-lderiv lderiv*))
             (define e2*
               (and es2**
                    (datum->syntax e2 `(,begin-stx ,@es2**) e2 e2)))
             (define second* 
               (make p:begin lifted-stx* e2* null #f lderiv**))
             (values (make lift-deriv e1 e2* first-d lifted-stx* second*)
                     e2*))))]
    
    [(Wrap lift/let-deriv (e1 e2 first lifted-stx next))
     (warn 'lift/let)
     (recv [(first first-e2)
            (parameterize ((current-unvisited-lifts null)
                           (current-unhidden-lifts null))
              (for-deriv first))]
           [(next next-e2) (for-deriv next)]
           (values (make lift/let-deriv e1 next-e2 first lifted-stx next)
                   next-e2))]
    
    ;; Errors
    
    [#f (values #f #f)]))

;; for-transformation : Transformation -> Transformation
(define (for-transformation tx)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals me2 ?2 _seq))
     (let ([locals (and locals (map for-local-action locals))])
       (make transformation e1 e2 rs ?1 me1 locals me2 ?2 _seq))]))

;; for-local-action : LocalAction -> LocalAction
(define (for-local-action la)
  (match la
    [(struct local-expansion (e1 e2 me1 me2 deriv for-stx? lifted opaque))
     (parameterize ((phase (if for-stx? (add1 (phase)) (phase))))
       (when (or lifted opaque)
         (fprintf (current-error-port)
                  "for-local-action: warning: losing information\n"))
       (let-values ([(deriv e2) (for-deriv deriv)])
         (make local-expansion e1 e2 me1 me2 deriv for-stx? lifted opaque)))]
    [(struct local-lift (expr id))
     (add-unhidden-lift (extract/remove-unvisited-lift id))
     la]
    [(struct local-lift-end (decl))
     (DEBUG-LIFTS
      (printf "hide:for-local-action: local-lift-end unimplemented~n"))
     la]
    [(struct local-bind (names bindrhs))
     (let-values ([(bindrhs e2) (for-bind-syntaxes bindrhs)])
       (make local-bind names bindrhs))]))

;; for-case-lambda-clauses : (list-of CaseLambdaClause) -> (list-of CaseLambdaClause) Stxs
(define (for-case-lambda-clauses clauses)
  (cond [(pair? clauses)
         (match (car clauses)
           [(Wrap clc (?1 renames body))
            (recv [(body* stx*) (for-bderiv body)]
                  [(rest* stxs*) (for-case-lambda-clauses (cdr clauses))]
                  (values (cons (make clc ?1 renames body*) rest*)
                          (with-syntax ([(?formals . ?body) renames]
                                        [?body* stx*])
                            (cons (syntax/skeleton renames (?formals . ?body*))
                                  stxs*))))])]
        [(null? clauses)
         (values null null)]))

;; for-bind-syntaxes : BindSyntaxes -> BindSyntaxes Syntax
(define (for-bind-syntaxes bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (recv [(rhs* stx*)
            (parameterize ((phase (add1 (phase))))
              (for-deriv rhs))]
           (values (make bind-syntaxes rhs* ?1)
                   stx*))]))

;; for-bind-syntaxess : (list-of BindSyntaxes) -> (list-of BindSyntaxes) Syntax
(define (for-bind-syntaxess bindrhss)
  (cond [(pair? bindrhss)
         (recv [(bindrhs* stx*) (for-bind-syntaxes (car bindrhss))]
               [(rest* stxs*) (for-bind-syntaxess (cdr bindrhss))]
               (values (cons bindrhs* rest*)
                       (cons stx* stxs*)))]
        [(null? bindrhss)
         (values null null)]))

;; for-rename : Rename -> (values Rename syntax)
(define (for-rename rename)
  (values rename rename))

;; for-renames : (list-of Rename) -> (values (list-of Rename) syntaxes)
(define (for-renames renames)
  (values renames renames))

;; for-deriv/phase-up : Derivation -> (values Deriv syntax)
(define (for-deriv/phase-up d)
  (parameterize ((phase (add1 (phase))))
    (for-deriv d)))

;; for-derivs : (list-of Deriv) -> (values (list-of Deriv) (list-of syntax))
(define (for-derivs derivs)
  (let ([results
         (map (lambda (d) (recv [(d e2) (for-deriv d)] (cons d e2)))
              derivs)])
    (values (map car results) (map cdr results))))

;; for-derivs/phase-up : (list-of WDeriv) -> (values (list-of WDeriv) (list-of syntax))
(define (for-derivs/phase-up derivs)
  (parameterize ((phase (add1 (phase))))
    (for-derivs derivs)))

;; for-cdr-bderivs : (list-of (cons 'a BlockDerivation))
;;               -> (values (list-of (cons 'a BlockDerivation)) (list-of syntax))
(define (for-cdr-bderivs xs+bderivs)
  (let ([results
         (map (lambda (d)
                (recv [(a b) (for-bderiv (cdr d))]
                      (cons (cons (car d) a) b)))
              xs+bderivs)])
    (values (map car results) (map cdr results))))

;; for-lderiv : ListDerivation -> (values ListDerivation (list-of syntax))
(define (for-lderiv ld)
  (match ld
    [(Wrap lderiv (es1 es2 #f derivs))
     (let-values ([(derivs stxs) (for-derivs derivs)])
       (let ([stxs (and (andmap syntax? stxs) stxs)])
         (values (make lderiv es1 stxs #f derivs)
                 stxs)))]
    [(Wrap lderiv (es1 es2 (and exn? ?1) derivs))
     (values ld es2)]
    [#f (values #f #f)]))

;; 1-pass method of handling blocks
;;   1 Combine pass1 and pass2 into super-pass2
;;   2 Recombine into block derivation with trivial pass1

;; for-bderiv : BlockDerivation -> (values BlockDerivation (list-of syntax))
(define (for-bderiv bd)
  (if (force-letrec-transformation)
      (match bd
        [(Wrap bderiv (es1 es2 pass1 trans pass2))
         (recv [(pass2 es2) (for-lderiv pass2)]
               (values (make bderiv es1 es2 pass1 trans pass2)
                       es2))])
      (match bd
        [(Wrap bderiv (es1 es2 pass1 trans pass2))
         (let ([pass2 (bderiv->lderiv bd)])
           (recv [(pass2 es2) (for-lderiv pass2)]
                 (values (make bderiv es1 es2 null 'list pass2)
                         es2)))]
        [#f (values #f #f)])))

;                                                                 
;                     ;;;;                                        
;   ;;                   ;                                        
;    ;                   ;                                        
;    ;                   ;                                        
;    ; ;;;     ;;;;      ;     ;; ;;;     ;;;;   ;;; ;;;   ;;;;;  
;    ;;  ;    ;   ;      ;     ;;;  ;;   ;   ;    ;;;  ;  ;;   ;  
;    ;   ;   ;;   ;;     ;      ;   ;;  ;;   ;;    ;   ;  ;;      
;    ;   ;   ;;;;;;;     ;      ;   ;;  ;;;;;;;    ;       ;;;    
;    ;   ;   ;           ;      ;   ;;  ;          ;         ;;;; 
;    ;   ;   ;;          ;      ;   ;;  ;;         ;      ;    ;; 
;    ;   ;    ;;         ;      ;   ;    ;;        ;      ;    ;; 
;   ;;; ;;;    ;;;;   ;;;;;;;   ;;;;      ;;;;   ;;;;;;   ;;;;;;  
;                               ;                                 
;                               ;                                 
;                              ;;;;                               
;                                                                 


;; show-macro? : identifier -> boolean
(define (show-macro? id)
  ((macro-policy) id))

;; show-mrule? : MRule -> boolean
(define (show-transformation? tx)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals me2 ?2 _seq))
     (ormap show-macro? rs)]))

