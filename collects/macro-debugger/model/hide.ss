
#lang scheme/base
(require scheme/match
         scheme/list
         "deriv.ss"
         "deriv-util.ss"
         "synth-engine.ss"
         "synth-derivs.ss"
         "stx-util.ss"
         "context.ss")

(provide hide/policy
         macro-policy
         force-letrec-transformation
         current-hiding-warning-handler
         (struct-out hiding-failure)
         (struct-out nonlinearity)
         (struct-out localactions)
         (struct-out hidden-lift-site))

;; hide/policy : WDeriv (identifier -> boolean) -> (values WDeriv syntax)
(define (hide/policy deriv show-macro?)
  (parameterize ((macro-policy show-macro?))
    (hide deriv)))

;; Warnings

(define (handle-hiding-failure d failure)
  (match failure
    [(struct nonlinearity (term paths))
     (warn 'nonlinearity term paths d)]
    [(struct localactions ())
     (warn 'localactions d)]
    [(struct hidden-lift-site ())
     (warn 'hidden-lift-site d)]))

(define-syntax DEBUG-LIFTS
  (syntax-rules ()
    [(DEBUG-LIFTS . b)
     (begin . b)]))

;; current-unvisited-lifts : (paramter-of Derivation)
;; The derivs for the lifts yet to be seen in the processing 
;; of the first part of the current lift-deriv.
(define current-unvisited-lifts (make-parameter null))

;; current-unhidden-lifts : (parameter-of Derivation)
;; The derivs for those lifts that occur within unhidden macros.
;; Derivs are moved from the current-unvisited-lifts to this list.
(define current-unhidden-lifts (make-parameter null))

;; add-unhidden-lift : Derivation -> void
(define (add-unhidden-lift d)
  (when d
    (current-unhidden-lifts
     (cons d (current-unhidden-lifts)))))

;; extract/remove-unvisted-lift : identifier -> Derivation
(define (extract/remove-unvisited-lift id)
  (define (get-defined-id d)
    (match d
      [(Wrap deriv (e1 e2))
       (with-syntax ([(?define-values (?id) ?expr) e1])
         #'?id)]))
  ;; The Wrong Way
  (let ([unvisited (current-unvisited-lifts)])
    (if (null? unvisited)
        (begin (printf "hide:extract/remove-unvisited-lift: out of lifts!")
               #f)
        (let ([lift (car unvisited)])
          (DEBUG-LIFTS
           (printf "extracting lift: ~s left\n" (length (cdr unvisited))))
          (current-unvisited-lifts (cdr unvisited))
          lift)))
  ;; The Right Way
  ;; FIXME: Doesn't work inside of modules. Why not?
  #;
  (let loop ([lifts (current-unvisited-lifts)]
             [prefix null])
    (cond [(null? lifts)
           (DEBUG-LIFTS
            (fprintf (current-error-port)
                     "hide:extract/remove-unvisited-lift: can't find lift for ~s~n"
                     id))
           (raise (make localactions))]
          [(bound-identifier=? id (get-defined-id (car lifts)))
           (let ([lift (car lifts)])
             (current-unvisited-lifts
              (let loop ([prefix prefix] [lifts (cdr lifts)])
                (if (null? prefix)
                    lifts
                    (loop (cdr prefix) (cons (car prefix) lifts)))))
             lift)]
          [else
           (loop (cdr lifts) (cons (car lifts) prefix))])))

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

;; hide : WDeriv -> (values WDeriv syntax)
(define (hide deriv)
  (for-deriv deriv))

;; for-deriv : WDeriv -> (values WDeriv syntax)
(define (for-deriv d)
  (match d
    ;; Primitives
    [(Wrap p:variable (e1 e2 rs ?1))
     (values d e2)]
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
    [(Wrap p:#%module-begin (e1 e2 rs ?1 pass1 pass2 ?2))
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
    [(Wrap p:if (e1 e2 rs ?1 full? test then else))
     (if full?
         (>>P d (p:if full? test then else)
              (if TEST THEN ELSE)
              ([for-deriv TEST test]
               [for-deriv THEN then]
               [for-deriv ELSE else]))
         (>>P d (p:if full? test then else)
              (if TEST THEN)
              ([for-deriv TEST test]
               [for-deriv THEN then])))]
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
    [(Wrap p:#%app (e1 e2 rs ?1 tagged-stx ld))
     (if (or (eq? e1 tagged-stx) (show-macro? #'#%app))
         ;; If explicitly tagged, simple
         (>>Prim d tagged-stx #t (p:#%app tagged-stx ld)
                 (#%app . LDERIV) (#%app . LDERIV)
                 ([for-lderiv LDERIV ld]))
         ;; If implicitly tagged:
         (seek/deriv d))]
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
    [(Wrap p:#%datum (e1 e2 rs ?1 tagged-stx))
     (cond [(or (eq? tagged-stx e1) (show-macro? #'#%datum))
            (values d e2)]
           [else
            (seek/deriv d)])]
    [(Wrap p:#%top (e1 e2 rs ?1 tagged-stx))
     (cond [(or (eq? tagged-stx e1) (show-macro? #'#%top))
            (values d e2)]
           [else
            (seek/deriv d)])]
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
           (with-handlers ([hiding-failure?
                            (lambda (failure)
                              (handle-hiding-failure d failure)
                              (show-k))])
             (seek/deriv d))))]
    
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
    [(Wrap transformation (e1 e2 rs ?1 me1 locals ?2 me2 _seq))
     (let ([locals (and locals (map for-local-action locals))])
       (make transformation e1 e2 rs ?1 me1 locals ?2 me2 _seq))]))

;; for-local-action : LocalAction -> LocalAction
(define (for-local-action la)
  (match la
    [(struct local-expansion (e1 e2 me1 me2 for-stx? deriv))
     (let-values ([(deriv e2) (for-deriv deriv)])
       (make local-expansion e1 e2 me1 me2 for-stx? deriv))]
    [(struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv))
     (error 'hide:for-local-action "not implemented for local-expand-expr")]
    [(struct local-lift (expr id))
     (add-unhidden-lift (extract/remove-unvisited-lift id))
     la]
    [(struct local-lift-end (decl))
     (DEBUG-LIFTS
      (printf "hide:for-local-action: local-lift-end unimplemented~n"))
     la]
    [(struct local-bind (bindrhs))
     (let-values ([(bindrhs e2) (for-bind-syntaxes bindrhs)])
       (make local-bind bindrhs))]))

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
;                              ;;      
;                              ;;      
;                               ;      
;                               ;      
;    ;;;;;     ;;;;     ;;;;    ;  ;;; 
;   ;;   ;    ;   ;    ;   ;    ;  ;   
;   ;;       ;;   ;;  ;;   ;;   ; ;    
;    ;;;     ;;;;;;;  ;;;;;;;   ;;;    
;      ;;;;  ;        ;         ;;;    
;   ;    ;;  ;;       ;;        ; ;;   
;   ;    ;;   ;;       ;;       ;  ;;  
;   ;;;;;;     ;;;;     ;;;;   ;;;  ;;;
;                                      


;; Seek:
;; The derivation is "inactive" or "hidden" by default,
;; but pieces of it can become visible if they correspond to subterms
;; of the hidden syntax.

;; seek/deriv : WDeriv -> (values WDeriv syntax)
;; Seeks for derivations of all proper subterms of the derivation's
;; initial syntax.
(define (seek/deriv d)
  (match d
    [(Wrap deriv (e1 e2))
     (let ([subterms (gather-proper-subterms e1)])
       (parameterize ((subterms-table subterms))
         (let ([sd (seek d)])
           (values sd (wderiv-e2 sd)))))]))

;; seek : WDeriv -> WDeriv
;; Expects macro-policy, subterms-table to be set up already
(define (seek d)
  (match d
    [(Wrap deriv (e1 e2))
     (recv [(subterms hidden-exn) (subterm-derivations d)]
           (begin (check-nonlinear-subterms subterms)
                  ;; Now subterm substitution is safe, because they don't overlap
                  (create-synth-deriv e1 subterms hidden-exn)))]))

;; create-synth-deriv : syntax (list-of Subterm) ?exn -> WDeriv
(define (create-synth-deriv e1 subterms hidden-exn)
  (let ([e2 (if hidden-exn #f (substitute-subterms e1 subterms))])
    (make p:synth e1 e2 null #f subterms hidden-exn)))

;; subterm-derivations : Derivation -> (list-of Subterm) ?exn
(define (subterm-derivations d)
  (subterms-of-deriv d))

;; subterms-of-deriv : Derivation -> (list-of Subterm) ?exn
(define (subterms-of-deriv d)
  (let ([path (check-visible d)])
    (if path
        (let-values ([(d _) (hide d)])
          (SKunit (list (make s:subterm path d))))
        (subterms-of-unlucky-deriv d))))

;; subterms-of-deriv/phase-up : Derivation -> (list-of Subterm) ?exn
(define (subterms-of-deriv/phase-up d)
  (parameterize ((phase (add1 (phase))))
    (subterms-of-deriv d)))

;; check-visible : Derivation -> Path/#f
(define (check-visible d)
  (match d
    [(Wrap deriv (e1 e2))
     (let ([paths (table-get (subterms-table) e1)])
       (cond [(null? paths) #f]
             [(null? (cdr paths))
              (car paths)]
             [else
              ;; More than one path to the same(eq?) syntax object
              ;; Not good.
              ;; FIXME: Better to delay check to here, or check whole table first?
              ;; FIXME
              (raise
               (make nonlinearity e1 paths))]))]
    [#f #f]))

;; subterms-of-unlucky-deriv : Derivation -> (list-of Subterm) ?exn
;; Guarantee: (wderiv-e1 deriv) is not in subterms table
(define (subterms-of-unlucky-deriv d)
  (match d
    ;; Primitives
    [(Wrap p:module (e1 e2 rs ?1 one-body-form? mb ?2 body))
     (cond [one-body-form?
            ;; FIXME: tricky... how to do renaming?
            (>>Seek [! ?1]
                    (subterms-of-deriv mb)
                    [! ?1]
                    (subterms-of-deriv body))]
           [else
            (with-syntax ([(?module ?name ?lang . ?body) e1]
                          [(?module-begin . ?body*) (wderiv-e1 body)])
              (>>Seek [! ?1]
                      [#:rename (do-rename #'?body #'?body*)]
                      [! ?2]
                      (subterms-of-deriv body)))])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 pass1 pass2 ?2))
     (>>Seek [! ?1]
             (subterms-of-lderiv (module-begin->lderiv d))
             [! ?2])]
    [(Wrap p:variable (e1 e2 rs ?1))
     (>>Seek)]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
     (>>Seek [! ?1]
             (subterms-of-deriv/phase-up rhs)
             [! ?2])]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (>>Seek [! ?1]
             (subterms-of-deriv rhs))]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner))
     (>>Seek [! ?1]
             (subterms-of-deriv inner))]
    [(Wrap p:if (e1 e2 rs ?1 full? test then else))
     (>>Seek [! ?1]
             (subterms-of-deriv test)
             (subterms-of-deriv then)
             (if full?
                 (subterms-of-deriv else)
                 (SKzero)))]
    [(Wrap p:wcm (e1 e2 rs ?1 key value body))
     (>>Seek [! ?1]
             (subterms-of-deriv key)
             (subterms-of-deriv value)
             (subterms-of-deriv body))]
    [(Wrap p:set! (e1 e2 rs ?1 id-resolves rhs))
     (>>Seek [! ?1]
             (subterms-of-deriv rhs))]
    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (>>Seek [! ?1]
             (subterms-of-deriv deriv))]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (>>Seek [! ?1]
             (subterms-of-lderiv lderiv))]
    [(Wrap p:begin0 (e1 e2 rs ?1 head lderiv))
     (>>Seek [! ?1]
             (subterms-of-deriv head)
             (subterms-of-lderiv lderiv))]
    [(Wrap p:#%app (e1 e2 rs ?1 tagges-stx lderiv))
     (>>Seek [! ?1]
             (subterms-of-lderiv lderiv))]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (>>Seek [! ?1]
             [#:rename (do-rename/lambda e1 renames)]
             (subterms-of-bderiv body))]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (>>Seek [! ?1]
             (SKmap2 subterms-of-case-lambda-clause
                     clauses
                     (stx->list (stx-cdr e1))))]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/let e1 renames)]
             (SKmap subterms-of-deriv rhss)
             (subterms-of-bderiv body))]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/let e1 renames)]
             (SKmap subterms-of-deriv rhss)
             (subterms-of-bderiv body))]
    [(Wrap p:letrec-syntaxes+values (e1 e2 rs ?1 srenames srhss vrenames vrhss body))
     (>>Seek [! ?1]
             [#:rename (do-rename/lsv1 e1 srenames)]
             (SKmap subterms-of-bind-syntaxes srhss)
             [#:rename (do-rename/lsv2 srenames vrenames)]
             (SKmap subterms-of-deriv vrhss)
             (subterms-of-bderiv body))]
    [(Wrap p::STOP (e1 e2 rs ?1))
     (>>Seek)]
    ;; synth (should synth be idempotent?... heh, no point for now)
    [(Wrap p:rename (e1 e2 rs ?1 rename inner))
     (>>Seek [! ?1]
             [#:rename (do-rename (car rename) (cdr rename))]
             (subterms-of-deriv inner))]
    
    ;; Macros
    
    [(Wrap mrule (e1 e2 tx next))
     (recv [(subterms exn table) (subterms-of-transformation tx)]
           (parameterize ((subterms-table table))
             (SKseq (lambda () (values subterms exn))
                    (lambda () (subterms-of-deriv next)))))]
    
    [(Wrap lift-deriv (e1 e2 first lifted-stx next))
     (raise (make hidden-lift-site))]
    
    [(Wrap lift/let-deriv (e1 e2 first lifted-stx next))
     (raise (make hidden-lift-site))]
    
    ;; Errors
    
    [#f (SKzero)]
    ))

;; subterms-of-transformation : Transformation -> (list-of Subterm) ?exn Table
(define (subterms-of-transformation tx)
  (match tx
    [(Wrap transformation (e1 e2 rs ?1 me1 locals ?2 me2 _seq))
     ;; FIXME: We'll need to use e1/e2/me1/me2 to synth locals, perhaps
     ;; FIXME: and we'll also need to account for *that* marking, too...
     (let ([end-table #f])
       (recv [(ss exn)
              (>>Seek [! ?1]
                      [#:rename/no (do-rename e1 me1)]
                      (SKmap subterms-of-local-action locals)
                      [! ?2]
                      [#:rename/no (do-rename me2 e2)]
                      (begin (set! end-table (subterms-table))
                             (SKzero)))]
             (values ss exn end-table)))]))

;; subterms-of-local-action : LocalAction -> (list-of Subterm) ?exn
(define (subterms-of-local-action local)
  (match local
    [(struct local-expansion (e1 e2 me1 me2 subterms-of-stx? deriv))
     (>>Seek [#:rename/no (do-rename me1 e1)]  ;; FIXME: right order?
             (recv [(subterms exn) (subterms-of-deriv deriv)]
                   (if (pair? (filter s:subterm? subterms))
                       (raise (make localactions))
                       (values subterms exn))))]
    [(struct local-expansion/expr (e1 e2 me1 me2 subterms-of-stx? opaque deriv))
     (>>Seek [#:rename/no (do-rename me1 e1)]  ;; FIXME: right order?
             (recv [(subterms exn) (subterms-of-deriv deriv)]
                   (if (pair? (filter s:subterm? subterms))
                       (raise (make localactions))
                       (values subterms exn))))]
    [(struct local-lift (expr id))
     ;; FIXME: seek in the lifted deriv, transplant subterm expansions *here*
     (extract/remove-unvisited-lift id)]
    [(struct local-lift-end (decl))
     ;; FIXME
     (>>Seek)]
    [(struct local-bind (bindrhs))
     (recv [(subterms exn) (subterms-of-bind-syntaxes bindrhs)]
           (if (pair? (filter s:subterm? subterms))
               (raise (make localactions))
               (values subterms exn)))]))

;; subterms-of-lderiv : ListDerivation -> (list-of Subterm)
(define (subterms-of-lderiv ld)
  (match ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (>>Seek [! ?1]
             (SKmap subterms-of-deriv derivs))]
    [#f (SKzero)]))

;; subterms-of-bderiv : BlockDerivation -> (list-of Subterm)
(define (subterms-of-bderiv bd)
  (subterms-of-lderiv (bderiv->lderiv bd)))

;; subterms-of-case-lambda-clause : Syntax CaseLambdaClause -> (list-of Subterm) ?exn
(define (subterms-of-case-lambda-clause stx clause)
  (match clause
    [(Wrap clc (?1 renames body))
     (>>Seek [! ?1]
             [#:rename (do-rename/case-lambda stx renames)]
             (subterms-of-bderiv body))]))

;; subterms-of-bind-syntaxes : BindSyntaxes -> (list-of Subterm) ?exn
(define (subterms-of-bind-syntaxes bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (>>Seek (subterms-of-deriv rhs)
             [! ?1])]))


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
    [(Wrap transformation (e1 e2 rs ?1 me1 locals ?2 me2 _seq))
     (ormap show-macro? rs)]))

;; gather-one-subterm : syntax syntax -> SubtermTable
(define (gather-one-subterm whole part)
  (let ([table (make-hash-table)])
    (let ([paths (find-subterm-paths part whole)])
      (for-each (lambda (p) (table-add! table part p)) paths))
    table))

;; gather-proper-subterms : Syntax -> SubtermTable
;; FIXME: Eventually, need to descend into vectors, boxes, etc.
(define (gather-proper-subterms stx0)
  (let ([table (make-hash-table)])
    ;; loop : Syntax Path -> void
    (define (loop stx rpath)
      (unless (eq? stx0 stx)
        (table-add! table stx (reverse rpath)))
      (let ([p (syntax-e stx)])
        (when (pair? p)
          (loop-cons p rpath 0))))
    ;; loop-cons : (cons Syntax ?) Path number -> void
    (define (loop-cons p rpath pos)
      (loop (car p) (cons (make ref pos) rpath))
      (let ([t (cdr p)])
        (cond [(syntax? t)
               (let ([te (syntax-e t)])
                 (if (pair? te)
                     (begin
                       (table-add! table t (reverse (cons (make tail pos) rpath)))
                       (loop-cons te rpath (add1 pos)))
                     (loop t (cons (make tail pos) rpath))))]
              [(pair? t)
               (loop-cons t rpath (add1 pos))]
              [(null? t)
               (void)])))
    (loop stx0 null)
    table))

(define (map/2values f items)
  (if (null? items)
      (values null null)
      (let*-values ([(a0 b0) (f (car items))]
                    [(as bs) (map/2values f (cdr items))])
        (values (cons a0 as) (cons b0 bs)))))



;                                               
;                              ;;;;             
;                     ;;          ;             
;     ;                ;          ;             
;     ;                ;          ;             
;   ;;;;;;    ;;;;;    ; ;;;      ;       ;;;;  
;     ;       ;   ;    ;;  ;;     ;      ;   ;  
;     ;           ;    ;   ;;     ;     ;;   ;; 
;     ;        ;;;;    ;   ;;     ;     ;;;;;;; 
;     ;      ;;   ;    ;   ;;     ;     ;       
;     ;      ;;   ;    ;   ;;     ;     ;;      
;     ;;     ;;  ;;    ;   ;      ;      ;;     
;      ;;;    ;;; ;;   ;;;;    ;;;;;;;    ;;;;  
;                                               
;                                               
;                                               


;; A Table is a hashtable[syntax => (list-of Path)
(define (table-add! table stx v)
  (hash-table-put! table stx (cons v (table-get table stx))))
(define (table-add-if-absent! table stx v)
  (unless (memq v (table-get table stx))
    (table-add! table stx v)))
(define (table-get table stx)
  (hash-table-get table stx (lambda () null)))

;; do-rename : syntax syntax -> (values (list-of Subterm) Table)
(define (do-rename stx rename)
  (let ([t (make-hash-table)]
        [old (subterms-table)])
    ;; loop : syntax syntax -> (list-of Subterm)
    ;; Puts things into the new table, too
    ;; If active? is #f, always returns null
    (define (loop stx rename active?)
      (cond [(and (syntax? stx) (syntax? rename))
             (let ([paths (table-get old stx)])
               (if (pair? paths)
                   (begin (hash-table-put! t rename paths)
                          (loop (syntax-e stx) (syntax-e rename) #f)
                          (if active?
                              (map (lambda (p) (make s:rename p stx rename))
                                   paths)
                              null))
                   (loop (syntax-e stx) (syntax-e rename) active?)))]
            [(syntax? rename)
             (loop stx (syntax-e rename) active?)]
            [(syntax? stx)
             (loop (syntax-e stx) rename active?)]
            [(and (pair? stx) (pair? rename))
             (append
              (loop (car stx) (car rename) active?)
              (loop (cdr stx) (cdr rename) active?))]
            [else
             null]))
    (let ([subterms (loop stx rename #t)])
      (values subterms t))))

(define (do-rename/lambda stx rename)
  (if rename
      (with-syntax ([(?lambda ?formals . ?body) stx])
        (do-rename (cons #'?formals #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/let stx rename)
  (if rename 
      (with-syntax ([(?let ?bindings . ?body) stx])
        (do-rename (cons #'?bindings #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/case-lambda stx rename)
  (if rename
      (with-syntax ([(?formals . ?body) stx])
        (do-rename (cons #'?formals #'?body) rename))
      (values null (subterms-table))))

(define (do-rename/lsv1 stx rename) 
  (if rename
      (with-syntax ([(?lsv ?sbindings ?vbindings . ?body) stx])
        (do-rename (cons #'?sbindings (cons #'?vbindings #'?body)) rename))
      (values null (subterms-table))))

(define (do-rename/lsv2 old-rename rename)
  (if rename
      (with-syntax ([(?sbindings ?vbindings . ?body) old-rename])
        (do-rename (cons #'?vbindings #'?body) rename))
      (values null (subterms-table))))
