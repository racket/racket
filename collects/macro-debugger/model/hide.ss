
(module hide mzscheme
  (require (lib "plt-match.ss")
           (lib "list.ss")
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
           (struct nonlinearity (message paths))
           (struct localactions ()))

  ;; hide/policy : Derivation (identifier -> boolean) -> (values Derivation syntax)
  (define (hide/policy deriv show-macro?)
    (parameterize ((macro-policy show-macro?))
      (hide deriv)))

  ;; current-hiding-warning-handler : (parameter-of (symbol string -> void))
  (define current-hiding-warning-handler
    (make-parameter
     (lambda (tag message) (printf "~a: ~a~n" tag message))))

  (define (warn tag message) ((current-hiding-warning-handler) tag message))


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
    (current-unhidden-lifts (cons d (current-unhidden-lifts))))

  ;; extract/remove-unvisted-lift : identifier -> Derivation
  (define (extract/remove-unvisited-lift id)
    (define (get-defined-id d)
      (match d
        [(AnyQ deriv (e1 e2))
         (with-syntax ([(?define-values (?id) ?expr) e1])
           #'?id)]))
    ;; The Wrong Way
    (let ([unvisited (current-unvisited-lifts)])
      (unless (pair? unvisited)
        (error 'hide:extract/remove-unvisited-lift 
               "out of lifts!"))
      (let ([lift (car unvisited)])
        (current-unvisited-lifts (cdr unvisited))
        lift))
    ;; The Right Way
    ;; FIXME: Doesn't work inside of modules. Why not?
    #;
    (let loop ([lifts (current-unvisited-lifts)]
               [prefix null])
      (cond [(null? lifts)
             #;(fprintf (current-error-port)
                      "hide:extract/remove-unvisited-lift: couldn't find lift for ~s~n"
                      id)
             (raise (make-localactions))]
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

  ;; hide : Derivation -> (values Derivation syntax)
  (define (hide deriv)

    ;; for-deriv : Derivation -> (values Derivation syntax)
    (define (for-deriv d)
      (match d

        ;; Primitives

        [(AnyQ p:variable (e1 e2 rs))
         (values d e2)]
        [(AnyQ p:module (e1 e2 rs single-body-form? body))
         ;; FIXME: Find the appropriate module-begin identifier to test
             ;; otherwise, hide module, seek for body elements...
         (let ([show-k
                (lambda ()
                  (>>Prim d e1 #t (make-p:module single-body-form? body)
                          (module name lang . _BODY)
                          (module name lang BODY)
                          ([for-deriv BODY body])))])
           (if (or single-body-form? (show-macro? #'#%plain-module-begin))
               (show-k)
               (with-handlers ([nonlinearity?
                                (lambda (nl)
                                  (warn 'nonlinearity
                                        (format "~a: ~s"
                                                (nonlinearity-message nl)
                                                (nonlinearity-paths nl)))
                                  (show-k))]
                               [localactions?
                                (lambda (nl)
                                  (warn 'localactions
                                        "opaque macro called local-expand or lifted expression")
                                  (show-k))])
                 (seek/deriv d))))]
        [(struct p:#%module-begin (e1 e2 rs pass1 pass2))
         ;; FIXME: hide tagging
         (let ([lderiv (module-begin->lderiv d)])
           (recv [(lderiv es2) (for-lderiv lderiv)]
                 [(d) (lderiv->module-begin lderiv e1)]
                 (values d (deriv-e2 d))))]
        [(AnyQ p:define-syntaxes (e1 e2 rs rhs))
         (>>P d (make-p:define-syntaxes rhs)
              (define-syntaxes variables RHS)
              ([for-deriv/phase-up RHS rhs]))]
        [(AnyQ p:define-values (e1 e2 rs rhs))
         (>>P d (make-p:define-values rhs)
              (define-values variables RHS)
              ([for-deriv RHS rhs]))]
        [(AnyQ p:expression (e1 e2 rs inner))
         (>>P d (make-p:expression inner)
              (#%expression INNER)
              ([for-deriv INNER inner]))]
        [(AnyQ p:if (e1 e2 rs full? test then else))
         (if full?
             (>>P d (make-p:if full? test then else)
                  (if TEST THEN ELSE)
                  ([for-deriv TEST test]
                   [for-deriv THEN then]
                   [for-deriv ELSE else]))
             (>>P d (make-p:if full? test then else)
                  (if TEST THEN)
                  ([for-deriv TEST test]
                   [for-deriv THEN then])))]
        [(AnyQ p:wcm (e1 e2 rs key mark body))
         (>>P d (make-p:wcm key mark body)
              (wcm KEY MARK BODY)
              ([for-deriv KEY key]
               [for-deriv MARK mark]
               [for-deriv BODY body]))]
        [(AnyQ p:set! (e1 e2 rs id-resolves rhs))
         (>>P d (make-p:set! id-resolves rhs)
              (set! id RHS)
              ([for-deriv RHS rhs]))]
        ;; FIXME: Correct?
        [(AnyQ p:set!-macro (e1 e2 rs deriv))
         (>>Pn d (make-p:set!-macro deriv)
               INNER
               ([for-deriv INNER deriv]))]
        [(AnyQ p:begin (e1 e2 rs lderiv))
         (>>P d (make-p:begin lderiv)
              (begin . LDERIV)
              ([for-lderiv LDERIV lderiv]))]
        [(AnyQ p:begin0 (e1 e2 rs first lderiv))
         (>>P d (make-p:begin0 first lderiv)
              (begin0 FIRST . LDERIV)
              ([for-deriv FIRST first]
               [for-lderiv LDERIV lderiv]))]
        [(AnyQ p:#%app (e1 e2 rs tagged-stx ld))
         (if (or (eq? e1 tagged-stx) (show-macro? #'#%app))
             ;; If explicitly tagged, simple
             (>>Prim d tagged-stx #t (make-p:#%app tagged-stx ld)
                  (#%app . LDERIV) (#%app . LDERIV)
                  ([for-lderiv LDERIV ld]))
             ;; If implicitly tagged:
             (let-values ([(ld* es2*) (for-lderiv ld)])
               (match ld*
                 [(IntQ lderiv (es1 es2 derivs*))
                  (let ([stx* (and e2 es2 (datum->syntax-object e2 es2 e2 e2))])
                    (values
                     (rewrap d (make-p:synth e1 stx* rs
                                             (map (lambda (n d)
                                                    (make-s:subterm (list (make-ref n)) d))
                                                  (iota (length derivs*))
                                                  derivs*)))
                     stx*))]
                 [(struct error-wrap (exn _ _))
                  (values (make-error-wrap exn #f (make-p:synth e1 #f rs null))
                          #f)])))]
        [(AnyQ p:lambda (e1 e2 rs renames body))
         (>>P d (make-p:lambda renames body)
              (lambda FORMALS . BODY)
              ([for-rename (FORMALS . _B) renames]
               [for-bderiv BODY body]))]

        [(AnyQ p:case-lambda (e1 e2 rs renames+bodies))
         (let ([var-renames (map caar renames+bodies)])
           (>>P d (make-p:case-lambda renames+bodies)
                (case-lambda [FORMALS . BODY] ...)
                ([for-renames (FORMALS ...) var-renames]
                 [for-cdr-bderivs (BODY ...) renames+bodies])))]

        [(AnyQ p:let-values (e1 e2 rs renames rhss body))
         (let ([var-renames (map stx-car (stx->list (stx-car renames)))])
           (>>P d (make-p:let-values renames rhss body)
                (let-values ([VARS RHS] ...) . BODY)
                ([for-renames (VARS ...) var-renames]
                 [for-derivs (RHS ...) rhss] 
                 [for-bderiv BODY body])))]

        [(AnyQ p:letrec-values (e1 e2 rs renames rhss body))
         (let ([var-renames (if renames (map stx-car (stx->list (stx-car renames))) null)])
           (>>P d (make-p:letrec-values renames rhss body)
                (letrec-values ([VARS RHS] ...) . BODY)
                ([for-renames (VARS ...) var-renames]
                 [for-derivs (RHS ...) rhss]
                 [for-bderiv BODY body])))]

        [(AnyQ p:letrec-syntaxes+values (e1 e2 rs srenames srhss vrenames vrhss body))
         (let ([svar-renames (if srenames (map stx-car (stx->list (stx-car srenames))) null)]
               [vvar-renames (if vrenames (map stx-car (stx->list (stx-car vrenames))) null)])
           (>>Pn d (make-p:letrec-syntaxes+values srenames srhss vrenames vrhss body)
                 (letrec-syntaxes+values ([SVARS SRHS] ...) ([VVARS VRHS] ...) . BODY)
                 ([for-renames (SVARS ...) svar-renames]
                  [for-renames (VVARS ...) vvar-renames]
                  [for-derivs/phase-up (SRHS ...) srhss]
                  [for-derivs (VRHS ...) vrhss]
                  [for-bderiv BODY body])))]
        [(AnyQ p:#%datum (e1 e2 rs tagged-stx))
         (cond [(or (eq? tagged-stx e1) (show-macro? #'#%datum))
                (values d e2)]
               [else
                ;; hide the #%datum tagging
                ;; Is this the right way?
                (values (make-p:synth e1 e1 rs null)
                        e1)])]
        [(AnyQ p:#%top (e1 e2 rs tagged-stx))
         (cond [(or (eq? tagged-stx e1) (show-macro? #'#%top))
                (values d e2)]
               [else
                ;; hide the #%top tagging
                (values (rewrap d (make-p:synth e1 e1 rs null))
                        e1)])]
        [(AnyQ p::STOP (e1 e2 rs))
         (values d e2)]

        [(AnyQ p:rename (e1 e2 rs rename inner))
         (>>P d (make-p:rename rename inner)
              INNER
              ([for-deriv INNER inner]))]

        ;; Macros

        [(AnyQ mrule (e1 e2 tx next))
         (let ([show-k
                (lambda ()
                  (recv [(tx) (for-transformation tx)]
                        [(next e2) (for-deriv next)]
                        (values (rewrap d (make-mrule e1 e2 tx next))
                                e2)))])
           (if (show-transformation? tx)
               (show-k)
               (with-handlers ([nonlinearity?
                                (lambda (nl)
                                  (warn 'nonlinearity
                                        (format "~a: ~s"
                                                (nonlinearity-message nl)
                                                (nonlinearity-paths nl)))
                                  (show-k))]
                               [localactions?
                                (lambda (nl)
                                  (warn 'localactions
                                        "opaque macro called local-expand or lifted expression")
                                  (show-k))])
                 (seek/deriv d))))]

        ;; Lift
        ;; Shaky invariant:
        ;; Only lift-exprs occur in first... no lift-end-module-decls
        ;; They occur in reverse order.
        ;; PROBLEM: Hiding process may disturb order lifts are seen.
        [(IntQ lift-deriv (e1 e2 first lifted-stx second) tag)
         ;; Option 2: Hide first, show *all* lifted expressions,
         ;; and hide second (lifted defs only; replace last expr with first-e2)
         (let* ([second-derivs
                 (match second
                   [(IntQ p:begin (_ _ _ (IntQ lderiv (_ _ inners))))
                    (reverse inners)])]
                [lift-derivs/0
                 ;; If interrupted, then main-expr deriv will not be in list
                 ;; second-derivs are already reversed
                 (if tag second-derivs (cdr second-derivs))]
                [begin-stx (stx-car lifted-stx)])
           (define-values (first-d first-e2 lift-derivs)
             ;; Note: lift-derivs are back in reverse order from current-unvisited-lifts
             (parameterize ((current-unvisited-lifts lift-derivs/0)
                            (current-unhidden-lifts null))
               #;(printf "setting current-unvisited-lifts: ~s~n" (length lift-derivs/0))
               (let-values ([(d e2) (for-deriv first)])
                 (when (pair? (current-unvisited-lifts))
                   (error 'hide:lift-deriv "missed ~s lift-expressions: ~s"
                          (length (current-unvisited-lifts))
                          (current-unvisited-lifts)))
                 (values d e2 (current-unhidden-lifts)))))
           (define lift-stxs (map lift/deriv-e1 lift-derivs))
           (define main-deriv (make-p:stop first-e2 first-e2 null))
           ;; If no lifted syntaxes remain, then simplify:
           (if (null? lift-derivs)
               (values first-d first-e2)
               (let ()
                 (define lifted-stx*
                   (datum->syntax-object lifted-stx
                                         `(,begin-stx ,@lift-stxs ,first-e2)
                                         lifted-stx
                                         lifted-stx))
                 (define inner-derivs 
                   ;; If interrupted, then main-expr deriv will not be in list
                   (if tag lift-derivs (append lift-derivs (list main-deriv))))
                 (define lderiv*
                   (rewrap second
                           (make-lderiv (map lift/deriv-e1 inner-derivs)
                                        (and (not tag)
                                             (map lift/deriv-e2 inner-derivs))
                                        inner-derivs)))
                 (define-values (lderiv** es2**) (for-lderiv lderiv*))
                 (define e2*
                   (and es2**
                        (datum->syntax-object e2 `(,begin-stx ,@es2**) e2 e2)))
                 (define second* 
                   (rewrap second (make-p:begin lifted-stx* e2* null lderiv**)))
                 (values (rewrap d (make-lift-deriv e1 e2* first-d lifted-stx* second*))
                         e2*))))]

        [(AnyQ lift/let-deriv (e1 e2 first lifted-stx next))
         ;; FIXME
         (error 'hide "lift/let unimplemented")]

        ;; Errors

        [#f (values #f #f)]))

    ;; for-transformation : Transformation -> Transformation???
    (define (for-transformation tx)
      (match tx
        [(AnyQ transformation (e1 e2 rs me1 me2 locals _seq))
         (let ([locals (map for-local-action (or locals null))])
           (rewrap tx (make-transformation e1 e2 rs me1 me2 locals _seq)))]))
    
    ;; for-local-action : LocalAction -> LocalAction
    (define (for-local-action la)
      (match la
        [(struct local-expansion (e1 e2 me1 me2 for-stx? deriv))
         (let-values ([(deriv e2) (for-deriv deriv)])
           (make-local-expansion e1 e2 me1 me2 for-stx? deriv))]
        [(struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv))
         (error 'hide:for-local-action "not implemented for local-expand-expr")]
        [(struct local-lift (expr id))
         (add-unhidden-lift (extract/remove-unvisited-lift id))
         la]
        [(struct local-lift-end (decl))
         ;;(printf "hide:for-local-action: local-lift-end unimplemented~n")
         la]
        [(struct local-bind (deriv))
         (let-values ([(deriv e2) (for-deriv deriv)])
           (make-local-bind deriv))]))

    ;; for-rename : Rename -> (values Rename syntax)
    (define (for-rename rename)
      (values rename rename))

    ;; for-renames : (list-of Rename) -> (values (list-of Rename) syntaxes)
    (define (for-renames renames)
      (values renames renames))

    ;; for-deriv/phase-up : Derivation -> (values Derivation syntax)
    (define (for-deriv/phase-up d)
      (parameterize ((phase (add1 (phase))))
        (for-deriv d)))

    ;; for-derivs : (list-of Derivation) -> (values (list-of Derivation) (list-of syntax))
    (define (for-derivs derivs)
      (let ([results 
             (map (lambda (d) (let-values ([(a b) (for-deriv d)]) (cons a b)))
                  derivs)])
        (values (map car results) (map cdr results))))

    ;; for-derivs/phase-up : (list-of Derivation) -> (values (list-of Derivation) (list-of syntax))
    (define (for-derivs/phase-up derivs)
      (parameterize ((phase (add1 (phase))))
        (for-derivs derivs)))

    ;; for-cdr-bderivs : (list-of (cons 'a BlockDerivation))
    ;;               -> (values (list-of (cons 'a BlockDerivation)) (list-of syntax))
    (define (for-cdr-bderivs xs+bderivs)
      (let ([results
             (map (lambda (d) (recv [(a b) (for-bderiv (cdr d))] (cons (cons (car d) a) b)))
                  xs+bderivs)])
        (values (map car results) (map cdr results))))

    ;; for-lderiv : ListDerivation -> (values ListDerivation (list-of syntax))
    (define (for-lderiv ld)
      (match ld
        [(AnyQ lderiv (es1 es2 derivs))
         (let-values ([(derivs stxs) (for-derivs derivs)])
           (let ([stxs (if (wrapped? ld) #f stxs)])
             (values (rewrap ld (make-lderiv es1 stxs derivs))
                     stxs)))]
        [#f (values #f #f)]))

    ;; 1-pass method of handling blocks
    ;;   1 Combine pass1 and pass2 into super-pass2
    ;;   2 Recombine into block derivation with trivial pass1

    ;; for-bderiv : BlockDerivation -> (values BlockDerivation (list-of syntax))
    (define (for-bderiv bd)
      (if (force-letrec-transformation)
          (match bd
            [(IntQ bderiv (es1 es2 pass1 trans pass2))
             (recv [(pass2 es2) (for-lderiv pass2)]
                   (values (rewrap/nt bd (make-bderiv es1 es2 pass1 trans pass2))
                           es2))])
          (match bd
            [(IntQ bderiv (es1 es2 pass1 trans pass2))
             (let ([pass2 (bderiv->lderiv bd)])
               (recv [(pass2 es2) (for-lderiv pass2)]
                     (values (rewrap/nt bd (make-bderiv es1 es2 null 'list pass2))
                             es2)))]
            [#f (values #f #f)])))

    (for-deriv deriv))


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

  ;; seek/deriv : Derivation -> (values Derivation syntax)
  ;; Seeks for derivations of all proper subterms of the derivation's
  ;; initial syntax.
  (define (seek/deriv d)
    (match d
      [(AnyQ deriv (e1 e2))
       (let ([subterms (gather-proper-subterms e1)])
         (parameterize ((subterms-table subterms))
           (match (seek d)
             [(and (struct error-wrap (exn tag inner)) ew)
              (values ew #f)
              #;(values ew (deriv-e2 inner))]
             [deriv
              (values (rewrap d deriv) (lift/deriv-e2 deriv))])))]))
  
  ;; seek : Derivation -> Derivation
  ;; Expects macro-policy, subterms-table to be set up already
  (define (seek d)
    (match d
      [(AnyQ deriv (e1 e2))
       (let ([subterm-derivs (subterm-derivations d)])
         (check-nonlinear-subterms subterm-derivs)
         ;; Now subterm substitution is safe, because they don't overlap
         (create-synth-deriv e1 subterm-derivs))]))

  ;; create-synth-deriv : syntax (list-of Subterm) -> Derivation
  (define (create-synth-deriv e1 subterm-derivs)
    (define (error? x)
      (and (s:subterm? x) 
           (error-wrap? (s:subterm-deriv x))
           (not (s:subterm-path x))))
    (define (interrupted? x)
      (and (s:subterm? x)
           (interrupted-wrap? (s:subterm-deriv x))))
    (let* ([errors (map s:subterm-deriv (filter error? subterm-derivs))]
           [subterms (filter (lambda (x) (not (error? x))) subterm-derivs)]
           [interrupted (filter interrupted? subterms)])
      (let ([e2 (and (null? errors)
                     (null? interrupted)
                     (substitute-subterms e1 subterms))])
        (let ([d (make-p:synth e1 e2 null subterms)]
              [wrap (cond [(pair? errors) (car errors)]
                          [(pair? interrupted) (car interrupted)]
                          [else #f])])
          (if wrap (rewrap wrap d) d)))))

  ;; subterm-derivations : Derivation -> (list-of Subterm)
  (define (subterm-derivations d)

    ;; for-deriv : Derivation -> (list-of Subterm)
    (define (for-deriv d)
      (let ([path (check-visible d)])
        (if path
            (let-values ([(d _) (hide d)])
              (list (make-s:subterm path d)))
            (for-unlucky-deriv/record-error d))))
    
    ;; for-deriv/phase-up : Derivation -> (list-of Subterm)
    (define (for-deriv/phase-up d)
      (parameterize ((phase (add1 (phase))))
        (for-deriv d)))

    ;; check-visible : Derivation -> Path/#f
    (define (check-visible d)
      (match d
        [(AnyQ deriv (e1 e2))
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
                   (make-nonlinearity
                    "nonlinearity in original term" paths))]))]
        [#f #f]))

    ;; for-unlucky-deriv/record-error -> (list-of Subterm)
    ;; Guarantee: (deriv-e1 deriv) is not in subterms table
    (define (for-unlucky-deriv/record-error d)
      (if (error-wrap? d)
          (append (for-unlucky-deriv d)
                  (list (make-s:subterm #f d)))
          (for-unlucky-deriv d)))
    
    ;; for-unlucky-deriv : Derivation -> (list-of Subterm)
    ;; Guarantee: (deriv-e1 deriv) is not in subterms table
    (define (for-unlucky-deriv d)
      (match d

        ;; Primitives
        [(AnyQ p:module (e1 e2 rs one-body-form? body))
         (cond [one-body-form?
                ;; FIXME: tricky... how to do renaming?
                (for-deriv body)]
               [else
                (with-syntax ([(?module ?name ?lang . ?body) e1]
                              [(?module-begin . ?body*) (lift/deriv-e1 body)])
                  (>>Seek [#:rename (do-rename #'?body #'?body*)]
                          (for-deriv body)))])]
        [(AnyQ p:#%module-begin (e1 e2 rs pass1 pass2))
         (let ([lderiv (module-begin->lderiv d)])
           (for-lderiv lderiv))]
        [(AnyQ p:variable (e1 e2 rs))
         null]
        [(AnyQ p:define-syntaxes (e1 e2 rs rhs))
         (>>Seek (for-deriv/phase-up rhs))]
        [(AnyQ p:define-values (e1 e2 rs rhs))
         (>>Seek (for-deriv rhs))]
        [(AnyQ p:expression (e1 e2 rs inner))
         (>>Seek (for-deriv inner))]
        [(AnyQ p:if (e1 e2 rs full? test then else))
         (>>Seek (for-deriv test)
                 (for-deriv then)
                 (if full?
                     (for-deriv else)
                     null))]
        [(AnyQ p:wcm (e1 e2 rs key value body))
         (>>Seek (for-deriv key)
                 (for-deriv value)
                 (for-deriv body))]
        [(AnyQ p:set! (e1 e2 rs id-resolves rhs))
         (>>Seek (for-deriv rhs))]
        [(AnyQ p:set!-macro (e1 e2 rs deriv))
         (>>Seek (for-deriv deriv))]
        [(AnyQ p:begin (e1 e2 rs lderiv))
         (>>Seek (for-lderiv lderiv))]
        [(AnyQ p:begin0 (e1 e2 rs head lderiv))
         (>>Seek (for-deriv head)
                 (for-lderiv lderiv))]
        [(AnyQ p:#%app (e1 e2 rs tagges-stx lderiv))
         (>>Seek (for-lderiv lderiv))]
        [(AnyQ p:lambda (e1 e2 rs renames body) exni)
         (>>Seek [#:rename (do-rename/lambda e1 renames)]
                 (for-bderiv body))]
        [(AnyQ p:case-lambda (e1 e2 rs renames+bodies))
         (with-syntax ([(?case-lambda ?clause ...) e1])
           (let ()
             (define (handle-clause clause-stx rename body)
               (>>Seek [#:rename (do-rename/case-lambda clause-stx rename)]
                       (for-bderiv body)))
             (let loop ([clauses (syntax->list #'(?clause ...))]
                        [renames+bodies renames+bodies])
               (if (pair? renames+bodies)
                   (append (handle-clause (car clauses)
                                          (caar renames+bodies)
                                          (cdar renames+bodies))
                           (loop (cdr clauses) (cdr renames+bodies)))
                   null))))]
        [(AnyQ p:let-values (e1 e2 rs renames rhss body))
         (>>Seek [#:rename (do-rename/let e1 renames)]
                 [#:append (map for-deriv rhss)]
                 (for-bderiv body))]
        [(AnyQ p:letrec-values (e1 e2 rs renames rhss body))
         (>>Seek [#:rename (do-rename/let e1 renames)]
                 [#:append (map for-deriv rhss)]
                 (for-bderiv body))]
        [(AnyQ p:letrec-syntaxes+values (e1 e2 rs srenames srhss vrenames vrhss body))
         (>>Seek [#:rename (do-rename/lsv1 e1 srenames)]
                 [#:append (map for-deriv/phase-up srhss)]
                 [#:rename (do-rename/lsv2 srenames vrenames)]
                 [#:append (map for-deriv vrhss)]
                 (for-bderiv body))]
        [(AnyQ p::STOP (e1 e2 rs))
         null]
        ;; synth (should synth be idempotent?... heh, no point for now)
        [(AnyQ p:rename (e1 e2 rs rename inner))
         (>>Seek [#:rename (do-rename (car rename) (cdr rename))]
                 (for-deriv inner))]

        ;; Macros

        [(AnyQ mrule (e1 e2 (? error-wrap? ew) next))
         (list (make-s:subterm #f ew))]
        [(AnyQ mrule (e1 e2 tx next))
         (recv [(subterms table) (for-transformation tx)]
               (parameterize ((subterms-table table))
                 (append subterms (for-deriv next))))]
        
        [(AnyQ lift-deriv (e1 e2 first lifted-stx next))
         #;(printf "encountered lift-deriv in seek mode!~n")
         (raise (make-localactions))
         (>>Seek (for-deriv first)
                 (for-deriv next))]

        [(AnyQ lift/let-deriv (e1 e2 first lifted-stx next))
         (raise (make-localactions))]

        ;; Errors

        [#f null]
        ))

    ;; for-transformation : Transformation -> (values (list-of Subterm) Table)
    (define (for-transformation tx)
      (match tx
        [(IntQ transformation (e1 e2 rs me1 me2 locals _seq))
         ;; FIXME: We'll need to use e1/e2/me1/me2 to synth locals, perhaps
         ;; FIXME: and we'll also need to account for *that* marking, too...
         (for-each for-local-action (or locals null))
         ;(let* ([table-at-end #f]
         ;       [subterms
         ;        (>>Seek [#:rename (do-rename e1 me1)]
         ;                [#:append (map for-local locals)]
         ;                [#:rename (do-rename me2 e2)]
         ;                (begin (set! table-at-end (subterms-table))
         ;                       null))])
         ;  (values subterms table-at-end))
         (let-values ([(rename-subterms1 table1) (do-rename e1 me1)])
           (parameterize ((subterms-table table1))
             (let () ;; [sss (map for-local locals)]
               (let-values ([(rename-subterms2 table2) (do-rename me2 e2)])
                 ;; FIXME: Including these seems to produce evil results
                 ;; ie, parts of the hidden macro use appear as marked
                 ;;     when they shouldn't
                 (values null ;; (append rename-subterms1 (apply append sss) rename-subterms2)
                         table2)))))]
        [(ErrW transformation (e1 e2 rs me1 me2 locals _seq))
         (for-each for-local-action (or locals null))
         (values null #f)]))

    ;; for-local-action : LocalAction -> (list-of Subterm)
    (define (for-local-action local)
      (match local
        [(struct local-expansion (e1 e2 me1 me2 for-stx? deriv))
         (let-values ([(rename-subtersm2 table2) (do-rename me1 e1)])
           (let ([subterms (for-deriv deriv)])
             (when (pair? (filter s:subterm? subterms))
               (raise (make-localactions)))))]
        [(struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv))
         (let-values ([(rename-subtersm2 table2) (do-rename me1 e1)])
           (let ([subterms (for-deriv deriv)])
             (when (pair? (filter s:subterm? subterms))
               (raise (make-localactions)))))]
        [(struct local-lift (expr id))
         ;; FIXME: seek in the lifted deriv, transplant subterm expansions *here*
         (extract/remove-unvisited-lift id)]
        [(struct local-lift-end (decl))
         ;; FIXME!!!
         (void)]
        [(struct local-bind (deriv))
         (raise (make-localactions))]))

    ;; for-lderiv : ListDerivation -> (list-of Subterm)
    (define (for-lderiv ld)
      (match ld
        [(IntQ lderiv (es1 es2 derivs))
         (apply append (map for-deriv derivs))]
        [(struct error-wrap (exn tag inner))
         (append (for-lderiv inner)
                 (list (make-s:subterm #f ld)))]
        [#f null]))

    ;; for-bderiv : BlockDerivation -> (list-of Subterm)
    (define (for-bderiv bd)
      (for-lderiv (bderiv->lderiv bd)))

    (for-deriv d))






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
      [(AnyQ transformation (e1 e2 rs me1 me2 locals _seq))
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
        (loop (car p) (cons (make-ref pos) rpath))
        (let ([t (cdr p)])
          (cond [(syntax? t)
                 (let ([te (syntax-e t)])
                   (if (pair? te)
                       (begin
                         (table-add! table t (reverse (cons (make-tail pos) rpath)))
                         (loop-cons te rpath (add1 pos)))
                       (loop t (cons (make-tail pos) rpath))))]
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
                                (map (lambda (p) (make-s:rename p stx rename))
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
        #;(printf "~nNew Table: ~s~n" t)
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

    )
