
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
           seek-syntax
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
              ([for-deriv RHS rhs]))]
        [(AnyQ p:define-values (e1 e2 rs rhs))
         (>>P d (make-p:define-values rhs)
              (define-values variables RHS)
              ([for-deriv RHS rhs]))]
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
               ([for-deriv INNER deriv]))
         #;
         (recv [(rhs-d rhs-e2) (for-deriv deriv)]
               (values (make-p:set!-macro e1 rhs-e2 rs rhs-d)
                       rhs-e2))]
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
                  [for-derivs (SRHS ...) srhss]
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
                  (recv #;[(tx) (for-transformation tx)]
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
        ;; Only normal lifts occur in first... no end-module-decl lifts.
        ;; They occur in reverse order.
        [(IntQ lift-deriv (e1 e2 first lifted-stx second) tag)
         ;; Option 1: Give up on first, hide on second
         #;
         (begin (warn 'lifts "lifts are unimplemented")
                (let-values ([(second e2) (for-deriv second)])
                  (values (rewrap d (make-lift-deriv e1 e2 first lifted-stx second))
                          e2)))
         ;; Option 2: Hide first, show *all* lifted expressions,
         ;; and hide second (lifted defs only; replace last expr with first-e2)
         (let* ([second-derivs
                 (match second
                   [(IntQ p:begin (_ _ _ (IntQ lderiv (_ _ inners))))
                    (reverse inners)])]
                [lift-stxs
                 (with-syntax ([(?begin form ...) lifted-stx])
                   (cdr (reverse (syntax->list #'(form ...)))))]
                [lift-derivs
                 ;; If interrupted, then main-expr deriv will not be in list
                 (if tag second-derivs (cdr second-derivs))]
                [begin-stx (stx-car lifted-stx)])
           (let-values ([(first-d first-e2) (for-deriv first)])
             (define lifted-stx*
               (datum->syntax-object lifted-stx
                                     `(,begin-stx ,@(reverse lift-stxs) ,first-e2)
                                     lifted-stx
                                     lifted-stx))
             (define main-deriv (make-p:stop first-e2 first-e2 null))
             (define inner-derivs 
               (reverse
                ;; If interrupted, then main-expr deriv will not be in list
                (if tag lift-derivs (cons main-deriv lift-derivs))))
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
                     e2*)))
         #;
         ;; Option3: Hide first, retaining transparent lifts and inlining opaque lifts
         ;; Hide second, only on retained lifts
         ;; Problem: lift order may be damaged by other hiding processes
         (let* ([second-derivs
                 (match second
                   [(IntQ p:begin (_ _ _ (IntQ lderiv (_ _ inners))))
                    (reverse inners)])]
                [lift-stxs
                 (with-syntax ([(?begin form ...) lifted-stx])
                   (cdr (reverse (syntax->list #'(form ...)))))]
                [lift-derivs (cdr second-derivs)]
                [begin-stx (stx-car lifted-stx)])
           (let-values ([(first-d first-e2 retained-lifts)
                         (parameterize ((lifts-available (map cons lift-stxs lift-derivs))
                                        (lifts-retained null))
                           (let-values ([(first-d first-e2) (for-deriv first)])
                             (unless (null? (lifts-available))
                               (printf "hide: lift-deriv: unused lift derivs!~n"))
                             (values first-d first-e2 (lifts-retained))))])
             ;; If all the lifts were hidden, then remove lift-deriv node
             ;; Otherwise, recreate with the retained lifts
             (if (null? retained-lifts)
                 (values first-d first-e2)
                 (let ()
                   (define retained-stxs (map car retained-lifts))
                   (define retained-derivs (map cdr retained-lifts))
                   (define lifted-stx*
                     (datum->syntax-object lifted-stx
                                           `(,begin-stx ,@retained-stxs ,first-e2)
                                           lifted-stx
                                           lifted-stx))
                   (define main-deriv (make-p:stop first-e2 first-e2 null))
                   (define inner-derivs 
                     (if tag retained-derivs (append retained-derivs main-deriv)))
                   (define lderiv*
                     (rewrap second
                             (make-lderiv (map lift/deriv-e1 inner-derivs)
                                          (map lift/deriv-e2 inner-derivs)
                                          inner-derivs)))
                   (define-values (ld*-d ld*-es2) (for-lderiv lderiv*))
                   (define e2*
                     (and ld*-es2 
                          (datum->syntax-object e2 `(,begin-stx ,@ld*-es2) e2 e2)))
                   (define second* 
                     (rewrap second (make-p:begin lifted-stx* e2* null ld*-d)))
                   (values (make-lift-deriv e1 e2* first-d lifted-stx* second*)
                           e2*)))))]

        ;; Errors

        [#f (values #f #f)]))

    ;; for-transformation : Transformation -> Transformation???
    #;
    (define (for-transformation tx)
      (match tx
        [(IntQ transformation (e1 e2 rs me1 me2 locals _seq))
         (error 'unimplemented "hide: for-transformation")]))
    
    ;; for-rename : Rename -> (values Rename syntax)
    (define (for-rename rename)
      (values rename rename))

    ;; for-renames : (list-of Rename) -> (values (list-of Rename) syntaxes)
    (define (for-renames renames)
      (values renames renames))

    ;; for-derivs : (list-of Derivation) -> (values (list-of Derivation) (list-of syntax))
    (define (for-derivs derivs)
      (let ([results 
             (map (lambda (d) (let-values ([(a b) (for-deriv d)]) (cons a b)))
                  derivs)])
        (values (map car results) (map cdr results))))

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
      (match bd
        [(IntQ bderiv (es1 es2 pass1 trans pass2))
         (let ([pass2 (bderiv->lderiv bd)])
           (recv [(pass2 es2) (for-lderiv pass2)]
                 (values (rewrap/nt bd (make-bderiv es1 es2 null 'list pass2))
                         es2)))]
        [#f (values #f #f)]))

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
           (or (interrupted-wrap? (s:subterm-deriv x)) (error-wrap? (s:subterm-deriv x)))))
    (let ([errors
           (map s:subterm-deriv (filter error? subterm-derivs))]
          [subterms (filter (lambda (x) (not (error? x))) subterm-derivs)])
      ;(printf "subterm paths:~n~s~n" (map s:subterm-path subterm-derivs))
      ;(printf "subterms:~n~s~n" subterm-derivs)
      (let ([e2 (and (null? errors) (substitute-subterms e1 subterms))])
        (let ([d (make-p:synth e1 e2 null subterms)])
          (if (pair? errors)
              (rewrap (car errors) d)
              d)))))

  ;; subterm-derivations : Derivation -> (list-of Subterm)
  (define (subterm-derivations d)

    ;; for-deriv : Derivation -> (list-of Subterm)
    ;; FIXME: finish
    (define (for-deriv d)
      (match d
        [(AnyQ deriv (e1 e2))
         (let ([paths (table-get (subterms-table) e1)])
           (cond [(null? paths)
                  (for-unlucky-deriv/record-error d)]
                 [(null? (cdr paths))
                  (let-values ([(d _) (hide d)])
                    (list (make-s:subterm (car paths) d)))]
                 [else
                  ;; More than one path to the same(eq?) syntax object
                  ;; Not good.
                  ;; FIXME: Better to delay check to here, or check whole table first?
                  ;; FIXME
                  (raise (make-nonlinearity "nonlinearity in original term" paths))]))]
        [#f null]))

    ;; for-unluck-deriv/record-error -> (list-of Subterm)
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
         (>>Seek (for-deriv rhs))]
        [(AnyQ p:define-values (e1 e2 rs rhs))
         (>>Seek (for-deriv rhs))]
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
         ;; 1 Make a new table
         ;;   Can narrow table to things that only occur in the renames
         ;; 2 Search body
         ;; 3 Make a "renaming" step... FIXME, how to represent?
         (>>Seek [! exni]
                 [#:rename (do-rename/lambda e1 renames)]
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
                 [#:append (map for-deriv srhss)]
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

        [(AnyQ mrule (e1 e2 (? transformation? tx) next))
         (recv [(subterms table) (for-transformation tx)]
               (parameterize ((subterms-table table))
                 (append subterms (for-deriv next))))]
        [(AnyQ mrule (e1 e2 (and ew (struct error-wrap (_ _ _))) next))
         (list (make-s:subterm #f ew))]


        [(AnyQ lift-deriv (e1 e2 first lifted-stx next))
         (>>Seek (for-deriv first)
                 (for-deriv next))]
        
        ;; Errors

;        [(struct error-wrap (exn tag (? deriv? inner)))
;         (append (for-deriv inner)
;                 (list (make-s:subterm #f (make-error-wrap exn tag #f))))]
        [#f null]
        ))

    ;; for-transformation : Transformation -> (values (list-of Subterm) Table)
    (define (for-transformation tx)
      (match tx
        [(struct transformation (e1 e2 rs me1 me2 locals _seq))
         ;; FIXME: We'll need to use e1/e2/me1/me2 to synth locals, perhaps
         ;; FIXME: and we'll also need to account for *that* marking, too...
         (unless (null? locals)
           (raise (make-localactions)))
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
             (let (#;[sss (map for-local locals)])
               (let-values ([(rename-subterms2 table2) (do-rename me2 e2)])
                 ;; FIXME: Including these seems to produce evil results
                 ;; ie, parts of the hidden macro use appear as marked
                 ;;     when they shouldn't
                 (values (append #;rename-subterms1
                                 #;(apply append sss)
                                 #;rename-subterms2)
                         table2)))))]))

    ;; for-local : LocalAction -> (list-of Subterm)
    #;
    (define (for-local local)
      (match local
        [(IntQ local-expansion (e1 e2 me1 me2 deriv))
         (error 'unimplemented "seek: for-local")]
        ;; Also need to handle local-bind
        ;; ...
        [else null]))

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
    (with-syntax ([(?lambda ?formals . ?body) stx])
      (do-rename (cons #'?formals #'?body) rename)))

  (define (do-rename/let stx rename)
    (with-syntax ([(?let ?bindings . ?body) stx])
      (do-rename (cons #'?bindings #'?body) rename)))

  (define (do-rename/case-lambda stx rename)
    (with-syntax ([(?formals . ?body) stx])
      (do-rename (cons #'?formals #'?body) rename)))

  (define (do-rename/lsv1 stx rename) 
    (with-syntax ([(?lsv ?sbindings ?vbindings . ?body) stx])
      (do-rename (cons #'?sbindings (cons #'?vbindings #'?body)) rename)))

  (define (do-rename/lsv2 old-rename rename)
    (if rename
        (with-syntax ([(?sbindings ?vbindings . ?body) old-rename])
          (do-rename (cons #'?vbindings #'?body) rename))
        (values null
                (subterms-table))))

  
  

;                                                                 
;                                                                 
;                                                                 
;                              ;;                                 
;                              ;;                                 
;                               ;                  ;              
;                               ;                  ;              
;    ;;;;;     ;;;;     ;;;;    ;  ;;;   ;;;;;   ;;;;;;  ;;;;  ;; 
;   ;;   ;    ;   ;    ;   ;    ;  ;    ;;   ;     ;       ;   ;  
;   ;;       ;;   ;;  ;;   ;;   ; ;     ;;         ;        ; ;   
;    ;;;     ;;;;;;;  ;;;;;;;   ;;;      ;;;       ;        ;;;   
;      ;;;;  ;        ;         ;;;        ;;;;    ;        ;;;   
;   ;    ;;  ;;       ;;        ; ;;    ;    ;;    ;        ; ;   
;   ;    ;;   ;;       ;;       ;  ;;   ;    ;;    ;;      ;   ;  
;   ;;;;;;     ;;;;     ;;;;   ;;;  ;;; ;;;;;;      ;;;  ;;;  ;;; 
;                                                                 
;                                                                 
;                                                                 
;                                                                 
  
  (define-syntax proptable
    (syntax-rules ()
      [(proptable expr)
       (let-values ([(subterms table) expr]
                    [(old-table) (subterms-table)])
         (hash-table-for-each
          old-table
          (lambda (k v) (hash-table-put! table k v)))
         (printf "** New table: ~s~n" (hash-table-count table))
         (begin (printf "  > ")
                (hash-table-for-each table (lambda (k v) (write (syntax-object->datum k)) (display " ")))
                (printf "~n"))
         (values subterms table))]))

  ;; seek-syntax : Syntax Derivation -> (list-of Derivation)
  (define (seek-syntax stx d)

    ;; for-deriv : Derivation -> (list-of Derivation)
    (define (for-deriv d)
      (cond [(hash-table-get (subterms-table) (lift/deriv-e1 d) #f)
             (list d)]
            [else (for-unlucky-deriv d)]))

    ;; for-unlucky-deriv : Derivation -> (list-of Derivation)
    (define (for-unlucky-deriv d)
      (parameterize ((print-struct #f)) 
        (printf "unlucky with ~s[[~s]]~n" d (syntax-object->datum (lift/deriv-e1 d))))
      (match d
        ;; Primitives
        [(AnyQ p:module (e1 e2 rs one-body-form? body))
         (cond [one-body-form?
                ;; FIXME: tricky... how to do renaming?
                (for-deriv body)]
               [else
                (with-syntax ([(?module ?name ?lang . ?body) e1]
                              [(?module-begin . ?body*) (lift/deriv-e1 body)])
                  (>>Seek [#:rename (proptable (do-rename #'?body #'?body*))]
                          (for-deriv body)))])]
        [(AnyQ p:#%module-begin (e1 e2 rs pass1 pass2))
         ;; FIXME: No new allocation!
         (let ([lderiv (module-begin->lderiv d)])
           (for-lderiv lderiv))]
        [(AnyQ p:variable (e1 e2 rs))
         null]
        [(AnyQ p:define-syntaxes (e1 e2 rs rhs))
         (>>Seek (for-deriv rhs))]
        [(AnyQ p:define-values (e1 e2 rs rhs))
         (>>Seek (for-deriv rhs))]
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
         (>>Seek [! exni]
                 [#:rename (proptable (do-rename/lambda e1 renames))]
                 (for-bderiv body))]
        [(AnyQ p:case-lambda (e1 e2 rs renames+bodies))
         (with-syntax ([(?case-lambda ?clause ...) e1])
           (let ()
             (define (handle-clause clause-stx rename body)
               (>>Seek [#:rename (proptable (do-rename/case-lambda clause-stx rename))]
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
         (>>Seek [#:rename (proptable (do-rename/let e1 renames))]
                 [#:append (map for-deriv rhss)]
                 (for-bderiv body))]
        [(AnyQ p:letrec-values (e1 e2 rs renames rhss body))
         (>>Seek [#:rename (proptable (do-rename/let e1 renames))]
                 [#:append (map for-deriv rhss)]
                 (for-bderiv body))]
        [(AnyQ p:letrec-syntaxes+values (e1 e2 rs srenames srhss vrenames vrhss body))
         (>>Seek [#:rename (proptable (do-rename/lsv1 e1 srenames))]
                 [#:append (map for-deriv srhss)]
                 [#:rename (proptable (do-rename/lsv2 srenames vrenames))]
                 [#:append (map for-deriv vrhss)]
                 (for-bderiv body))]
        [(AnyQ p::STOP (e1 e2 rs))
         null]
        [(AnyQ p:synth (e1 e2 rs ss))
         (let loop ([ss ss])
           (if (null? ss)
               null
               (let ([s0 (car ss)])
                 (parameterize ((print-struct #f)) (printf "subterm: ~s~n" s0))
                 (cond [(s:subterm? s0)
                        (>>Seek (for-deriv (s:subterm-deriv s0))
                                (loop (cdr ss)))]
                       [(s:rename? s0)
                        (>>Seek [#:rename (proptable 
                                           (do-rename (s:rename-before s0)
                                                      (s:rename-after s0)))]
                                (loop (cdr ss)))]
                       [else
                        (loop (cdr ss))]))))]
        [(AnyQ p:rename (e1 e2 rs rename inner))
         (>>Seek [#:rename (proptable (do-rename (car rename) (cdr rename)))]
                 (for-deriv inner))]
        
        ;; Macros

        [(AnyQ mrule (e1 e2 (? transformation? tx) next))
         (recv [(subterms table) (for-transformation tx)]
               (parameterize ((subterms-table table))
                 (append subterms (for-deriv next))))]
        
        [(AnyQ lift-deriv (e1 e2 first lifted-stx next))
         (>>Seek (for-deriv first)
                 (for-deriv next))]
        
        [#f null]
        ))

    ;; for-transformation : Transformation -> (values (list-of Subterm) Table)
    (define (for-transformation tx)
      (match tx
        [(struct transformation (e1 e2 rs me1 me2 locals _seq))
         ;; FIXME: We'll need to use e1/e2/me1/me2 to synth locals, perhaps
         ;; FIXME: and we'll also need to account for *that* marking, too...
         (let-values ([(rename-subterms1 table1) (proptable (do-rename e1 me1))])
           (parameterize ((subterms-table table1))
             (let-values ([(rename-subterms2 table2) (proptable (do-rename me2 e2))])
               ;; FIXME: Including these seems to produce evil results
               ;; ie, parts of the hidden macro use appear as marked
               ;;     when they shouldn't
               (values null table2))))]))

    ;; for-lderiv : ListDerivation -> (list-of Subterm)
    (define (for-lderiv ld)
      (match ld
        [(IntQ lderiv (es1 es2 derivs))
         (apply append (map for-deriv derivs))]
        [(struct error-wrap (exn tag inner))
         (for-lderiv inner)]
        [#f null]))

    ;; for-bderiv : BlockDerivation -> (list-of Subterm)
    (define (for-bderiv bd)
      (for-lderiv (bderiv->lderiv bd)))

    (let ([table0 (make-hash-table)])
      (hash-table-put! table0 stx #t)
      (parameterize ((subterms-table table0))
        (for-deriv d))))

    )
