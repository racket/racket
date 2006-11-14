
(module hide mzscheme
  (require (lib "plt-match.ss")
           (lib "list.ss")
           "deriv.ss"
           "deriv-util.ss"
           "synth-engine.ss"
           "stx-util.ss"
           "context.ss")
  (provide (all-defined))
  
  #;
  (provide hide/policy
           hide
           #;seek/syntax
           macro-policy
           current-hiding-warning-handler)

  ;; hide/policy : Derivation (identifier -> boolean) -> (values Derivation syntax)
  (define (hide/policy deriv show-macro?)
    (parameterize ((macro-policy show-macro?))
      (hide deriv)))

  ;; current-hiding-warning-handler : (parameter-of (symbol string -> void))
  (define current-hiding-warning-handler
    (make-parameter
     (lambda (tag message) (printf "~a: ~a~n" tag message))))

  (define (warn tag message) ((current-hiding-warning-handler) tag message))

  ;; machinery for reporting things that macro hiding can't handle
  (define-struct nonlinearity (message paths))
  (define-struct localactions ())


;   +@   ++           -                    
;   *@+  ++           @-                   
;   *@@  ++  -+@+-  -+@+++   -+@+-   -+@@+ 
;   ++++ ++ -@+-+@-  +@+*-   @+-+@   @+--- 
;   ++-@ ++ ++   @+   +-    *@   @- -@-    
;   ++ @-++ ++   ++   +-    +@@@@@+  +@+*  
;   ++ *@$+ ++   +$   +-    +@----    -+@$ 
;   ++  @@+ ++   ++   +-    *@          -@-
;   ++  +@+ -@* +@-   ++ --  @+  -   -  *@ 
;   *+  -@*  *@@@-    -@@@-  -@@@@-  @@@@- 

  
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


;   -$   @-    $+        @-        
;   -$   @-    --        @-        
;   -$   @-  +++-    -@@+@-  -+@+- 
;   -@---@-  -*@+   -@+-@@-  @+-+@ 
;   -@@@@@-    ++   ++   @- *@   @-
;   -$   @-    ++   ++   @- +@@@@@+
;   -$   @-    ++   ++   @- +@---- 
;   -$   @-    ++   ++   @- *@     
;   -$   @-    ++   -@- $@-  @+  - 
;   -$   @-    ++    +@@+@-  -@@@@-

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
        [(IntQ transformation (e1 e2 rs me1 me2 locals))
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


;    -@@@$                  -$     
;    @*  -                  -$     
;   -@-      -+@+-   -+@+-  -$  -+-
;    @@-     @+-+@   @+-+@  -$ -@+ 
;     +@$-  *@   @- *@   @- -+-@+  
;      -@@  +@@@@@+ +@@@@@+ -@@+   
;        @- +@----  +@----  -@+@-  
;        @- *@      *@      -$ +@- 
;    +- +@   @+  -   @+  -  -$  +@ 
;   -@@@@-   -@@@@-  -@@@@- -$   $+

  ;; Seek:
  ;; The derivation is "inactive" or "hidden" by default,
  ;; but pieces of it can become visible if they correspond to subterms
  ;; of the hidden syntax.

  ;; seek/syntax : syntax Derivation -> (union (cons Derivation Derivation) #f)
  ;; Seeks for derivations of *exactly* the given syntax (not a subterm)
  ;; Does track the syntax through renaming, however.
  ;; Returns the whole derivation followed by the subterm derivation.
  ;; If there is no subderivation for that syntax, returns #f.
  #;
  (define (seek/syntax stx deriv)
    (let ([subterms (gather-one-subterm (deriv-e1 deriv) stx)])
      (parameterize ((subterms-table subterms))
        (let ([subderivs (subterm-derivations deriv)])
          (unless (and (pair? subderivs) (null? (cdr subderivs)))
            (error 'seek/syntax "nonlinear subterm derivations"))
          (if (pair? subderivs)
              (values (create-synth-deriv (deriv-e1 deriv) subderivs)
                      (s:subterm-deriv (car subderivs)))
              #f)))))

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
              (values ew (deriv-e2 inner))]
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
      (let ([e2 (substitute-subterms e1 subterms)])
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
                  (for-unlucky-deriv d)]
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
        [(struct transformation (e1 e2 rs me1 me2 locals))
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


  ;; check-nonlinear-subterms : (list-of Subterm) -> void
  ;; FIXME: No checking on renamings... need to add
  ;; Note: Make sure subterm contexts are *disjoint*, not just *distinct*
  (define (check-nonlinear-subterms subterm-derivs)
    (check-nonlinear-paths
     (map s:subterm-path
          (filter s:subterm? subterm-derivs))))

  ;; check-nonlinear-paths : (list-of Path) -> void
  ;; FIXME: This is overly conservative for now, but probably 
  ;; okay given the way I construct paths.
  (define (check-nonlinear-paths paths)
    ;; If there is a self path (null), then it must be the only path.
    ;; If there are any tail paths, there can be only one (too restrictive?), 
    ;;    and the number must be at least as high as any ref paths.
    ;; Group ref paths by number and recur
    (define (tail-path? x) (and (pair? x) (tail? (car x))))
    (define (ref-path? x) (and (pair? x) (ref? (car x))))

    (let ([null-paths (filter null? paths)]
          [tail-paths (filter tail-path? paths)]
          [ref-paths (filter ref-path? paths)])
      (when (and (pair? null-paths)
                 (or (> (length null-paths) 1)
                     (pair? tail-paths)
                     (pair? ref-paths)))
        (raise (make-nonlinearity "self path plus others" paths)))
      (when (pair? tail-paths)
        (when (> (length tail-paths) 1)
          (raise (make-nonlinearity "multiple tail paths" paths)))
        (let ([n (tail-n (car (car tail-paths)))])
          (for-each (lambda (p)
                      (when (> (ref-n (car p)) n)
                        (raise (make-nonlinearity
                                "ref path after tail path"
                                paths))))
                    ref-paths)))
      (let ([ref-path-partitions (partition&cdr-ref-paths ref-paths)])
        (for-each check-nonlinear-paths ref-path-partitions))))

  ;; partition&cdr-ref-paths : (list-of Path) -> (list-of (list-of Path))
  (define (partition&cdr-ref-paths paths)
    (let ([t (make-hash-table 'equal)]
          [/null (lambda () null)])
      (for-each (lambda (p)
                  (hash-table-put! t (ref-n (car p))
                                   (cons (cdr p)
                                         (hash-table-get t (ref-n (car p)) /null))))
                paths)
      (hash-table-map t (lambda (k v) v))))

  ;; substitute-subterms : Syntax (list-of Subterm) -> Syntax
  ;; - s:subterm contexts guaranteed to be disjoint.
  ;; - s:renames replace syntax with syntax of same structure
  ;; FIXME: Could do this more efficiently using the structure of contexts...
  (define (substitute-subterms stx subterm-derivs)
    (cond [(null? subterm-derivs)
           stx]
          [(s:subterm? (car subterm-derivs))
           (let* ([subterm0 (car subterm-derivs)]
                  [path0 (s:subterm-path subterm0)]
                  [deriv0 (s:subterm-deriv subterm0)])
             (let ([e2 (lift/deriv-e2 deriv0)])
               (and e2
                    (substitute-subterms
                     (if path0 (path-replace stx path0 (deriv-e2 deriv0)) stx)
                     (cdr subterm-derivs)))))]
          [(s:rename? (car subterm-derivs))
           (let ([subterm0 (car subterm-derivs)])
             (substitute-subterms
              (path-replace stx
                            (s:rename-path subterm0)
                            (s:rename-after subterm0))
              (cdr subterm-derivs)))]
          [else (error 'substitute-subterms "neither s:subterm nor s:rename")]))

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



;                     +###+                                       
;  +@@  +@@:             @+                                       
;   @+   @+              @+                                       
;   @+   @+              @+                                       
;   @+   @+   +###+      @+   :@@ +@+    +###+  :@$$ +@#  -+###+: 
;   @+   @+  +#:  #+     @+    +@+**@+  +#:  #+  :+@++*@  #+   ++ 
;   @@###@+  #+   +#     @+    +@:  +#  #+   +#   +@:  +  @+   :: 
;   @+   @+  @@###@#     @+    +@   +@  @@###@#   +@      +@#++   
;   @+   @+  @+          @+    +@   +@  @+        +@        ++#@+ 
;   @+   @+  #+          @+    +@   ++  #+        +@      +    +@ 
;   @+   @+  :@+- :+     @+    +@- +@*  :@+- :+   +@      @:   ++ 
;  +@#  +@#-  :+@@#+  +##@@##  +@$$#+    :+@@#+ :#@@##+   +#@##+  
;                              +@                                 
;                              +@                                 
;                             :###+                               

  ;; show-macro? : identifier -> boolean
  (define (show-macro? id)
    ((macro-policy) id))
  
  ;; show-mrule? : MRule -> boolean
  (define (show-transformation? tx)
    (match tx
      [($$ transformation (e1 e2 rs me1 me2 locals))
       (let ([rs (reverse rs)])
         (and (pair? rs) (show-macro? (car rs))))]
      [($$ interrupted-wrap (tag inner))
       (show-transformation? inner)]
      [($$ error-wrap (exn tag inner))
       (show-transformation? inner)]))

  (define (map/2values f items)
    (if (null? items)
        (values null null)
        (let*-values ([(a0 b0) (f (car items))]
                      [(as bs) (map/2values f (cdr items))])
          (values (cons a0 as) (cons b0 bs)))))
  
  ;; decompose-letrec : Derivation -> (values DerivList
  ;;                                          (list-of (cons Syntax Derivation))
  ;;                                          (list-of (cons Syntax Derivation))
  ;; Extract the syntax RHS, value RHSs, and expression derivs
  ;; from a block-generated letrec-values or letrec-syntaxes form.
  (define (decompose-letrec deriv)
    (match deriv
      [(IntQ p:letrec-syntaxes+values (_ _ _ srenames srhss vrenames vrhss body))
       ;; Assertion: pass1 of the body is always trivial
       (with-syntax ([(([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body) srenames]
                     [(([?vvars* ?vrhs*] ...) . ?body*) vrenames])
         (values (map cons
                      (syntax->list #'(?svars ...))
                      srhss)
                 (map cons (syntax->list #'(?vvars* ...)) vrhss)
                 (lderiv-derivs (bderiv-pass2 body))))]
      [(IntQ p:letrec-values (_ _ _ vrenames vrhss body))
       ;; Assertion: pass1 of the body is always trivial
       (with-syntax ([(([?vars ?rhs] ...) . ?body) vrenames])
         (values null
                 (map cons (syntax->list #'(?vars ...)) vrhss)
                 (match body
                   [(IntQ bderiv (_ _ _pass1 _ (IntQ lderiv (_ _ derivs))))
                    derivs]
                   [#f
                    null])))]))

  ;; combine-derivs : Derivation Derivation -> Derivation
  ;; Adds the second derivation to the end of the first derivation.
  ;; Inserts a p:rename rule when the final syntax of the first derivation 
  ;; is not identical to the initial syntax of the second.
  (define (combine-derivs head tail)
    ;; head-loop : Derivation -> (values Derivation syntax)
    (define (head-loop head)
      (match head
        [(struct mrule (e1 e2 tx next))
         (recv [(next e2) (head-loop next)]
               (values (outer-rewrap tail (make-mrule e1 e2 tx next))
                       e2))]
        [(IntW mrule (e1 e2 tx #f) 'macro)
         (values head e2)]
        ;; FIXME!!!
        [(struct p:stop (e1 e2 rs))
         (adjust-tail e2 rs)]
        ;; FIXME: combine these?
        [(struct p::STOP (e1 e2 rs))
         (adjust-tail e2 rs)]
        [(struct p:variable (e1 e2 rs))
         (adjust-tail e2 rs)]))

    ;; adjust-tail : syntax (list-of syntax) -> (values Derivation syntax)
    (define (adjust-tail head-e2 head-rs)
      (match tail
        [(AnyQ deriv (e1 e2))
         (values (if (eq? head-e2 e1)
                     tail
                     (outer-rewrap 
                      tail
                      (make-p:rename head-e2 e2 head-rs (cons head-e2 e1) tail)))
                 e2)]
        [#f (values (make-p:stop head-e2 head-e2 head-rs)
                    head-e2)]))

    (recv [(d s) (head-loop head)]
          d))

  ;; wrap-p:rename : syntax (cons syntax syntax) Derivation -> Derivation
  (define (wrap-p:rename e1 rename deriv)
    (make-p:rename e1 (deriv-e2 deriv) null rename deriv))

  ;; wrap/rename-from : syntax Derivation -> Derivation
  (define (wrap/rename-from e0 d)
    (match d
      [(AnyQ deriv (e1 e2))
       (rewrap d (make-p:rename e0 e2 null (cons e0 e1) d))]))
  
  ;; reconstruct-defval : syntax syntax Derivation -> Derivation
  ;; Reconstruct a define-values node from its rhs deriv
  (define (reconstruct-defval head-e2 dvvars dvrhs)
    (match dvrhs
      [(AnyQ deriv (rhs-e1 rhs-e2))
       (with-syntax ([(?dv ?vars ?rhs) head-e2]
                     [?vars* dvvars]
                     [?rhs* rhs-e1])
         ;; Are there any other renames that 
         ;; should be applied to the rhs?
         (let* ([dv1 head-e2]
                [dv1* (syntax/skeleton dv1 (?dv ?vars* ?rhs*))]
                [dv2 
                 (and rhs-e2
                      (with-syntax ([?rhs** rhs-e2])
                        (syntax/skeleton dv1 (?dv ?vars* ?rhs**))))])
           (outer-rewrap 
            dvrhs
            (make-p:rename
             dv1
             dv2
             null
             (cons (cons #'?vars #'?rhs)
                   (cons #'?vars* #'?rhs*))
             (outer-rewrap dvrhs (make-p:define-values dv1* dv2 null dvrhs))))))]))

  ;; bderiv->lderiv : BlockDerivation -> ListDerivation
  ;; Combines pass1 and pass2 into a single pass(2) list derivation
  (define (bderiv->lderiv bd)
    (match bd
      [#f #f]
      [(IntQ bderiv (es1 _es2 pass1 trans pass2))
       (let-values ([(_dss dvs exprs)
                     (case trans
                       [(letrec)
                        (match pass2
                          [(IntQ lderiv (_ _ (list letrec-deriv)) _)
                           (decompose-letrec letrec-deriv)])]
                       [(list)
                        (match pass2
                          [($$ lderiv (_ _ derivs) _)
                           (values null null derivs)]
                          [#f
                           (values null null null)])])]
                    [(brules) pass1]
                    [(suffix) es1]
                    [(interrupted?) (interrupted-wrap? bd)])
         ;; take-expr : -> Derivation/#f
         (define (take-expr)
           (if (pair? exprs)
               (begin0 (car exprs)
                       (set! exprs (cdr exprs)))
               #f))
         ;; take-defval : -> (cons syntax Derivation) | #f
         (define (take-defval)
           (if (pair? dvs)
               (begin0 (car dvs)
                       (set! dvs (cdr dvs)))
               #f))

         ;; loop : number -> (list-of BRule)
         ;; brules, dvs, exprs, suffix threaded through, so use set!
         ;; dss are all trivial; fully expanded in pass 1
         (define (loop count)
           (if (positive? count)
               (match brules
                 [(cons (and first (struct error-wrap (exn tag #f))) next)
                  (set! suffix (stx-cdr suffix))
                  (set! brules next)
                  (cons first null)]
                 [(cons (struct b:defvals (renames head)) next)
                  (let ([dv (take-defval)])
                    (set! suffix (stx-cdr suffix))
                    (set! brules next)
                    (let ([finish (and dv (reconstruct-defval (deriv-e2 head) (car dv) (cdr dv)))])
                      (cons (make-b:expr renames (combine-derivs head finish))
                            (loop (sub1 count)))))]
                 [(cons (and first (ErrW b:defvals (renames head))) next)
                  ;; Error is after head
                  (let ([dv (take-defval)])
                    (set! suffix (stx-cdr suffix))
                    (set! brules next)
                    (cons
                     (make-b:expr
                      renames
                      (combine-derivs head
                                      (rewrap first
                                              (make-p:define-values (deriv-e2 head)
                                                                    #f
                                                                    null
                                                                    #f))))
                     null #;(loop (sub1 count))))]
                 [(cons (IntQ b:defstx (renames head rhs)) next)
                  (let ([stx (car suffix)])
                    (set! _dss (cdr _dss))
                    (set! suffix (stx-cdr suffix))
                    (set! brules next)
                    (cons (make-b:defstx renames head rhs)
                          (loop (sub1 count))))]
                 [(cons (struct b:splice (renames head tail)) next)
                  (let ([n (- (length tail) (length (stx->list (stx-cdr suffix))))])
                    (set! suffix tail)
                    (set! brules next)
                    (let* ([splice-derivs (loop n)]
                           [next (loop (sub1 count))])
                      (cons (make-b:begin renames head splice-derivs)
                            next)))]
                 [(cons (ErrW b:splice (renames head tail) exn) next)
                  ;; Problem with tail
                  (set! suffix tail)
                  (set! brules next)
                  (cons (make-b:expr renames
                                     (combine-derivs head
                                                     (make-error-wrap
                                                      exn
                                                      #f
                                                      (make-p:begin (deriv-e2 head)
                                                                    #f
                                                                    null
                                                                    #f))))
                        null)]
                 [(cons (and first (IntQ b:expr (renames head))) next)
                  (let ([expr1 (take-expr)])
                    (set! suffix (stx-cdr suffix))
                    (set! brules next)
                    (cons (make-b:expr renames (combine-derivs head expr1))
                          (if (wrapped? first) null (loop (sub1 count)))))]
                 ['()
                  ;; We've reached the end of pass1 processing.
                  ;; We need to pull in exprs to fill out the begin/block shape.
                  (let* ([expr1 (take-expr)]
                         [e1 (stx-car suffix)]
                         [expr1 (or expr1 (make-p:stop e1 e1 null))]
                         [expr1-e1 (match expr1 [(AnyQ deriv (e1 e2)) e1])])
                    (set! suffix (stx-cdr suffix))
                    (cons (make-b:expr (cons e1 expr1-e1) expr1)
                          (loop (sub1 count))))])
               ;; Otherwise, we've reached the end, either locally or globally
               null))

         ;; to-deriv : BRule syntax -> Derivation
         (define (to-deriv br stx)
           (match br
             [(struct b:expr (renames head))
              (outer-rewrap head (make-p:rename stx (lift/deriv-e2 head) null renames head))]
             [(struct b:begin (renames head inners))
              (with-syntax ([(?begin . ?inner-terms) (lift/deriv-e2 head)])
                (let* ([inner-derivs (map to-deriv inners (syntax->list #'?inner-terms))]
                       [inner-es1 (map lift/deriv-e1 inner-derivs)]
                       [inner-es2 (map lift/deriv-e2 inner-derivs)]
                       [interrupted?
                        (or (wrapped? head)
                            (ormap wrapped? inner-derivs))]
                       [e2 (if interrupted? 
                               #f
                               (with-syntax ([?inner-terms* inner-es2])
                                 (syntax/skeleton (lift/deriv-e2 head) (?begin . ?inner-terms*))))]
                       [base
                        (wrap-p:rename stx renames
                                       (combine-derivs
                                        head
                                        (make-p:begin (lift/deriv-e2 head) e2 null
                                                      (make-lderiv inner-es1 inner-es2 
                                                                   inner-derivs))))])
                  (if interrupted?
                      (make-interrupted-wrap #f base)
                      base)))]))
         
         (define (map2stxs f as bs)
           (if (pair? as)
               (cons (f (car as) (stx-car bs)) (map2stxs f (cdr as) (stx-cdr bs)))
               null))

         (let* ([brules (loop (stxs-improper-length es1))]
                [derivs (map2stxs to-deriv brules es1)])
           (rewrap/nt bd (make-lderiv es1 (if interrupted? #f (map deriv-e2 derivs)) derivs))))]))


  ;; module-begin->lderiv : PRule -> ListDerivation
  (define (module-begin->lderiv pr)
    (let-values ([(forms pass1 pass2)
                  (match pr
                    [(IntQ p:#%module-begin (e1 _ _ pass1 pass2))
                     (values (stx-cdr e1) pass1 pass2)])])
      ;; loop : number -> (list-of Derivation)
      ;; NOTE: Definitely returns a list of <number> elements; 
      ;; fills the end of the list with #f if necessary.
      (define (loop count)
        ;(printf "** MB->L (~s)~n" count)
        ;(printf "  forms: ~s~n" forms)
        ;(printf "  pass1: ~s~n" pass1)
        (if (positive? count)
            (match pass1
              [(cons (struct mod:prim (head prim)) next)
               (let ([form0 (stx-car forms)]
                     [pass1-part (car pass1)])
                 (set! forms (stx-cdr forms))
                 (set! pass1 next)
                 (let ([pass2-part (car (loop2 1))])
                   (cons (wrap/rename-from form0 (combine-prim pass1-part pass2-part))
                         (loop (sub1 count)))))]
              [(cons (struct mod:splice (head tail)) next)
               (let ([form0 (stx-car forms)]
                     [pass1-part (car pass1)]
                     [inner-n (- (length (stx->list tail))
                                 (length (stx->list (stx-cdr forms))))])
                 (set! forms tail)
                 (set! pass1 next)
                 (let ([inners (loop inner-n)])
                   (cons (wrap/rename-from form0 (combine-begin head inners))
                         (loop (sub1 count)))))]
              [(cons (struct mod:lift (head tail)) next)
               (let ([form0 (stx-car forms)]
                     [inner-n (length (stx->list tail))])
                 (set! forms (stx-cdr forms))
                 (set! pass1 next)
                 (let ([inners (loop inner-n)])
                   (set! forms (cons (deriv-e2 head) forms))
                   (let ([finish (car (loop 1))])
                     (cons (wrap/rename-from form0 (combine-lifts head finish inners))
                           (loop (sub1 count))))))]
              ['()
               #;(printf "module-begin->lderiv:loop: unexpected null~n")
               (cons #f (loop (sub1 count)))])
            null))

      ;; loop2 : number -> (list-of Derivation)
      ;; NOTE: Definitely returns a list of <number> elements; 
      ;; fills the end of the list with #f if necessary.
      (define (loop2 count)
        ;(printf "** loop2 (~s)~n" count)
        ;(printf "  forms: ~s~n" forms)
        ;(printf "  pass2: ~s~n" pass2)
        (if (positive? count)
            (match pass2
              [(cons (struct mod:skip ()) next)
               (set! pass2 next)
               (cons #f (loop2 (sub1 count)))]
              [(cons (struct mod:cons (deriv)) next)
               (set! pass2 next)
               (cons deriv (loop2 (sub1 count)))]
              [(cons (struct mod:lift (deriv tail)) next)
               (set! pass2 next)
               (let* ([head-e1 (deriv-e1 deriv)]
                      [head-e2 (deriv-e2 deriv)]
                      [inner-n (length tail)]
                      [inners (loop2 inner-n)]
                      [inners-es1 (map deriv-e1 inners)]
                      [inners-es2 (map deriv-e2 inners)]
                      [begin-stx1 #`(begin #,@inners-es1 #,(deriv-e2 deriv))]
                      [begin-stx2 #`(begin #,@inners-es2 #,(deriv-e2 deriv))])
                 (cons
                  (make-lift-deriv 
                   head-e1 begin-stx2
                   deriv
                   begin-stx1
                   (make-p:begin begin-stx1 begin-stx2 null
                                 (make-lderiv (append inners-es1 (list head-e2))
                                              (append inners-es2 (list head-e2))
                                              (append inners
                                                      (list (make-p:stop head-e2 head-e2 null))))))
                  (loop2 (sub1 count))))]
              ['()
               #;(printf "module-body->lderiv:loop2: unexpected null~n")
               (cons #f (loop2 (sub1 count)))])
            null))

      (let* ([derivs (loop (stxs-improper-length forms))]
             [es1 (map lift/deriv-e1 derivs)]
             [es2 (if (wrapped? pr) #f (map lift/deriv-e2 derivs))])
        (rewrap pr (make-lderiv es1 es2 derivs)))))

  (define (stxs-improper-length stx)
    (let loop ([stx stx] [n 0])
      (syntax-case stx ()
        [(first . rest) (loop #'rest (add1 n))]
        [_ n])))
  
  ;; combine-prim : MBRule Derivation -> Derivation
  ;; The MRule is always a mod:prim rule.
  ;; Need to insert a rename step in between...
  (define (combine-prim mr deriv)
    (let ([head (mod:prim-head mr)]
          [pr (mod:prim-prim mr)])
      (match pr
        [(struct p:define-syntaxes (e1 e2 rs rhs))
         ;; deriv is #f or trivial
         (combine-derivs head pr)]
        [(struct p:define-values (e1 e2 '() #f))
         ;; deriv is a pderiv for the entire define-values form
         (combine-derivs head deriv)]
        [#f
         ;; deriv is a complete derivation of the rest of the form
         (combine-derivs head deriv)]
        [(struct p::STOP (e1 e2 rs))
         ;; deriv is #f
         (combine-derivs head pr)])))
  
  ;; combine-begin : Derivation (list-of Derivation) -> Derivation
  (define (combine-begin head inners)
    (let* ([inners-es1 (map deriv-e1 inners)]
           [inners-es2 (map deriv-e2 inners)]
           [begin-e1 (deriv-e2 head)]
           [begin-e2 (with-syntax ([(?begin . _) begin-e1]
                                   [inners-es1 inners-es1])
                       (syntax/skeleton begin-e1 (?begin . inners-es1)))])
      (combine-derivs head
                      (make-p:begin begin-e1 begin-e2 null
                                    (make-lderiv inners-es1 inners-es2
                                                 inners)))))

  ;; combine-lifts : Derivation Derivation (list-of Derivation) -> Derivation
  (define (combine-lifts head finish inners)
    (let ([head-e1 (deriv-e1 head)]
          [head-e2 (deriv-e2 head)]
          [finish-e1 (deriv-e1 finish)]
          [finish-e2 (deriv-e2 finish)]
          [inners-es1 (map deriv-e1 inners)]
          [inners-es2 (map deriv-e2 inners)])
      (let ([begin-e1 #`(begin #,@inners-es1 #,finish-e2)]
            [begin-e2 #`(begin #,@inners-es2 #,finish-e2)])
        (make-lift-deriv 
         head-e1 begin-e2
         (combine-derivs head finish)
         (make-p:begin begin-e1 begin-e2 null
                       (make-lderiv (append inners-es1 (list finish-e2))
                                    (append inners-es2 (list finish-e2))
                                    (append inners
                                            (list (make-p:stop finish-e2
                                                               finish-e2
                                                               null)))))))))
  

  ;; lderiv->module-begin : ListDerivation -> PRule
  (define (lderiv->module-begin ld e1)
    (match ld
      [(IntQ lderiv (inners-es1 inners-es2 inners))
       (with-syntax ([(?module-begin . _) e1]
                     [inners-es1* inners-es1]
                     [inners-es2* inners-es2])
         (rewrap ld
                 (make-p:#%module-begin
                  (syntax/skeleton e1 (?module-begin . inners-es1*))
                  (syntax/skeleton e1 (?module-begin . inners-es2*))
                  null ;; FIXME 
                  (map (lambda (d) (make-mod:cons d)) inners)
                  (map (lambda (x) (make-mod:skip)) inners))))]))


  ;; Subterm Table
  ;; -------------
  
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
  )
