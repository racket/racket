
(module synth-derivs mzscheme
  (require (lib "plt-match.ss")
           (lib "list.ss")
           "deriv.ss"
           "deriv-util.ss"
           "synth-engine.ss"
           "stx-util.ss"
           "context.ss")
  (provide (all-defined))

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
        (raise (make nonlinearity #f paths)))
      (when (pair? tail-paths)
        (when (> (length tail-paths) 1)
          (raise (make nonlinearity #f paths)))
        (let ([n (tail-n (car (car tail-paths)))])
          (for-each (lambda (p)
                      (when (> (ref-n (car p)) n)
                        (raise (make nonlinearity #f paths))))
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
             (let ([e2 (wderiv-e2 deriv0)])
               (and e2
                    (substitute-subterms
                     (if path0 (path-replace stx path0 (wderiv-e2 deriv0)) stx)
                     (cdr subterm-derivs)))))]
          [(s:rename? (car subterm-derivs))
           (let ([subterm0 (car subterm-derivs)])
             (substitute-subterms
              (path-replace stx
                            (s:rename-path subterm0)
                            (s:rename-after subterm0))
              (cdr subterm-derivs)))]
          [else (error 'substitute-subterms "neither s:subterm nor s:rename")]))

  ;; wrap/rename : syntax (cons syntax syntax) WDeriv -> WDeriv
  (define (wrap/rename e1 rename d)
    (make p:rename e1 (wderiv-e2 d) null #f rename d))
  
  ;; wrap/rename-from : syntax WDeriv -> WDeriv
  ;; Wrap with renaming: given syntax to initial term of given deriv
  (define (wrap/rename-from e0 d)
    (if (eq? e0 (wderiv-e1 d))
        d
        (wrap/rename e0 (cons e0 (wderiv-e1 d)) d)))
  
  ;; combine-derivs : WDeriv WDeriv -> WDeriv
  ;; Adds the second derivation to the end of the first derivation.
  ;; Inserts a p:rename rule when the final syntax of the first derivation 
  ;; is not identical to the initial syntax of the second.
  (define (combine-derivs head tail)
    ;; head-loop : Derivation -> (values Derivation syntax)
    (define (head-loop head)
      (match head
        [(Wrap mrule (e1 e2 tx next))
         (recv [(next e2) (head-loop next)]
               (values (make mrule e1 e2 tx next)
                       e2))]
        [(Wrap p:variable (e1 e2 rs ?1))
         (adjust-tail e2 rs)]
        ;; FIXME: appropriate?
        [(Wrap p::STOP (e1 e2 rs ?1))
         (adjust-tail e2 rs)]
        [#f (values #f #f)]))
    
    ;; adjust-tail : syntax (list-of syntax) -> (values WDeriv syntax)
    (define (adjust-tail head-e2 head-rs)
      (match tail
        [(Wrap deriv (e1 e2))
         (values (if (eq? head-e2 e1)
                     tail
                     (wrap/rename-from head-e2 tail))
                 e2)]
        [#f (values (make p:stop head-e2 head-e2 head-rs #f)
                    head-e2)]))
    
    (recv [(d s) (head-loop head)]
          d))

  (define (combine/head e1 renames head next)
    (wrap/rename e1 renames (combine-derivs head next)))
  
  (define (reconstruct-defval/head e1 renames head dvvars dvrhs ?1)
    (combine/head e1 renames head
                  (and dvrhs
                       (reconstruct-defval (wderiv-e2 head) dvvars dvrhs ?1))))

  (define (reconstruct-defstx/head e1 renames head dsvars dsrhs ?1)
    (combine/head e1 renames head
                  (and dsrhs
                       (reconstruct-defstx (wderiv-e2 head) dsvars dsrhs ?1))))

  ;; reconstruct-defval : syntax syntax WDeriv -> WDeriv
  ;; Reconstruct a define-values node from its rhs deriv
  (define (reconstruct-defval head-e2 dvvars dvrhs ?1)
    (if (not ?1)
        (let-values ([(def1 def2 renames)
                      (reconstruct-definition-stxs head-e2 dvvars dvrhs)])
          (wrap/rename head-e2 renames
                       (make p:define-values def1 def2 null #f dvrhs)))
        (make p:define-values head-e2 #f null ?1 #f)))

  ;; reconstruct-defstx : syntax syntax Derivation -> Derivation
  (define (reconstruct-defstx head-e2 dsvars bindrhs ?1)
    (if (not ?1)
        (match bindrhs
          [(Wrap bind-syntaxes (rhs ?2))
           (let-values ([(def1 def2 rename)
                         (reconstruct-definition-stxs head-e2 dsvars rhs)])
             (wrap/rename head-e2 rename
                          (make p:define-syntaxes def1 def2 null #f rhs ?2)))])
        (make p:define-syntaxes head-e2 #f null ?1 #f #f)))
  
  ;; reconstruct-definition-stxs : Syntax Syntax WDeriv -> Syntax Syntax Renames
  (define (reconstruct-definition-stxs def0 vars rhs)
    (let ([rhs-e1 (wderiv-e1 rhs)]
          [rhs-e2 (wderiv-e2 rhs)])
      (with-syntax ([(?define ?vars ?rhs) def0]
                    [?vars1 vars]
                    [?rhs1 rhs-e1])
        (define def1
          (syntax/skeleton def0 (?define ?vars1 ?rhs1)))
        (define def2
          (and rhs-e2
               (with-syntax ([?rhs2 rhs-e2])
                 (syntax/skeleton def0 (?define ?vars1 ?rhs2)))))
        (define the-rename
          (cons (cons #'?vars #'?rhs)
                (cons #'?vars1 #'?vars1)))
        (define the-e1 def1)
        (define the-e2 def2)
        (values def1 def2 the-rename))))

  (define (reconstruct-begin/head e1 renames head inners)
    (let* ([inner-es1 (map wderiv-e1 inners)]
           [inner-es2 (wderivlist-es2 inners)]
           [e2 (and inner-es2
                    (with-syntax ([(?begin . ?inner-terms) (wderiv-e2 head)]
                                  [?inner-terms* inner-es2])
                      (syntax/skeleton (wderiv-e2 head)
                                       (?begin . ?inner-terms*))))])
      (wrap/rename e1 renames
                   (combine-derivs
                    head
                    (make p:begin (wderiv-e2 head) e2 null #f
                          (make lderiv inner-es1 inner-es2 #f inners))))))
  
  ;; bderiv->lderiv : WBDeriv -> WLDeriv
  ;; Combines pass1 and pass2 into a single pass(2) list derivation
  (define (bderiv->lderiv bd)
    (match bd
      [#f #f]
      [(Wrap bderiv (es1 _es2 pass1 trans pass2))
       (let-values ([(_dss dvs exprs)
                     (case trans
                       [(letrec)
                        (match pass2
                          [(Wrap lderiv (_ _ #f (list letrec-deriv)))
                           (decompose-letrec letrec-deriv)])]
                       [(list)
                        (match pass2
                          [(Wrap lderiv (_ _ #f derivs))
                           (values null null derivs)]
                          [#f
                           (values null null null)])])]
                    [(brules) pass1]
                    [(suffix) es1])
         (let* ([derivs (brules->derivs brules es1 dvs exprs)]
                [es2 (wderivlist-es2 derivs)])
           (make lderiv es1 es2 #f derivs)))]))

  (define (brules->derivs brules suffix dvs exprs)
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
          (cons #f #f)))

    ;; loop : number -> (list-of WDeriv)
    ;; brules, dvs, exprs, suffix threaded through, so use set!
    ;; dss are all trivial; fully expanded in pass 1
    ;; May not return all of 'count' items
    (define (loop count)
      (if (positive? count)
          (match brules
            [(list (and first (Wrap b:error (exn))))
             (set! brules null)
             (list (make p:unknown suffix #f null exn))]
            [(cons (and first (Wrap b:defvals (renames head ?1))) next)
             (let ([stx (stx-car suffix)]
                   [dv (take-defval)])
               (set! suffix (stx-cdr suffix))
               (set! brules next)
               (cons (reconstruct-defval/head stx renames head
                                              (car dv) (cdr dv) ?1)
                     (loop (sub1 count))))]
            [(cons (and first (Wrap b:defstx (renames head ?1 bindrhs))) next)
             (let ([stx (stx-car suffix)])
               #;(set! _dss (cdr _dss))
               (set! suffix (stx-cdr suffix))
               (set! brules next)
               (let ()
                 (define svars
                   (with-syntax ([(?ds ?svars . ?body) (cdr renames)])
                     #'?svars))
                 (define first*
                   (reconstruct-defstx/head stx renames head svars bindrhs ?1))
                 (cons first* (loop (sub1 count)))))]
            [(cons (Wrap b:splice (renames head #f tail ?2)) next)
             (let ([stx (stx-car suffix)]
                   [n (- (length (stx->list tail))
                         (length (stx->list (stx-cdr suffix))))])
               (set! suffix tail)
               (set! brules next)
               ;; When there's an error after the splice (empty begin),
               ;; then push it as a b:error in the remaining brules.
               (when ?2 (set! brules (cons (make b:error ?2) brules)))
               (let* ([splice-derivs (loop n)])
                 (cons (reconstruct-begin/head stx renames head splice-derivs)
                       (loop (sub1 count)))))]
            [(list (Wrap b:splice (renames head (and exn? ?1) #f #f)))
             (let ([stx (stx-car suffix)])
               (set! suffix null)
               (set! brules null)
               (list (wrap/rename stx renames
                                  (combine-derivs head
                                                  (make p:begin
                                                    (wderiv-e2 head) #f null ?1 #f)))))]
            [(cons (and first (Wrap b:expr (renames head))) next)
             (let ([stx (stx-car suffix)]
                   [expr1 (take-expr)])
               (set! suffix (stx-cdr suffix))
               (set! brules next)
               (cons (wrap/rename stx renames (combine-derivs head expr1))
                     (loop (sub1 count))))]
            ['()
             ;; We've reached the end of pass1 processing.
             ;; We need to pull in exprs to fill out the begin/block shape.
             (let* ([e1 (stx-car suffix)]
                    [expr1 (or (take-expr) (make p:stop e1 e1 null #f))]
                    [expr1-e1 (wderiv-e1 expr1)])
               (set! suffix (stx-cdr suffix))
               (cons (wrap/rename-from e1 expr1)
                     (loop (sub1 count))))])
          ;; Otherwise, we've reached the end, either locally or globally
          null))

    ;; outer-loop : -> (list-of WDeriv)
    (define (outer-loop)
      (if (or (pair? brules) (not (stx-null? suffix)))
          (append (loop 1) (outer-loop))
          null))

    #;(outer-loop)
    ;; FIXME: Need extra +1 in case of improper list?
    (loop (stx-improper-length suffix)))

  ;; module-begin->lderiv : p:#%module-begin -> ??? ListDerivation
  ;; Only use when ?1 is #f.
  (define (module-begin->lderiv pr)
    (let-values ([(forms pass1 pass2)
                  (match pr
                         [(Wrap p:#%module-begin (e1 _ _ #f pass1 pass2 ?2))
                          (values (stx-cdr e1) pass1 pass2)])])
      
      ;; eat-skip : -> void
      (define (eat-skip)
        (match pass2
          [(cons (struct mod:skip ()) next)
           (set! pass2 next)]
          [else (error 'eat-skip "expected skip!")]))

      ;; loop : number -> (list-of WDeriv)
      ;; NOTE: Definitely returns a list of <number> elements; 
      ;; fills the end of the list with #f if necessary.
      (define (loop count)
        ;(printf "** MB->L (~s)~n" count)
        ;(printf "  forms: ~s~n" forms)
        ;(printf "  pass1: ~s~n" pass1)
        (if (positive? count)
            (if (pair? pass1)
                (loop-nz count)
                (cons #f (loop (sub1 count))))
            null))

      ;; loop-nz : number -> (list-of WDeriv)
      (define (loop-nz count)
        (match pass1
          [(cons (Wrap mod:prim (head prim)) next)
           (let ([form0 (stx-car forms)]
                 [pass1-part (car pass1)])
             (set! forms (stx-cdr forms))
             (set! pass1 next)
             (let ([pass2-part (car (loop2 1))])
               (cons (wrap/rename-from form0
                                       (combine-prim pass1-part pass2-part))
                     (loop (sub1 count)))))]
          [(cons (Wrap mod:splice (head ?1 tail)) next)
           (let ([form0 (stx-car forms)]
                 [pass1-part (car pass1)])
             (set! forms tail)
             (set! pass1 next)
             (if (not ?1)
                 (let ([inner-n (- (length (stx->list tail))
                                   (length (stx->list (stx-cdr forms))))])
                   (let ([inners (loop inner-n)])
                     (cons (wrap/rename-from form0 (combine-begin head inners))
                           (loop (sub1 count)))))
                 (combine-derivs head
                                 (make p:begin (wderiv-e2 head) #f null ?1 #f))))]
          [(cons (Wrap mod:lift (head tail)) next)
           (let ([form0 (stx-car forms)]
                 [inner-n (length (stx->list tail))])
             (set! forms (stx-cdr forms))
             (set! pass1 next)
             (let ([inners (loop inner-n)])
               (set! forms (cons (wderiv-e2 head) forms))
               (let ([finish (car (loop 1))])
                 (cons (wrap/rename-from form0 (combine-lifts head finish inners))
                       (loop (sub1 count))))))]
          [(cons (Wrap mod:lift-end (tail)) next)
           ;; FIXME
           ;; Best approach for now: just stop processing here.
           (when (pair? next)
             (warn 'hidden-lift-site/continuing))
           (set! pass1 null)
           (set! forms null)
           null]))

      ;; loop2 : number -> (list-of WDeriv)
      ;; NOTE: Definitely returns a list of <number> elements; 
      ;; fills the end of the list with #f if necessary.
      (define (loop2 count)
        ;(printf "** loop2 (~s)~n" count)
        ;(printf "  forms: ~s~n" forms)
        ;(printf "  pass2: ~s~n" pass2)
        (if (positive? count)
            (match pass2
              [(cons (Wrap mod:skip ()) next)
               (set! pass2 next)
               (cons #f (loop2 (sub1 count)))]
              [(cons (Wrap mod:cons (deriv)) next)
               (set! pass2 next)
               (cons deriv (loop2 (sub1 count)))]
              [(cons (Wrap mod:lift (deriv tail)) next)
               (set! pass2 next)
               (let* ([head-e1 (wderiv-e1 deriv)]
                      [head-e2 (wderiv-e2 deriv)]
                      [inner-n (length tail)]
                      [inners (loop2 inner-n)]
                      [inners-es1 (map wderiv-e1 inners)]
                      [inners-es2 (map wderiv-e2 inners)]
                      [inners-es2 (and (andmap syntax? inners-es2) inners-es2)]
                      [begin-stx1
                       (datum->syntax-object
                        #f
                        `(begin ,@inners-es1 ,(wderiv-e2 deriv)))]
                      [begin-stx2
                       (and inners-es2
                            (datum->syntax-object
                             #f `(begin ,@inners-es2 ,(wderiv-e2 deriv))))])
                 (eat-skip)
                 (cons
                  (make lift-deriv 
                    head-e1 begin-stx2
                    deriv
                    begin-stx1
                    (make p:begin begin-stx1 begin-stx2 null #f
                          (make lderiv
                            (append inners-es1 (list head-e2))
                            (append inners-es2 (list head-e2))
                            #f
                            (append inners
                                    (list (make p:stop head-e2 head-e2 null #f))))))
                  (loop2 (sub1 count))))]
              ['()
               #;(printf "module-body->lderiv:loop2: unexpected null~n")
               (cons #f (loop2 (sub1 count)))])
            null))
      
      (define (outer-loop)
        (if (pair? pass1)
            (append (loop 1) (outer-loop))
            null))
      
      (let* ([derivs (outer-loop)]
             [es1 forms]
             [es2 (wderivlist-es2 derivs)])
        (make lderiv es1 es2 #f derivs))))

  ;; combine-prim : (W MBRule) WDeriv -> WDeriv
  ;; The MRule is always a mod:prim rule.
  ;; Need to insert a rename step in between...
  (define (combine-prim mr deriv)
    (let ([head (mod:prim-head mr)]
          [pr (mod:prim-prim mr)])
      (match pr
        [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
         ;; deriv is #f or trivial
         (combine-derivs head pr)]
        [(Wrap p:define-values (e1 e2 '() ?1 #f))
         ;; deriv is a pderiv for the entire define-values form
         (combine-derivs head deriv)]
        [#f
         ;; deriv is a complete derivation of the rest of the form
         (combine-derivs head deriv)]
        [(Wrap p::STOP (e1 e2 rs ?1))
         ;; deriv is #f
         (combine-derivs head pr)])))
  
  ;; combine-begin : OkDeriv (list-of (W Deriv)) -> WDeriv
  (define (combine-begin head inners)
    (let* ([inners-es1 (map wderiv-e1 inners)]
           [inners-es2 (wderivlist-es2 inners)]
           [begin-e1 (wderiv-e2 head)]
           [begin-e2 (and inners-es2
                          (with-syntax ([(?begin . _) begin-e1]
                                        [inners-es1 inners-es1])
                            (syntax/skeleton begin-e1 (?begin . inners-es1))))])
      (combine-derivs
       head
       (let ([ld (make lderiv inners-es1 inners-es2 #f inners)])
         (make p:begin begin-e1 begin-e2 null #f ld)))))
  
  ;; combine-lifts : OkDeriv WDeriv (list-of WDeriv) -> WDeriv
  (define (combine-lifts head finish inners)
    (let* ([head-e1 (wderiv-e1 head)]
           [head-e2 (wderiv-e2 head)]
           [finish-e1 (wderiv-e1 finish)]
           [finish-e2 (wderiv-e2 finish)]
           [inners-es1 (map wderiv-e1 inners)]
           [inners-es2 (wderivlist-es2 inners)])
      (let ([begin-e1 #`(begin #,@inners-es1 #,head-e2)]
            [begin-e2 (and inners-es2 finish-e2 #`(begin #,@inners-es2 #,finish-e2))])
        (make lift-deriv head-e1 begin-e2
              head
              begin-e1
              (make p:begin begin-e1 begin-e2 null #f
                    (make lderiv
                      (append inners-es1 (list head-e2))
                      (and inners-es2 finish-e2
                           (append inners-es2 (list finish-e2)))
                      #f
                      (append inners
                              (if inners-es2 (list finish) null))))))))


  ;; lderiv->module-begin : ListDerivation Syntax (list-of identifier) -> PRule
  (define (lderiv->module-begin ld e1 rs)
    (match ld
      [(Wrap lderiv (inners-es1 inners-es2 ?1 inners))
       (with-syntax ([(?module-begin . _) e1]
                     [inners-es1* inners-es1]
                     [inners-es2* inners-es2])
         (make p:#%module-begin
           (syntax/skeleton e1 (?module-begin . inners-es1*))
           (syntax/skeleton e1 (?module-begin . inners-es2*))
           rs
           #f
           (map (lambda (d) (make mod:cons d)) inners)
           (map (lambda (x) (make mod:skip)) inners)
           #f))]))

  ;; decompose-letrec : Derivation -> (list-of (cons Syntax Derivation))
  ;;                                  (list-of (cons Syntax Derivation))
  ;;                                  (list-of Derivation)
  ;; Extract the syntax RHS, value RHSs, and expression derivs
  ;; from a block-generated letrec-values or letrec-syntaxes form.
  (define (decompose-letrec deriv)
    (match deriv
      [(Wrap p:letrec-syntaxes+values (_ _ _ #f srenames srhss vrenames vrhss body))
       ;; Assertion: pass1 of the body is always trivial
       (with-syntax ([(([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body) srenames])
         (with-syntax ([(([?vvars* ?vrhs*] ...) . ?body*)
                        (or vrenames #'(([?vvars ?vrhs] ...) . ?body))])
           (values (map cons
                        (syntax->list #'(?svars ...))
                        srhss)
                   (map cons (syntax->list #'(?vvars* ...)) vrhss)
                   (lderiv-derivs (bderiv-pass2 body)))))]
      [(Wrap p:letrec-values (_ _ _ #f vrenames vrhss body))
       ;; Assertion: pass1 of the body is always trivial
       (with-syntax ([(([?vars ?rhs] ...) . ?body) vrenames])
         (values null
                 (map cons (syntax->list #'(?vars ...)) vrhss)
                 (match body
                   [(Wrap bderiv (_ _ _pass1 _ (Wrap lderiv (_ _ ?1 derivs))))
                    derivs]
                   [#f
                    null])))]))

  )
