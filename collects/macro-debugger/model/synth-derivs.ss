
(module synth-derivs mzscheme
  (require (lib "plt-match.ss")
           (lib "list.ss")
           "deriv.ss"
           "deriv-util.ss"
           "synth-engine.ss"
           "stx-util.ss"
           "context.ss")
  (provide (all-defined))

  ;; machinery for reporting things that macro hiding can't handle
  (define-struct nonlinearity (message paths))
  (define-struct localactions ())

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
                     (wrap/rename-from head-e2 tail))
                 e2)]
        [#f (values (make-p:stop head-e2 head-e2 head-rs)
                    head-e2)]))

    (recv [(d s) (head-loop head)]
          d))

  ;; wrap-p:rename : syntax (cons syntax syntax) Derivation -> Derivation
  (define (wrap-p:rename e1 rename deriv)
    (make-p:rename e1 (lift/deriv-e2 deriv) null rename deriv))

  ;; wrap-rename : syntax (cons syntax syntax) Derivation -> Derivation
  (define (wrap-rename e1 rename deriv)
    (outer-rewrap deriv (wrap-p:rename e1 rename deriv)))

  ;; wrap/rename-from : syntax Derivation -> Derivation
  ;; Wrap with renaming: given syntax to initial term of given deriv
  (define (wrap/rename-from e0 d)
    (match d
      [(AnyQ deriv (e1 e2))
       (outer-rewrap d (wrap-p:rename e0 (cons e0 e1) d))]))
  
  ;; reconstruct-defval : syntax syntax Derivation -> Derivation
  ;; Reconstruct a define-values node from its rhs deriv
  (define (reconstruct-defval head-e2 dvvars dvrhs)
    (reconstruct-definition-form head-e2 dvvars dvrhs make-p:define-values))

  ;; reconstruct-defstx : syntax syntax Derivation -> Derivation
  (define (reconstruct-defstx head-e2 dsvars dsrhs)
    (reconstruct-definition-form head-e2 dsvars dsrhs make-p:define-syntaxes))

  (define (reconstruct-definition-form head-e2 dvvars dvrhs make-Definition)
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
           (wrap-rename dv1
                        (cons (cons #'?vars #'?rhs)
                              (cons #'?vars* #'?rhs*))
                        (outer-rewrap dvrhs
                                      (make-Definition dv1* dv2 null dvrhs)))))]))

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
                          [(AnyQ lderiv (_ _ derivs))
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
                  (let ([stx (stx-car suffix)])
                    (set! _dss (cdr _dss))
                    (set! suffix (stx-cdr suffix))
                    (set! brules next)
                    (let* ([svars 
                            (with-syntax ([(?ds ?svars . ?body) (cdr renames)])
                              #'?svars)]
                           [finish (reconstruct-defstx (deriv-e2 head) svars rhs)])
                      (cons (make-b:expr renames (combine-derivs head finish))
                            (loop (sub1 count)))))]
                 [(cons (struct b:splice (renames head tail)) next)
                  (let ([n (- (length (stx->list tail))
                              (length (stx->list (stx-cdr suffix))))])
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
              (wrap-rename stx renames head)]
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

         (let* ([brules (loop (stx-improper-length es1))]
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
                 (eat-skip)
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

      ;; eat-skip : -> void
      (define (eat-skip)
        (match pass2
          [(cons (struct mod:skip ()) next)
           (set! pass2 next)]
          [else (error 'eat-skip "expected skip!")]))

      (let* ([derivs (loop (stx-improper-length forms))]
             [es1 (map lift/deriv-e1 derivs)]
             [es2 (if (wrapped? pr) #f (map lift/deriv-e2 derivs))])
        (rewrap pr (make-lderiv es1 es2 derivs)))))

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

  
  ;; decompose-letrec : Derivation -> (values DerivList
  ;;                                          (list-of (cons Syntax Derivation))
  ;;                                          (list-of (cons Syntax Derivation))
  ;; Extract the syntax RHS, value RHSs, and expression derivs
  ;; from a block-generated letrec-values or letrec-syntaxes form.
  (define (decompose-letrec deriv)
    (match deriv
      [(IntQ p:letrec-syntaxes+values (_ _ _ srenames srhss vrenames vrhss body))
       ;; Assertion: pass1 of the body is always trivial
       (with-syntax ([(([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body) srenames])
         (with-syntax ([(([?vvars* ?vrhs*] ...) . ?body*)
                        (or vrenames #'(([?vvars ?vrhs] ...) . ?body))])
           (values (map cons
                        (syntax->list #'(?svars ...))
                        srhss)
                   (map cons (syntax->list #'(?vvars* ...)) vrhss)
                   (lderiv-derivs (bderiv-pass2 body)))))]
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

  
  )