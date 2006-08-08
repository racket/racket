
(module reductions mzscheme
  (require (lib "plt-match.ss")
           "stx-util.ss"
           "deriv-util.ss"
           "context.ss"
           "deriv.ss"
           "reductions-engine.ss")
  (provide reductions
           (struct step (redex contractum e1 e2 note lctx))
           (struct misstep (redex e1 exn)))

  ;; Setup for reduction-engines

  (define-syntax Expr
    (syntax-id-rules ()
      [Expr (values reductions deriv-e1 deriv-e2)]))
  (define-syntax List
    (syntax-id-rules ()
      [List (values list-reductions lderiv-es1 lderiv-es2)]))
  (define-syntax Block
    (syntax-id-rules ()
      [Block (values block-reductions bderiv-es1 bderiv-es2)]))

  ;; Reductions

  ;; reductions : Derivation -> ReductionSequence
  (define (reductions d)
    (match d
      ;; Primitives
      [(struct p:variable (e1 e2 rs))
       null]
      [(IntQ p:module (e1 e2 rs body))
       (with-syntax ([(?module name language . BODY) e1])
         (let ([ctx (lambda (x) (d->so e1 `(,#'?module ,#'name ,#'language ,x)))]
               [body-e1 (match body [($$ deriv (body-e1 _) _) body-e1])])
           (cons (walk e1 (ctx body-e1) "Tag #%module-begin")
                 (with-context ctx
                   (reductions body)))))]
      [(IntQ p:#%module-begin (e1 e2 rs pass1 pass2))
       #;(R e1 (?module-begin . MBODY)
            [! exni 'blah]
            [ModulePass1 MBODY pass1]
            => (lambda (e1prime)
                 (R e1prime (?module-begin2 . MBODY2)
                    [ModulePass2 MBODY2 pass2])))
       (with-syntax ([(?#%module-begin form ...) e1])
         (let-values ([(reductions1 final-stxs1)
                       (with-context (lambda (x) (d->so e1 (cons #'?#%module-begin x)))
                         (mbrules-reductions pass1 (syntax->list #'(form ...)) #t))])
           (let-values ([(reductions2 final-stxs2)
                         (with-context (lambda (x) (d->so e1 (cons #'?#%module-begin x)))
                           (mbrules-reductions pass2 final-stxs1 #f))])
             (append reductions1 reductions2))))]
      [(AnyQ p:define-syntaxes (e1 e2 rs rhs) exni)
       (R e1 _
          [! exni]
          [#:pattern (?define-syntaxes formals RHS)]
          [Expr RHS rhs])]
      [(AnyQ p:define-values (e1 e2 rs rhs) exni)
       (R e1 _
          [! exni]
          [#:pattern (?define-values formals RHS)]
          [Expr RHS rhs])]
      [(AnyQ p:if (e1 e2 rs full? test then else) exni)
       (if full?
           (R e1 _
              [! exni]
              [#:pattern (?if TEST THEN ELSE)]
              [Expr TEST test]
              [Expr THEN then]
              [Expr ELSE else])
           (R e1 _
              [! exni]
              [#:pattern (?if TEST THEN)]
              [Expr TEST test]
              [Expr THEN then]))]
      [(AnyQ p:wcm (e1 e2 rs key mark body) exni)
       (R e1 _
          [! exni]
          [#:pattern (?wcm KEY MARK BODY)]
          [Expr KEY key]
          [Expr MARK mark]
          [Expr BODY body])]
      [(AnyQ p:begin (e1 e2 rs lderiv) exni)
       (R e1 _
          [! exni]
          [#:pattern (?begin . LDERIV)]
          [List LDERIV lderiv])]
      [(AnyQ p:begin0 (e1 e2 rs first lderiv) exni)
       (R e1 _
          [! exni]
          [#:pattern (?begin0 FIRST . LDERIV)]
          [Expr FIRST first]
          [List LDERIV lderiv])]
      [(AnyQ p:#%app (e1 e2 rs tagged-stx lderiv) exni)
       (let ([tail
              (R tagged-stx (?#%app . LDERIV)
                 [! exni]
                 [List LDERIV lderiv])])
         (if (eq? tagged-stx e1)
             tail
             (cons (walk e1 tagged-stx "Tag application") tail)))]
      [(AnyQ p:lambda (e1 e2 rs renames body) exni)
       (R e1 _
          [! exni]
          [#:bind (?formals* . ?body*) renames]
          [#:pattern (?lambda ?formals . ?body)]
          [#:walk (syntax/skeleton e1 (?lambda ?formals* . ?body*))
                  #'?formals #'?formals*
                  "Rename formal parameters"]
          [Block ?body body])
       #;
       (R e1 _1
          [! exni]
          => 
          (lambda (stx)
            (with-syntax ([(?lambda ?formals . ?body) stx]
                          [(?formals* . ?body*) renames])
              (let ([mid (syntax/skeleton e1 (?lambda ?formals* . ?body*))])
                (append 
                 (if (stx-pair? #'?formals)
                     (list (walk/foci/E #'?formals #'?formals* e1 mid
                                        "Rename formal parameters"))
                     null)
                 (R mid (LAMBDA FORMALS . BODY)
                    [Block BODY body]))))))
       #;(with-syntax ([(?lambda ?formals . ?body) e1]
                       [(?formals* . ?body*) renames])
           (let ([mid (syntax/skeleton e1 (?lambda ?formals* . ?body*))])
             (append
              (if (stx-pair? #'?formals)
                  (list (walk/foci/E #'?formals #'?formals* e1 mid
                                     "Rename formal parameters"))
                  null)
              (R mid (LAMBDA FORMALS . BODY)
                 [Block BODY body]))))]
      [(struct p:case-lambda (e1 e2 rs renames+bodies))
       #;
       (R e1 _
          [! exni]
          [#:pattern (?case-lambda [?formals . ?body] ...)]
          [#:bind [(?formals* . ?body*) ...] (map car renames+bodies)]
          [#:walk (syntax/skeleton e1 (?case-lambda [?formals* . ?body*] ...))
                  (syntax->list #'(?formals ...))
                  (syntax->list #'(?formals* ...))
                  "Rename formal parameters"]
          [Block (?body ...) (map cdr renames+bodies)])
       (with-syntax ([(?case-lambda [?formals . ?body] ...) e1]
                     [((?formals* . ?body*) ...) (map car renames+bodies)])
         (let ([mid (syntax/skeleton e1 (?case-lambda [?formals* . ?body*] ...))])
           (cons (walk/foci/E (syntax->list #'(?formals ...))
                              (syntax->list #'(?formals* ...))
                              e1 mid "Rename formal parameters")
                 (R mid (CASE-LAMBDA [FORMALS . BODY] ...)
                    [Block (BODY ...) (map cdr renames+bodies)]))))]
      [(AnyQ p:let-values (e1 e2 rs renames rhss body) exni)
       (R e1 _
          [! exni]
          [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
          [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
          [#:walk (syntax/skeleton e1 (?let-values ([?vars* ?rhs*] ...) . ?body*))
                  (syntax->list #'(?vars ...))
                  (syntax->list #'(?vars* ...))
                  "Rename bound variables"]
          [Expr (?rhs ...) rhss]
          [Block ?body body])
       #;
       (with-syntax ([(?let-values ([?vars ?rhs] ...) . ?body) e1]
                     [(([?vars* ?rhs*] ...) . ?body*) renames])
         (let ([mid (syntax/skeleton e1 (?let-values ([?vars* ?rhs*] ...) . ?body*))])
           (cons (walk/foci/E (syntax->list #'(?vars ...))
                              (syntax->list #'(?vars* ...))
                              e1 mid "Rename let-bound variables")
                 (R mid (LET-VALUES ([VARS RHS] ...) . BODY)
                    [Expr (RHS ...) rhss]
                    [Block BODY body]))))]
      [(AnyQ p:letrec-values (e1 e2 rs renames rhss body) exni)
       (R e1 _
          [! exni]
          [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
          [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
          [#:walk (syntax/skeleton e1 (?letrec-values ([?vars* ?rhs*] ...) . ?body*))
                  (syntax->list #'(?vars ...))
                  (syntax->list #'(?vars* ...))
                  "Rename bound variables"]
          [Expr (?rhs ...) rhss]
          [Block ?body body])
       #;
       (with-syntax ([(?letrec-values ([?vars ?rhs] ...) . ?body) e1]
                     [(([?vars* ?rhs*] ...) . ?body*) renames])
         (let ([mid (syntax/skeleton e1 (?letrec-values ([?vars* ?rhs*] ...) . ?body*))])
           (cons (walk/foci/E (syntax->list #'(?vars ...))
                              (syntax->list #'(?vars* ...))
                              e1 mid "Rename letrec-bound variables")
                 (R mid (LETREC-VALUES ([VARS RHS] ...) . BODY)
                    [Expr (RHS ...) rhss]
                    [Block BODY body]))))]

      [(AnyQ p:letrec-syntaxes+values (e1 e2 rs srenames srhss vrenames vrhss body) exni)
       (R e1 _
          [! exni]
          [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
          [#:bind (([?svars* ?srhs*] ...) ([?vvars* ?vrhs] ...) . ?body*) srenames]
          [#:walk (syntax/skeleton e1 (?lsv ([?svars* ?srhs*] ...) ([?vvars* ?vrhs*] ...) . ?body*))
                  (syntax->list #'(?svars ...))
                  (syntax->list #'(?svars* ...))
                  "Rename bound variables"]
          [Expr (?srhs ...) srhss]
          [#:bind (([?vvars** ?vrhs**] ...) . ?body**) vrenames]
          [#:walk (syntax/skeleton e1 (?lsv ([?svars* ?srhs*] ...) ([?vars** ?vrhs**] ...) . ?body**))
                  (syntax->list #'(?vvars* ...))
                  (syntax->list #'(?vvars** ...))
                  "Rename bound variables"]
          [Expr (?vrhs ...) vrhss]
          [Block ?body body]
          => (lambda (mid)
               (if (eq? mid e2)
                   null
                   (list (walk mid e2 "Remove syntax bindings")))))
       #;
       (with-syntax ([(?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body) e1]
                     [(([?svars* ?srhs*] ...) ?vpart* . ?body*) srenames])
         (with-syntax ([(([?vvars* ?vrhs*] ...) . ?body**) 
                        (or vrenames #'(?vpart* . ?body*))])
           (let ([mid (syntax/skeleton
                       e1
                       (?lsv ([?svars* ?srhs*] ...) ([?vvars* ?vrhs] ...) . ?body**))])
             (cons 
              (walk/foci/E (syntax->list #'(?svars ... ?vvars ...))
                           (syntax->list #'(?svars* ... ?vvars* ...))
                           e1 mid "Rename local variables")
              (R mid (LETREC-SYNTAXES+VALUES ([SVARS SRHS] ...) ([VVARS VRHS] ...) . BODY)
                 [Expr (SRHS ...) srhss]
                 [Expr (VRHS ...) vrhss]
                 [Block BODY body]
                 => (lambda (mid)
                      (if (eq? mid e2)
                          null
                          (list (walk mid e2 "Finish letrec-syntaxes+values")))))))))]

      ;; The auto-tagged atomic primitives
      [(AnyQ p:#%datum (e1 e2 rs tagged-stx) exni)
       (append (if (eq? e1 tagged-stx)
                   null
                   (list (walk e1 tagged-stx "Tag datum")))
               (if exni
                   (list (stumble tagged-stx (car exni)))
                   null))]
      [(AnyQ p:#%top (e1 e2 rs tagged-stx) exni)
       (append (if (eq? e1 tagged-stx)
                   null
                   (list (walk e1 tagged-stx "Tag top-level variable")))
               (if exni
                   (list (stumble tagged-stx (car exni)))
                   null))]
      
      ;; The rest of the automatic primitives
      [(AnyQ p::STOP (e1 e2 rs) exni)
       (R e1 _
          [! exni])]

      [(AnyQ p:set!-macro (e1 e2 rs deriv) exni)
       (R e1 _
          [! exni]
          => (lambda (mid)
               (reductions deriv)))]
      [(AnyQ p:set! (e1 e2 rs id-rs rhs) exni)
       (R e1 _
          [! exni]
          [#:pattern (SET! VAR RHS)]
          [Expr RHS rhs])]

      ;; Synthetic primitives
      ;; These have their own subterm replacement mechanisms
      [(and d (AnyQ p:synth (e1 e2 rs subterms)))
       (let loop ([term e1] [subterms subterms])
         (cond [(null? subterms) 
                (let ([exn (and (error-wrap? d) (error-wrap-exn d))])
                  (if exn
                      (list (stumble term exn))
                      null))]
               [(pair? subterms)
                (let* ([subterm0 (car subterms)]
                       [path0 (s:subterm-path subterm0)]
                       [deriv0 (s:subterm-deriv subterm0)])
                  (let ([ctx (lambda (x) (path-replace term path0 x))])
                    (append (with-context ctx
                              (reductions deriv0))
                            (loop (path-replace term path0 (deriv-e2 deriv0))
                                  (cdr subterms)))))]))]

      ;; FIXME
      [(IntQ p:rename (e1 e2 rs rename inner))
       (reductions inner)]

      ;; Error

;      [(struct error-wrap (exn tag (? prule? prule)))
;       ;; Let's take the attitude that all primitive syntax errors
;       ;; occur "at the beginning"
;       (list (make-misstep (deriv-e1 prule) (E (deriv-e1 prule)) exn))]
;      
;      #;
;      [($$ interrupted-wrap (tag prule))
;       (reductions prule orig-stx)]

      ;; Macros
      [(IntQ mrule (e1 e2 transformation next))
       (append (reductions-transformation transformation)
               (reductions next))]

      ;; Lifts
      
      [(IntQ lift-deriv (e1 e2 first lifted-stx second))
       (append (reductions first)
               (list (walk (deriv-e2 first) lifted-stx "Capture lifts"))
               (reductions second))]

      ;; Skipped
      
      [#f null]))

  ;; reductions-transformation : Transformation -> ReductionSequence
  (define (reductions-transformation tx)
    (match tx
      [(struct transformation (e1 e2 rs me1 me2 locals))
       (append (reductions-locals e1 locals)
               (list (walk e1 e2 "Macro transformation")))]
      [(IntW transformation (e1 e2 rs me1 me2 locals) 'locals)
       (reductions-locals e1 locals)]
      [(ErrW transformation (e1 e2 rs me1 me2 locals) 'transform exn)
       (append (reductions-locals e1 locals)
               (list (stumble e1 exn)))]))

  ;; reductions-locals : syntax (list-of LocalAction) -> ReductionSequence
  (define (reductions-locals stx locals)
    (with-new-local-context stx
      (apply append (map reductions-local locals))))

  ;; reductions-local : LocalAction -> ReductionSequence
  (define (reductions-local local)
    (match local
      [(IntQ local-expansion (e1 e2 me1 me2 deriv))
       (reductions deriv)]
      [(struct local-lift (expr id))
       (list (walk expr id "Macro lifted expression to top-level"))]
      [(struct local-lift-end (decl))
       (list (walk decl decl "Declaration lifted to end of module"))]))

  ;; list-reductions : ListDerivation -> ReductionSequence
  (define (list-reductions ld)
    (match ld
      [(IntQ lderiv (es1 es2 derivs))
       (let loop ([derivs derivs] [suffix es1])
         (cond [(pair? derivs)
                (append
                 (with-context (lambda (x) (cons x (stx-cdr suffix)))
                   (reductions (car derivs)))
                 (with-context (lambda (x) (cons (deriv-e2 (car derivs)) x))
                   (loop (cdr derivs) (stx-cdr suffix))))]
               [(null? derivs)
                null]))]
      [(ErrW lderiv (es1 es2 derivs) _ exn)
       (list (stumble es1 exn))]

      [#f null]))

  ;; block-reductions : BlockDerivation -> ReductionSequence
  (define (block-reductions bd)
    (match bd
      ;; If interrupted in pass1, skip pass2
      [(IntW bderiv (es1 es2 pass1 trans pass2) 'pass1)
       (let-values ([(reductions stxs) (brules-reductions pass1 es1)])
         reductions)]
      ;; Otherwise, do both
      [(IntQ bderiv (es1 es2 pass1 trans pass2))
       (let-values ([(reductions1 stxs1) (brules-reductions pass1 es1)])
         (append reductions1
                 (if (eq? trans 'letrec)
                     (match pass2
                       [($$ lderiv (pass2-es1 _ _) _exni)
                        (list (walk stxs1 pass2-es1 "Transform block to letrec"))])
                     null)
                 (list-reductions pass2)))]
      [#f null]))

  ;; brules-reductions : (list-of-BRule) syntax-list -> ReductionSequence syntax-list
  (define (brules-reductions brules all-stxs)
    (let loop ([brules brules] [suffix all-stxs] [prefix null] [rss null])
      (match brules
        [(cons (struct b:expr (renames head)) next)
         (let ([estx (deriv-e2 head)])
           (loop next (stx-cdr suffix) (cons estx prefix)
                 (cons (with-context (lambda (x)
                                       (revappend prefix (cons x (stx-cdr suffix))))
                         (reductions head))
                       rss)))]
        [(cons (IntW b:expr (renames head) tag) '())
         (loop '() #f #f 
               (cons (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                                   (reductions head))
                     rss))]
        [(cons (struct b:defvals (renames head)) next)
         (let ([head-rs 
                (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                  (reductions head))])
           (loop next (stx-cdr suffix) (cons (deriv-e2 head) prefix)
                 (cons head-rs rss)))]
        [(cons ($$ b:defstx (renames head rhs) _exni) next)
         (let* ([estx (deriv-e2 head)]
                [estx2 (with-syntax ([(?ds ?vars ?rhs) estx]
                                     [?rhs* (deriv-e2 rhs)])
                         ;;FIXME
                         #'(?ds ?vars ?rhs*))])
           (loop next (cdr suffix) (cons estx2 prefix)
                 (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                               (cons (with-context (CC (?ds ?vars ?rhs) estx ?rhs)
                                                   (reductions rhs))
                                     (cons (reductions head)
                                           rss)))))]
        [(cons (struct b:splice (renames head tail)) next)
         (loop next tail prefix
               (cons (list (walk/foci (deriv-e2 head)
                                      (take-until tail (stx-cdr suffix))
                                      (E (revappend prefix 
                                                    (cons (deriv-e2 head) (stx-cdr suffix))))
                                      (E (revappend prefix tail))
                                      "Splice block-level begin"))
                     (cons (with-context (lambda (x) 
                                           (revappend prefix (cons x (stx-cdr suffix))))
                             (reductions head))
                           rss)))]
        [(cons (struct b:begin (renames head derivs)) next)
         ;; FIXME
         (error 'unimplemented)]
        [(cons (struct error-wrap (exn tag _inner)) '())
         (values (list (make-misstep suffix (E (revappend prefix suffix)) exn))
                 (revappend prefix suffix))]
        ['()
         (values (apply append (reverse rss))
                 (revappend prefix suffix))])))

  ;; mbrules-reductions : MBRules (list-of syntax) -> ReductionSequence
  ;; The reprocess-on-lift? argument controls the behavior of a mod:lift event.
  ;; In Pass1, #t; in Pass2, #f.
  (define (mbrules-reductions mbrules all-stxs reprocess-on-lift?)
    ;(printf "**** MB Reductions, pass ~s~n" (if reprocess-on-lift? 1 2))
    (let* ([final-stxs #f]
           [reductions
            (let loop ([mbrules mbrules] [suffix all-stxs] [prefix null])
              ;(printf "** MB loop~n")
              ;(printf "  rules: ~s~n" mbrules)
              ;(printf "  suffix: ~s~n" suffix)
              ;(printf "  prefix: ~s~n" prefix)
              (match mbrules
                [(cons ($$ mod:skip ()) next)
                 (loop next (stx-cdr suffix) (cons (stx-car suffix) prefix))]
                [(cons ($$ mod:cons (head) _exni) next)
                 (append (with-context (lambda (x) 
                                         (revappend prefix (cons x (stx-cdr suffix))))
                                       (append (reductions head)))
                         (let ([estx (and (deriv? head) (deriv-e2 head))])
                           (loop next (stx-cdr suffix) (cons estx prefix))))]
                [(cons ($$ mod:prim (head prim) _exni) next)
                 (append (with-context (lambda (x)
                                         (revappend prefix (cons x (stx-cdr suffix))))
                           (if (and prim (not (p:define-values? prim)))
                               (append (reductions head)
                                       (reductions prim))
                               (reductions head)))
                         (let ([estx (and (deriv? head) (deriv-e2 head))])
                           (loop next (stx-cdr suffix) (cons estx prefix))))]
                [(cons ($$ mod:splice (head stxs)) next)
                 ;(printf "suffix is: ~s~n~n" suffix)
                 ;(printf "stxs is: ~s~n" stxs)
                 (append
                  (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                                (reductions head))
                  (let ([suffix-tail (stx-cdr suffix)]
                        [head-e2 (deriv-e2 head)])
                    (cons (walk/foci head-e2
                                     (stx-take stxs
                                               (- (stx-improper-length stxs)
                                                  (stx-improper-length suffix-tail)))
                                     (E (revappend prefix (cons head-e2 suffix-tail)))
                                     (E (revappend prefix stxs))
                                     "Splice module-level begin")
                          (loop next stxs prefix))))]
                [(cons ($$ mod:lift (head stxs)) next)
                 ;(printf "suffix is: ~s~n~n" suffix)
                 ;(printf "stxs is: ~s~n" stxs)
                 (append
                  (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                                (reductions head))
                  (let ([suffix-tail (stx-cdr suffix)]
                        [head-e2 (deriv-e2 head)])
                    (let ([new-suffix (append stxs (cons head-e2 suffix-tail))])
                      (cons (walk/foci null
                                       stxs
                                       (E (revappend prefix (cons head-e2 suffix-tail)))
                                       (E (revappend prefix new-suffix))
                                       "Splice definitions from lifted expressions")
                            (loop next
                                  new-suffix
                                  prefix)))))]
                [(cons ($$ mod:lift-end (tail)) next)
                 (append
                  (if (pair? tail)
                      (list (walk/foci null
                                       tail
                                       (E (revappend prefix suffix))
                                       (E (revappend prefix tail))
                                       "Splice lifted module declarations"))
                      null)
                  (loop next tail prefix))]
                ['()
                 (set! final-stxs (reverse prefix))
                 null]))])
      (values reductions final-stxs)))
  
  
  )
