
(module reductions mzscheme
  (require (lib "plt-match.ss")
           "stx-util.ss"
           "deriv-util.ss"
           "context.ss"
           "deriv.ss"
           "reductions-engine.ss")

  (provide reductions
           reductions+definites)

  ;; Setup for reduction-engines

  (define-syntax Expr
    (syntax-id-rules ()
      [Expr (values reductions* deriv-e1 deriv-e2)]))
  (define-syntax List
    (syntax-id-rules ()
      [List (values list-reductions lderiv-es1 lderiv-es2)]))
  (define-syntax Block
    (syntax-id-rules ()
      [Block (values block-reductions bderiv-es1 bderiv-es2)]))

  ;; Syntax

  (define-syntax match/with-derivation
    (syntax-rules ()
      [(match/with-derivation d . clauses)
       (let ([dvar d])
         (with-derivation dvar
           (match dvar . clauses)))]))
  
  ;; Reductions

  ;; reductions : Derivation -> ReductionSequence
  (define (reductions d)
    (parameterize ((current-definites null)
                   (current-frontier null))
      (when d (add-frontier (list (lift/deriv-e1 d))))
      (reductions* d)))

  (define (reductions+definites d)
    (parameterize ((current-definites null)
                   (current-frontier null))
      (when d (add-frontier (list (lift/deriv-e1 d))))
      (let ([rs (reductions* d)])
        (values rs (current-definites)))))

  (define (reductions* d)
    (match d
      [(AnyQ prule (e1 e2 rs))
       (and rs (learn-definites rs))
       (blaze-frontier e1)]
      [_ (void)])
    (match/with-derivation d
      ;; Primitives
      [(struct p:variable (e1 e2 rs))
       (learn-definites (list e2))
       (if (bound-identifier=? e1 e2)
           null
           (list (walk e1 e2 'resolve-variable)))]
      [(IntQ p:module (e1 e2 rs #f body))
       (with-syntax ([(?module name language . BODY) e1])
         (let ([ctx (lambda (x) (d->so e1 `(,#'?module ,#'name ,#'language ,x)))]
               [body-e1 (match body [(AnyQ deriv (body-e1 _)) body-e1])])
           (cons (walk e1 (ctx body-e1) 'tag-module-begin)
                 (with-context ctx
                   (add-frontier (list (lift/deriv-e1 body)))
                   (reductions* body)))))]
      [(IntQ p:module (e1 e2 rs #t body))
       (with-syntax ([(?module name language . BODY) e1])
         (let ([ctx (lambda (x) (d->so e1 `(,#'?module ,#'name ,#'language ,x)))])
           (with-context ctx
             (add-frontier (list (lift/deriv-e1 body)))
             (reductions* body))))]
      [(AnyQ p:#%module-begin (e1 e2 rs pass1 pass2))
       (with-syntax ([(?#%module-begin form ...) e1])
         (let ([frame (lambda (x) (d->so e1 (cons #'?#%module-begin x)))])
           (let-values ([(reductions1 final-stxs1)
                         (with-context frame
                           (add-frontier (syntax->list #'(form ...)))
                           (mbrules-reductions pass1 (syntax->list #'(form ...)) #t))])
             (let-values ([(reductions2 final-stxs2)
                           (with-context frame
                             ;(add-frontier final-stxs1)
                             (mbrules-reductions pass2 final-stxs1 #f))])
               (if (error-wrap? d)
                   (append reductions1 reductions2 
                           (list (stumble (frame final-stxs2) (error-wrap-exn d))))
                   (append reductions1 reductions2))))))]
      [(AnyQ p:define-syntaxes (e1 e2 rs rhs) exni)
       (R e1
          [! exni]
          [#:pattern (?define-syntaxes formals RHS)]
          [#:frontier (list #'RHS)]
          [Expr RHS rhs])]
      [(AnyQ p:define-values (e1 e2 rs rhs) exni)
       (R e1
          [! exni]
          [#:pattern (?define-values formals RHS)]
          [#:frontier (list #'RHS)]
          [#:if rhs
                [Expr RHS rhs]])]
      [(AnyQ p:expression (e1 e2 rs inner) exni)
       (R e1
          [! exni]
          [#:pattern (?expr INNER)]
          [Expr INNER inner])]
      [(AnyQ p:if (e1 e2 rs full? test then else) exni)
       (if full?
           (R e1
              [! exni]
              [#:pattern (?if TEST THEN ELSE)]
              [#:frontier (list #'TEST #'THEN #'ELSE)]
              [Expr TEST test]
              [Expr THEN then]
              [Expr ELSE else])
           (R e1
              [! exni]
              [#:pattern (?if TEST THEN)]
              [#:frontier (list #'TEST #'THEN)]
              [Expr TEST test]
              [Expr THEN then]))]
      [(AnyQ p:wcm (e1 e2 rs key mark body) exni)
       (R e1
          [! exni]
          [#:pattern (?wcm KEY MARK BODY)]
          [#:frontier (list #'KEY #'MARK #'BODY)]
          [Expr KEY key]
          [Expr MARK mark]
          [Expr BODY body])]
      [(AnyQ p:begin (e1 e2 rs lderiv) exni)
       (R e1
          [! exni]
          [#:pattern (?begin . LDERIV)]
          [#:frontier (stx->list* #'LDERIV)]
          [List LDERIV lderiv])]
      [(AnyQ p:begin0 (e1 e2 rs first lderiv) exni)
       (R e1
          [! exni]
          [#:pattern (?begin0 FIRST . LDERIV)]
          [#:frontier (cons #'FIRST (stx->list* #'LDERIV))]
          [Expr FIRST first]
          [List LDERIV lderiv])]
      [(AnyQ p:#%app (e1 e2 rs tagged-stx lderiv) exni)
       (let ([tail
              (R tagged-stx
                 [! exni]
                 [#:pattern (?#%app . LDERIV)]
                 [#:frontier (stx->list* #'LDERIV)]
                 [List LDERIV lderiv])])
         (if (eq? tagged-stx e1)
             tail
             (cons (walk e1 tagged-stx 'tag-app) tail)))]
      [(AnyQ p:lambda (e1 e2 rs renames body) exni)
       (R e1
          [! exni]
          [#:bind (?formals* . ?body*) renames]
          [#:pattern (?lambda ?formals . ?body)]
          [#:frontier (stx->list* #'?body)]
          [#:rename (syntax/skeleton e1 (?lambda ?formals* . ?body*))
                    #'?formals #'?formals*
                    'rename-lambda]
          [Block ?body body])]
      [(struct p:case-lambda (e1 e2 rs renames+bodies))
       #;
       (R e1
          [! exni]
          [#:pattern (?case-lambda [?formals . ?body] ...)]
          ;; FIXME: frontier
          [#:bind [(?formals* . ?body*) ...] (map car renames+bodies)]
          [#:rename
           (syntax/skeleton e1 (?case-lambda [?formals* . ?body*] ...))
           (syntax->list #'(?formals ...))
           (syntax->list #'(?formals* ...))
           'rename-case-lambda]
          [Block (?body ...) (map cdr renames+bodies)])
       (with-syntax ([(?case-lambda [?formals . ?body] ...) e1]
                     [((?formals* . ?body*) ...) (map car renames+bodies)])
         (add-frontier (apply append (map stx->list* (syntax->list #'(?body ...)))))
         (let ([mid (syntax/skeleton e1 (?case-lambda [?formals* . ?body*] ...))])
           (rename-frontier #'(?formals ...) #'(?formals* ...))
           (cons (walk/foci (syntax->list #'(?formals ...))
                            (syntax->list #'(?formals* ...))
                            e1 mid 'rename-case-lambda)
                 ;; FIXME: Missing renames frames here
                 (R mid
                    [#:pattern (CASE-LAMBDA [FORMALS . BODY] ...)]
                    [Block (BODY ...) (map cdr renames+bodies)]))))]
      [(AnyQ p:let-values (e1 e2 rs renames rhss body) exni)
       (R e1
          [! exni]
          [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
          [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
          [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
          [#:rename
           (syntax/skeleton e1 (?let-values ([?vars* ?rhs*] ...) . ?body*))
           (syntax->list #'(?vars ...))
           (syntax->list #'(?vars* ...))
           'rename-let-values]
          [Expr (?rhs ...) rhss]
          [Block ?body body])]
      [(AnyQ p:letrec-values (e1 e2 rs renames rhss body) exni)
       (R e1
          [! exni]
          [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
          [#:frontier (append (syntax->list #'(?rhs ...)) (stx->list* #'?body))]
          [#:bind (([?vars* ?rhs*] ...) . ?body*) renames]
          [#:rename
           (syntax/skeleton e1 (?letrec-values ([?vars* ?rhs*] ...) . ?body*))
           (syntax->list #'(?vars ...))
           (syntax->list #'(?vars* ...))
           'rename-letrec-values]
          [Expr (?rhs ...) rhss]
          [Block ?body body])]
      [(AnyQ p:letrec-syntaxes+values
             (e1 e2 rs srenames srhss vrenames vrhss body) exni)
       (R e1
          [! exni]
          [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
          [#:frontier (append (syntax->list #'(?srhs ...))
                              (syntax->list #'(?vrhs ...))
                              (stx->list* #'?body))]
          [#:bind (([?svars* ?srhs*] ...) ([?vvars* ?vrhs*] ...) . ?body*) srenames]
          [#:rename
           (syntax/skeleton e1
                            (?lsv ([?svars* ?srhs*] ...) ([?vvars* ?vrhs*] ...)
                              . ?body*))
           (syntax->list #'(?svars ...))
           (syntax->list #'(?svars* ...))
           'rename-lsv]
          [Expr (?srhs ...) srhss]
          ;; If vrenames is #f, no var bindings to rename
          [#:if vrenames
                [#:bind (([?vvars** ?vrhs**] ...) . ?body**) vrenames]
                [#:rename
                 (syntax/skeleton e1 (?lsv ([?svars* ?srhs*] ...)
                                           ([?vvars** ?vrhs**] ...)
                                        . ?body**))
                 (syntax->list #'(?vvars* ...))
                 (syntax->list #'(?vvars** ...))
                 'rename-lsv]]
          [Expr (?vrhs ...) vrhss]
          [Block ?body body]
          => (lambda (mid)
               (list (walk mid e2 'lsv-remove-syntax))))]
      ;; The auto-tagged atomic primitives
      [(AnyQ p:#%datum (e1 e2 rs tagged-stx) exni)
       (append (if (eq? e1 tagged-stx)
                   null
                   (list (walk e1 tagged-stx 'tag-datum)))
               (if exni
                   (list (stumble tagged-stx (car exni)))
                   null))]
      [(AnyQ p:#%top (e1 e2 rs tagged-stx) exni)
       (with-syntax ([(?top . ?var) tagged-stx])
         (learn-definites (list #'?var)))
       (append (if (eq? e1 tagged-stx)
                   null
                   (list (walk e1 tagged-stx 'tag-top)))
               (if exni
                   (list (stumble tagged-stx (car exni)))
                   null))]
      
      ;; The rest of the automatic primitives
      [(AnyQ p::STOP (e1 e2 rs) exni)
       (R e1
          [! exni])]

      [(AnyQ p:set!-macro (e1 e2 rs deriv) exni)
       (R e1
          [! exni]
          [#:frontier (list e1)]
          => (lambda (mid)
               (reductions* deriv)))]
      [(AnyQ p:set! (e1 e2 rs id-rs rhs) exni)
       (R e1
          [! exni]
          [#:pattern (SET! VAR RHS)]
          [#:frontier (list #'RHS)]
          [#:learn id-rs]
          [Expr RHS rhs])]

      ;; Synthetic primitives
      ;; These have their own subterm replacement mechanisms
      ;; FIXME: Frontier
      [(and d (AnyQ p:synth (e1 e2 rs subterms)))
       ;; First, compute the frontier based on the expanded subterms
       ;; Run through the renames in reverse order to get the pre-renamed terms
       (define synth-frontier
         (parameterize ((current-frontier null))
           (let floop ([subterms subterms])
             (cond [(null? subterms)
                    (void)]
                   [(s:subterm? (car subterms))
                    (floop (cdr subterms))
                    (add-frontier
                     (list (lift/deriv-e1 (s:subterm-deriv (car subterms)))))]
                   [(s:rename? (car subterms))
                    (floop (cdr subterms))
                    (rename-frontier (s:rename-after (car subterms))
                                     (s:rename-before (car subterms)))]))
           (current-frontier)))
       (add-frontier synth-frontier)
       ;; Then compute the reductions
       (let loop ([term e1] [subterms subterms])
         (cond [(null? subterms) 
                (let ([exn (and (error-wrap? d) (error-wrap-exn d))])
                  (if exn
                      (list (stumble term exn))
                      null))]
               [(s:subterm? (car subterms))
                (let* ([subterm0 (car subterms)]
                       [path0 (s:subterm-path subterm0)]
                       [deriv0 (s:subterm-deriv subterm0)])
                  (let ([ctx (lambda (x) (path-replace term path0 x))])
                    (append (with-context ctx
                              (reductions* deriv0))
                            (loop (and term
                                       (deriv? deriv0)
                                       (path-replace term path0 (deriv-e2 deriv0)))
                                  (cdr subterms)))))]
               [(s:rename? (car subterms))
                (let* ([subterm0 (car subterms)])
                  ;; FIXME: add renaming steps?
                  ;; FIXME: if so, coalesce?
                  (rename-frontier (s:rename-before subterm0)
                                   (s:rename-after subterm0))
                  (loop (and term
                             (path-replace term
                                           (s:rename-path subterm0)
                                           (s:rename-after subterm0)))
                        (cdr subterms)))]))]

      ;; FIXME
      [(IntQ p:rename (e1 e2 rs rename inner))
       (rename-frontier (car rename) (cdr rename))
       (reductions* inner)]

      ;; Error

      ;; Macros
      [(IntQ mrule (e1 e2 transformation next))
       (blaze-frontier e1)
       ;;(printf "frontier for mrule: ~s~n" (current-frontier))
       (append (reductions-transformation transformation)
               (begin (when next (add-frontier (list (lift/deriv-e1 next))))
                      (reductions* next)))]

      ;; Lifts

      [(IntQ lift-deriv (e1 e2 first lifted-stx second))
       (blaze-frontier e1)
       (let ([rs1 (reductions* first)])
         (add-frontier (list lifted-stx))
         (append rs1 
                 (list (walk (deriv-e2 first) lifted-stx 'capture-lifts))
                 (reductions* second)))]

      ;; Skipped
      
      [#f null]
      
      #;
      [else (error 'reductions "unmatched case: ~s" d)]))

  ;; reductions-transformation : Transformation -> ReductionSequence
  (define (reductions-transformation tx)
    (match tx
      [(struct transformation (e1 e2 rs me1 me2 locals seq))
       (learn-definites rs)
       (append (reductions-locals e1 locals)
               (list (walk e1 e2 'macro-step)))]
      [(IntW transformation (e1 e2 rs me1 me2 locals seq) 'locals)
       (learn-definites rs)
       (reductions-locals e1 locals)]
      [(ErrW transformation (e1 e2 rs me1 me2 locals seq) 'bad-transformer exn)
       (learn-definites rs)
       (list (stumble e1 exn))]
      [(ErrW transformation (e1 e2 rs me1 me2 locals seq) 'transform exn)
       (learn-definites rs)
       (append (reductions-locals e1 locals)
               (list (stumble e1 exn)))]))

  ;; reductions-locals : syntax (list-of LocalAction) -> ReductionSequence
  (define (reductions-locals stx locals)
    (with-new-local-context stx
      (apply append (map reductions-local locals))))

  ;; reductions-local : LocalAction -> ReductionSequence
  (define (reductions-local local)
    (match/with-derivation local
      [(struct local-expansion (e1 e2 me1 me2 for-stx? deriv))
       (reductions* deriv)]
      [(struct local-expansion/expr (e1 e2 me1 me2 for-stx? opaque deriv))
       (fprintf (current-error-port)
                "reductions: local-expand-expr not fully implemented")
       (reductions* deriv)]
      [(struct local-lift (expr id))
       (list (walk expr id 'local-lift))]
      [(struct local-lift-end (decl))
       (list (walk decl decl 'module-lift))]
      [(struct local-bind (deriv))
       (reductions* deriv)]))

  ;; list-reductions : ListDerivation -> ReductionSequence
  (define (list-reductions ld)
    (match/with-derivation ld
      [(IntQ lderiv (es1 es2 derivs))
       (let loop ([derivs derivs] [suffix es1])
         (cond [(pair? derivs)
                (append
                 (with-context (lambda (x) (cons x (stx-cdr suffix)))
                   (reductions* (car derivs)))
                 (with-context (lambda (x) (cons (deriv-e2 (car derivs)) x))
                   (loop (cdr derivs) (stx-cdr suffix))))]
               [(null? derivs)
                null]))]
      [(ErrW lderiv (es1 es2 derivs) _ exn)
       (list (stumble es1 exn))]

      [#f null]))

  ;; block-reductions : BlockDerivation -> ReductionSequence
  (define (block-reductions bd)
    (match/with-derivation bd
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
                       [(AnyQ lderiv (pass2-es1 _ _))
                        (list (walk stxs1 pass2-es1 'block->letrec))])
                     null)
                 (begin (add-frontier (stx->list* (lift/lderiv-es1 pass2)))
                        (list-reductions pass2))))]
      [#f null]))

  ;; brules-reductions : (list-of-BRule) syntax-list -> ReductionSequence syntax-list
  (define (brules-reductions brules all-stxs)
    (let loop ([brules brules] [suffix all-stxs] [prefix null] [rss null])
      (cond [(pair? brules)
             (let ([brule0 (car brules)]
                   [next (cdr brules)])
               (match/with-derivation brule0
                 [(struct b:expr (renames head))
                  (rename-frontier (car renames) (cdr renames))
                  (let ([estx (deriv-e2 head)])
                    (loop next (stx-cdr suffix) (cons estx prefix)
                          (cons (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                                  (reductions* head))
                                rss)))]
                 [(IntW b:expr (renames head) tag)
                  (rename-frontier (car renames) (cdr renames))
                  (loop next #f #f 
                        (cons (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                                (reductions* head))
                              rss))]
                 [(struct b:defvals (renames head))
                  (rename-frontier (car renames) (cdr renames))
                  (let ([head-rs 
                         (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                           (reductions* head))])
                    (loop next (stx-cdr suffix) (cons (deriv-e2 head) prefix)
                          (cons head-rs rss)))]
                 [(AnyQ b:defstx (renames head rhs))
                  (rename-frontier (car renames) (cdr renames))
                  (let* ([estx (deriv-e2 head)]
                         [estx2 (and (deriv? rhs)
                                     (with-syntax ([(?ds ?vars ?rhs) estx]
                                                   [?rhs* (deriv-e2 rhs)])
                                       (datum->syntax-object estx
                                                             `(,#'?ds ,#'?vars ,#'?rhs*)
                                                             estx estx)))])
                    (loop next (stx-cdr suffix) (cons estx2 prefix)
                          (with-context (lambda (x) (revappend prefix (cons x (stx-cdr suffix))))
                            (cons (with-context (CC ?rhs estx (?ds ?vars ?rhs))
                                    (reductions* rhs))
                                  (cons (reductions* head)
                                        rss)))))]
                 [(struct b:splice (renames head tail))
                  (rename-frontier (car renames) (cdr renames))
                  (loop next tail prefix
                        (cons (list (walk/foci (deriv-e2 head)
                                               (stx-take tail
                                                         (- (stx-improper-length tail)
                                                            (stx-improper-length (stx-cdr suffix))))
                                               (revappend prefix 
                                                          (cons (deriv-e2 head) (stx-cdr suffix)))
                                               (revappend prefix tail)
                                               'splice-block))
                              (cons (with-context (lambda (x) 
                                                    (revappend prefix (cons x (stx-cdr suffix))))
                                      (reductions* head))
                                    rss)))]
                 [(struct b:begin (renames head derivs))
                  ;; FIXME
                  (error 'unimplemented)]
                 [(struct error-wrap (exn tag _inner))
                  (values (list (stumble/E suffix (revappend prefix suffix) exn))
                          (revappend prefix suffix))]))]
            [(null? brules)
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
              (define (the-context x) (revappend prefix (cons x (stx-cdr suffix))))
              (cond [(pair? mbrules)
                     (let ([mbrule0 (car mbrules)]
                           [next (cdr mbrules)])
                       (match/with-derivation mbrule0
                         [(struct mod:skip ())
                          ;(blaze-frontier (stx-car suffix))
                          (loop next (stx-cdr suffix) (cons (stx-car suffix) prefix))]
                         [(struct mod:cons (head))
                          ;(blaze-frontier (stx-car suffix))
                          (rename-frontier (stx-car suffix) (lift/deriv-e1 head))
                          (add-frontier (list (lift/deriv-e1 head)))
                          (append (with-context the-context (append (reductions* head)))
                                  (let ([estx (and (deriv? head) (deriv-e2 head))])
                                    (loop next (stx-cdr suffix) (cons estx prefix))))]
                         [(AnyQ mod:prim (head prim))
                          ;(blaze-frontier (stx-car suffix))
                          (rename-frontier (stx-car suffix) (lift/deriv-e1 head))
                          (add-frontier (list (lift/deriv-e1 head)))
                          (append (with-context the-context
                                    (append (reductions* head)
                                            (begin
                                              (when prim
                                                (add-frontier (list (lift/deriv-e1 prim))))
                                              (reductions* prim))))
                                  (let ([estx 
                                         (if prim
                                             (lift/deriv-e2 prim)
                                             (and (deriv? head) (deriv-e2 head)))])
                                    (loop next (stx-cdr suffix) (cons estx prefix))))]
                         [(ErrW mod:splice (head stxs) exn)
                          ;(blaze-frontier (stx-car suffix))
                          (rename-frontier (stx-car suffix) (lift/deriv-e1 head))
                          (add-frontier (list (lift/deriv-e1 head)))
                          (append (with-context the-context (reductions* head))
                                  (list (stumble (deriv-e2 head) exn)))]
                         [(struct mod:splice (head stxs))
                          ;(blaze-frontier (stx-car suffix))
                          (rename-frontier (stx-car suffix) (lift/deriv-e1 head))
                          (add-frontier (list (lift/deriv-e1 head)))
                          (append
                           (with-context the-context (reductions* head))
                           (let ([suffix-tail (stx-cdr suffix)]
                                 [head-e2 (deriv-e2 head)])
                             (let ([new-stxs (stx-take stxs
                                                       (- (stx-improper-length stxs)
                                                          (stx-improper-length suffix-tail)))])
                               (cons (walk/foci head-e2
                                                new-stxs
                                                (revappend prefix (cons head-e2 suffix-tail))
                                                (revappend prefix stxs)
                                                'splice-module)
                                     (begin (add-frontier new-stxs)
                                            (loop next stxs prefix))))))]
                         [(struct mod:lift (head stxs))
                          ;; FIXME: frontier
                          (append
                           (with-context the-context (reductions* head))
                           (let ([suffix-tail (stx-cdr suffix)]
                                 [head-e2 (deriv-e2 head)])
                             (let ([new-suffix (append stxs (cons head-e2 suffix-tail))])
                               (cons (walk/foci null
                                                stxs
                                                (revappend prefix (cons head-e2 suffix-tail))
                                                (revappend prefix new-suffix)
                                                'splice-lifts)
                                     (loop next
                                           new-suffix
                                           prefix)))))]
                         [(struct mod:lift-end (tail))
                          ;; FIXME: frontier
                          (append
                           (if (pair? tail)
                               (list (walk/foci null
                                                tail
                                                (revappend prefix suffix)
                                                (revappend prefix tail)
                                                'splice-module-lifts))
                               null)
                           (loop next tail prefix))]))]
                    [(null? mbrules)
                     (set! final-stxs (reverse prefix))
                     null]))])
      (values reductions final-stxs)))

  (define (stx->list* stx)
    (cond [(pair? stx)
           (cons (car stx) (stx->list* (cdr stx)))]
          [(null? stx)
           null]
          [(syntax? stx)
           (let ([x (syntax-e stx)])
             (if (pair? x)
                 (cons (car x) (stx->list* (cdr x)))
                 (list stx)))]
          [else null]))
  )
