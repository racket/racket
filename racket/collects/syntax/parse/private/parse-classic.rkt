#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     syntax/private/id-table
                     racket/syntax
                     "minimatch.rkt"
                     "rep-attrs.rkt"
                     "rep-data.rkt"
                     "rep-patterns.rkt"
                     "rep.rkt"
                     "kws.rkt"
                     "const-expr.rkt"
                     "opt.rkt")
         racket/syntax
         racket/stxparam
         syntax/stx
         "residual.rkt"
         "runtime.rkt"
         "runtime-reflect.rkt")

(provide (for-syntax codegen-rhs
                     codegen-clauses))

;; ============================================================

(begin-for-syntax
 (define (codegen-rhs name formals relsattrs the-rhs splicing? [ctx #f])
   (define-values (transparent? description variants defs commit? delimit-cut?)
     (match the-rhs
       [(rhs _ transparent? description variants defs commit? delimit-cut?)
        (values transparent? description variants defs commit? delimit-cut?)]))
   (define vdefss (map variant-definitions variants))
   (define formals* (rewrite-formals formals #'x #'rl))
   (define patterns (map variant-pattern variants))
   (define no-fail?
     (and (not splicing?) ;; FIXME: commit? needed?
          (patterns-cannot-fail? patterns)))
   (when (and no-fail? ctx) (log-syntax-parse-debug "(stxclass) cannot fail: ~e" ctx))
   (define body
     (cond [(null? patterns)
            #'(fail (failure* pr es))]
           [splicing?
            (with-syntax ([(alternative ...)
                           (for/list ([pattern (in-list patterns)])
                             (with-syntax ([pattern pattern]
                                           [relsattrs relsattrs]
                                           [iattrs (pattern-attrs pattern)]
                                           [commit? commit?]
                                           [result-pr
                                            (if transparent?
                                                #'rest-pr
                                                #'(ps-pop-opaque rest-pr))])
                               #'(parse:H x cx rest-x rest-cx rest-pr pattern pr es
                                          (variant-success relsattrs iattrs (rest-x rest-cx result-pr)
                                                           success fh0 commit?))))])
              #'(try alternative ...))]
           [else
            (with-syntax ([matrix
                           (optimize-matrix
                            (for/list ([pattern (in-list patterns)])
                              (with-syntax ([iattrs (pattern-attrs pattern)]
                                            [relsattrs relsattrs]
                                            [commit? commit?])
                                (pk1 (list pattern)
                                     #'(variant-success relsattrs iattrs ()
                                                        success fh0 commit?)))))])
              #'(parse:matrix ((x cx pr es)) matrix))]))
   (with-syntax ([formals* formals*]
                 [(def ...) defs]
                 [((vdef ...) ...) vdefss]
                 [description description]
                 [transparent? transparent?]
                 [delimit-cut? delimit-cut?]
                 [body body])
     #`(lambda (x cx pr es undos fh0 cp0 rl success . formals*)
         (with ([this-syntax x]
                [this-role rl])
               def ...
               vdef ... ...
               (#%expression
                (syntax-parameterize ((this-context-syntax
                                       (make-this-context-syntax-transformer #'pr)))
                  (let ([es (es-add-thing pr description 'transparent? rl
                                          #,(if no-fail? #'#f #'es))]
                        [pr (if 'transparent? pr (ps-add-opaque pr))])
                    (with ([fail-handler fh0]
                           [cut-prompt cp0]
                           [undo-stack undos])
                      ;; Update the prompt, if required
                      ;; FIXME: can be optimized away if no cut exposed within variants
                      (with-maybe-delimit-cut delimit-cut?
                        body)))))))))

 ;; constant-desc : Expr -> String/#f
 (define (constant-desc e)
   (string-expr-value e)))

#|
Parsing protocols:

(parse:<X> <X-args> pr es success-expr) : Ans

  <S-args> : x cx
  <H-args> : x cx rest-x rest-cx rest-pr
  <EH-args> : x cx ???
  <A-args> : x cx

  x is term to parse, usually syntax but can be pair/null (stx-list?) in cdr patterns
  cx is most recent syntax object: if x must be coerced to syntax, use cx as lexctx and src
  pr, es are progress and expectstack, respectively
  rest-x, rest-cx, rest-pr are variable names to bind in context of success-expr

(stxclass-parser x cx pr es undos fail-handler cut-prompt role success-proc arg ...) : Ans

  success-proc:
    for stxclass, is (fail-handler undos attr-value ... -> Ans)
    for splicing-stxclass, is (undos fail-handler rest-x rest-cx rest-pr attr-value -> Ans)
  fail-handler, cut-prompt : undos failure -> Ans

Fail-handler is normally represented with stxparam 'fail-handler', but must be
threaded through stxclass calls (in through stxclass-parser, out through
success-proc) to support backtracking. Cut-prompt is never changed within
stxclass or within alternative, so no threading needed.

The undo stack is normally represented with stxparam 'undo-stack', but must be
threaded through stxclass calls (like fail-handler). A failure handler closes
over a base undo stack and receives an extended current undo stack; the failure
handler unwinds effects by performing every action in the difference between
them and then restores the saved undo stack.

Usually sub-patterns processed in tail position, but *can* do non-tail calls for:
  - ~commit
  - var of stxclass with ~commit
It is also safe to keep normal tail-call protocol and just adjust fail-handler.
There is no real benefit to specializing ~commit, since it does not involve
creating a success closure.

Some optimizations:
  - commit protocol for stxclasses (but not ~commit, no point)
  - avoid continue-vs-end choice point in (EH ... . ()) by eager pair check
  - integrable stxclasses, specialize ellipses of integrable stxclasses
  - pattern lists that cannot fail set es=#f to disable ExpectStack allocation
|#

;; ----

(begin-for-syntax
 (define (wash stx)
   (syntax-e stx))
 (define (wash-list washer stx)
   (let ([l (stx->list stx)])
     (unless l (raise-type-error 'wash-list "stx-list" stx))
     (map washer l)))
 (define (wash-iattr stx)
   (with-syntax ([#s(attr name depth syntax?) stx])
     (attr #'name (wash #'depth) (wash #'syntax?))))
 (define (wash-sattr stx)
   (with-syntax ([#s(attr name depth syntax?) stx])
     (attr (wash #'name) (wash #'depth) (wash #'syntax?))))
 (define (wash-iattrs stx)
   (wash-list wash-iattr stx))
 (define (wash-sattrs stx)
   (wash-list wash-sattr stx))
 (define (generate-n-temporaries n)
   (generate-temporaries
    (for/list ([i (in-range n)])
      (string->symbol (format "g~sx" i))))))

;; ----

#|
Conventions:
  - rhs : RHS
  - iattr : IAttr
  - relsattr : SAttr
  - splicing? : bool
  - x : id (var)
  - cx : id (var, may be shadowed)
  - pr : id (var, may be shadowed)
  - es : id (var, may be shadowed)
  - success : var (bound to success procedure)
  - k : expr
  - rest-x, rest-cx, rest-pr : id (to be bound)
  - fh, cp, rl : id (var)
|#

;; (with-maybe-delimit-cut bool expr)
(define-syntax with-maybe-delimit-cut
  (syntax-rules ()
    [(wmdc #t k)
     (with ([cut-prompt fail-handler]) k)]
    [(wmdc #f k)
     k]))

;; (variant-success relsattrs variant (also:id ...) success bool) : expr[Ans]
(define-syntax (variant-success stx)
  (syntax-case stx ()
    [(variant-success relsattrs iattrs (also ...) success fh0 commit?)
     #`(with-maybe-reset-fail commit? fh0
         (base-success-expr iattrs relsattrs (also ...) success))]))

;; (with-maybe-reset-fail bool id expr)
(define-syntax with-maybe-reset-fail
  (syntax-rules ()
    [(wmrs #t fh0 k)
     (with ([fail-handler fh0]) k)]
    [(wmrs #f fh0 k)
     k]))

;; (base-success-expr iattrs relsattrs (also:id ...) success) : expr[Ans]
(define-syntax (base-success-expr stx)
  (syntax-case stx ()
    [(base-success-expr iattrs relsattrs (also ...) success)
     (let ([reliattrs
            (reorder-iattrs (wash-sattrs #'relsattrs)
                            (wash-iattrs #'iattrs))])
       (with-syntax ([(#s(attr name _ _) ...) reliattrs])
         #'(success fail-handler undo-stack also ... (attribute name) ...)))]))

;; ----

(begin-for-syntax
  ;; codegen-clauses : Symbol Expr[Syntax] Id (Listof Defn) (Listof SinglePattern) (Listof Expr)
  ;;                   Syntax Boolean
  ;;                -> Expr
  (define (codegen-clauses who context x all-defs patterns body-exprs ctx track-literals?)
    (define (wrap-track-literals stx)
      (if track-literals? (quasisyntax/loc stx (track-literals '#,who #,stx)) stx))
    (define no-fail? (patterns-cannot-fail? patterns))
    (when (and no-fail? ctx) (log-syntax-parse-debug "cannot fail: ~e" ctx))
    (with-syntax ([(who context x) (list who context x)]
                  [(def ...) all-defs]
                  [init-es (if no-fail? #'#f #'#t)])
      #`(let* ([ctx0 (normalize-context 'who context x)]
               [pr (ps-empty x (cadr ctx0))]
               [es init-es]
               [cx x]
               [fh0 (syntax-patterns-fail ctx0)])
          def ...
          (parameterize ((current-syntax-context (cadr ctx0))
                         (current-state '#hasheq())
                         (current-state-writable? #f))
            (with ([fail-handler fh0]
                   [cut-prompt fh0]
                   [undo-stack null])
              #,(wrap-track-literals
                 (cond [(pair? patterns)
                        (with-syntax ([matrix
                                       (optimize-matrix
                                        (for/list ([pattern (in-list patterns)]
                                                   [body-expr (in-list body-exprs)])
                                          (pk1 (list pattern) body-expr)))])
                          #'(parse:matrix ((x cx pr es)) matrix))
                        #;
                        (with-syntax ([(alternative ...)
                                       (for/list ([pattern (in-list patterns)]
                                                  [body-expr (in-list body-exprs)])
                                         #`(parse:S x cx #,pattern pr es #,body-expr))])
                          #`(try alternative ...))]
                       [else
                        #`(fail (failure* pr es))]))))))))

;; ----

;; (parse:matrix ((x cx pr es) ...) (PK ...)) : expr[Ans]
;; (parse:matrix (in1 ... inN) (#s(pk1 (P11 ... P1N) e1) ... #s(pk1 (PM1 ... PMN) eM)))
;; represents the matching matrix
;;   [_in1_..._inN_|____]
;;   [ P11 ... P1N | e1 ]
;;   [  :       :  |  : ]
;;   [ PM1 ... PMN | eM ]

(define-syntax (parse:matrix stx)
  (syntax-case stx ()
    [(parse:matrix ins (pk ...))
     #'(try (parse:pk ins pk) ...)]))

(define-syntax (parse:pk stx)
  (syntax-case stx ()
    [(parse:pk () #s(pk1 () k))
     #'k]
    [(parse:pk ((x cx pr es) . ins) #s(pk1 (pat1 . pats) k))
     #'(parse:S x cx pat1 pr es (parse:pk ins #s(pk1 pats k)))]
    [(parse:pk ((x cx pr es) . ins) #s(pk/same pat1 inner))
     #'(parse:S x cx pat1 pr es (parse:matrix ins inner))]
    [(parse:pk ((x cx pr es) . ins) #s(pk/pair inner))
     #'(let-values ([(datum tcx)
                     (if (syntax? x)
                         (values (syntax-e x) x)
                         (values x cx))])
         (if (pair? datum)
             (let ([hx (car datum)]
                   [hcx (car datum)]
                   [hpr (ps-add-car pr)]
                   [tx (cdr datum)]
                   [tpr (ps-add-cdr pr)])
               (parse:matrix ((hx hcx hpr es) (tx tcx tpr es) . ins) inner))
             (let ([es* (if (null? datum) (es-add-proper-pair (first-desc:matrix inner) es) es)])
               (fail (failure* pr es*)))))]
    [(parse:pk (in1 . ins) #s(pk/and inner))
     #'(parse:matrix (in1 in1 . ins) inner)]))

(define-syntax (first-desc:matrix stx)
  (syntax-case stx ()
    [(fdm (#s(pk1 (pat1 . pats) k)))
     #'(first-desc:S pat1)]
    [(fdm (#s(pk/same pat1 pks)))
     #'(first-desc:S pat1)]
    [(fdm (pk ...)) ;; FIXME
     #'#f]))

;; ----

;; (parse:S x cx S-pattern pr es k) : expr[Ans]
;; In k: attrs(S-pattern) are bound.
(define-syntax (parse:S stx)
  (syntax-case stx ()
    [(parse:S x cx pattern0 pr es k)
     (syntax-case #'pattern0 ()
       [#s(pat:seq-end)
        #`(k x cx pr)]
       [#s(pat:any)
        #'k]
       [#s(pat:svar name)
        #'(let-attributes ([#s(attr name 0 #t) (datum->syntax cx x cx)])
            k)]
       [#s(pat:var/p name parser argu (nested-a ...) role
                     #s(scopts attr-count commit? _delimit? _desc))
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) (datum->syntax cx x cx)])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #'(app-argu parser x cx pr es undo-stack fail-handler cut-prompt role
                          (lambda (fh undos av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh] [undo-stack undos])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs undos av ...)
                              (with ([fail-handler
                                      (lambda (undos fs)
                                        (unwind-to undos undo-stack)
                                        (values fs undo-stack (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es undo-stack
                                            fail-handler cut-prompt role
                                            (lambda (fh undos av ...) (values #f undos av ...))
                                            argu)))])
                  (if fs
                      (fail fs)
                      (let-attributes (name-attr ...)
                        (let-attributes* ((nested-a ...) (av ...))
                          (with ([undo-stack undos])
                            k)))))))]
       [#s(pat:reflect obj argu attr-decls name (nested-a ...))
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) (datum->syntax cx x cx)])
                           #'())])
          (with-syntax ([arity (arguments->arity (syntax->datum #'argu))])
            #'(let ([parser (reflect-parser obj 'arity 'attr-decls #f)])
                (app-argu parser x cx pr es undo-stack fail-handler cut-prompt #f
                          (lambda (fh undos . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                (with ([fail-handler fh] [undo-stack undos])
                                  k))))
                          argu))))]
       [#s(pat:datum datum)
        (with-syntax ([unwrap-x
                       (if (atomic-datum-stx? #'datum)
                           #'(if (syntax? x) (syntax-e x) x)
                           #'(syntax->datum (datum->syntax #f x)))])
          #`(let ([d unwrap-x])
              (if (equal? d (quote datum))
                  k
                  (fail (failure* pr (es-add-atom 'datum es))))))]
       [#s(pat:literal literal input-phase lit-phase)
        #`(if (and (identifier? x)
                   (free-identifier=? x (quote-syntax literal) input-phase lit-phase))
              (with ([undo-stack (cons (current-state) undo-stack)])
                (state-cons! 'literals x)
                k)
              (fail (failure* pr (es-add-literal (quote-syntax literal) es))))]
       [#s(pat:action action subpattern)
        #'(parse:A x cx action pr es (parse:S x cx subpattern pr es k))]
       [#s(pat:head head tail)
        #`(parse:H x cx rest-x rest-cx rest-pr head pr es
                   (parse:S rest-x rest-cx tail rest-pr es k))]
       [#s(pat:dots head tail)
        #`(parse:dots x cx head tail pr es k)]
       [#s(pat:and subpatterns)
        (for/fold ([k #'k]) ([subpattern (in-list (reverse (syntax->list #'subpatterns)))])
          #`(parse:S x cx #,subpattern pr es #,k))]
       [#s(pat:or (a ...) (subpattern ...) (subattrs ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh undos id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh] [undo-stack undos])
                         k)))])
              (try (parse:S x cx subpattern pr es
                            (disjunct subattrs success () (id ...)))
                   ...)))]
       [#s(pat:not subpattern)
        #`(let* ([fh0 fail-handler]
                 [pr0 pr]
                 [es0 es]
                 [fail-to-succeed
                  (lambda (undos fs) (unwind-to undos undo-stack) k)])
            ;; ~not implicitly prompts to be safe,
            ;; but ~! not allowed within ~not (unless within ~delimit-cut, etc)
            ;; (statically checked!)
            (with ([fail-handler fail-to-succeed]
                   [cut-prompt fail-to-succeed]) ;; to be safe
              (parse:S x cx subpattern pr es
                       (fh0 undo-stack (failure* pr0 es0)))))]
       [#s(pat:pair head tail)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)]
                [cx (if (syntax? x) x cx)])  ;; FIXME: shadowing cx?!
            (if (pair? datum)
                (let ([hx (car datum)]
                      [hcx (car datum)]
                      [hpr (ps-add-car pr)]
                      [tx (cdr datum)]
                      [tpr (ps-add-cdr pr)])
                  (parse:S hx hcx head hpr es
                           (parse:S tx cx tail tpr es k)))
                (let ([es* (if (null? datum) (es-add-proper-pair (first-desc:S head) es) es)])
                  (fail (failure* pr es*)))))]
       [#s(pat:vector subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (vector? datum)
                (let ([datum (vector->list datum)]
                      [vcx (if (syntax? x) x cx)] ;; FIXME: (vector? datum) => (syntax? x) ???
                      [pr* (ps-add-unvector pr)])
                  (parse:S datum vcx subpattern pr* es k))
                (fail (failure* pr es))))]
       [#s(pat:box subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (box? datum)
                (let ([datum (unbox datum)]
                      [bcx (if (syntax? x) x cx)] ;; FIXME: (box? datum) => (syntax? x) ???
                      [pr* (ps-add-unbox pr)])
                  (parse:S datum bcx subpattern pr* es k))
                (fail (failure* pr es))))]
       [#s(pat:pstruct key subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (let ([xkey (prefab-struct-key datum)])
                  (and xkey (equal? xkey 'key)))
                (let ([datum (cdr (vector->list (struct->vector datum)))]
                      [scx (if (syntax? x) x cx)] ;; FIXME: (struct? datum) => (syntax? x) ???
                      [pr* (ps-add-unpstruct pr)])
                  (parse:S datum scx subpattern pr* es k))
                (fail (failure* pr es))))]
       [#s(pat:describe pattern description transparent? role)
        #`(let ([es* (es-add-thing pr description transparent? role es)]
                [pr* (if 'transparent? pr (ps-add-opaque pr))])
            (parse:S x cx pattern pr* es* k))]
       [#s(pat:delimit pattern)
        #`(let ([cp0 cut-prompt])
            (with ([cut-prompt fail-handler])
              (parse:S x cx pattern pr es (with ([cut-prompt cp0]) k))))]
       [#s(pat:commit pattern)
        #`(let ([fh0 fail-handler]
                [cp0 cut-prompt])
            (with ([cut-prompt fh0])
              (parse:S x cx pattern pr es
                       (with ([cut-prompt cp0]
                              [fail-handler fh0])
                             k))))]
       [#s(pat:ord pattern group index)
        #`(let ([pr* (ps-add pr '#s(ord group index))])
            (parse:S x cx pattern pr* es k))]
       [#s(pat:post pattern)
        #`(let ([pr* (ps-add-post pr)])
            (parse:S x cx pattern pr* es k))]
       [#s(pat:integrated name predicate description role)
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) x*])
                           #'())])
          #'(let ([x* (datum->syntax cx x cx)])
              (if (predicate x*)
                  (let-attributes (name-attr ...) k)
                  (let ([es* (es-add-thing pr 'description #t role es)])
                    (fail (failure* pr es*))))))]
       [_ (wrong-syntax stx "internal error: bad S pattern: ~e" #'pattern0)])]))

;; (first-desc:S S-pattern) : expr[FirstDesc]
(define-syntax (first-desc:S stx)
  (syntax-case stx ()
    [(fds p)
     (syntax-case #'p ()
       [#s(pat:any)
        #''(any)]
       [#s(pat:svar name)
        #''(any)]
       [#s(pat:var/p _ _ _ _ _ #s(scopts _ _ _ desc))
        #'(quote desc)]
       [#s(pat:datum d)
        #''(datum d)]
       [#s(pat:literal id _ip _lp)
        #''(literal id)]
       [#s(pat:describe _p desc _t? _role)
        #`(quote #,(or (constant-desc #'desc) #'#f))]
       [#s(pat:delimit pattern)
        #'(first-desc:S pattern)]
       [#s(pat:commit pattern)
        #'(first-desc:S pattern)]
       [#s(pat:ord pattern _ _)
        #'(first-desc:S pattern)]
       [#s(pat:post pattern)
        #'(first-desc:S pattern)]
       [#s(pat:integrated _name _pred description _role)
        #''description]
       [_ #'#f])]))

;; (first-desc:H HeadPattern) : Expr
(define-syntax (first-desc:H stx)
  (syntax-case stx ()
    [(fdh hpat)
     (syntax-case #'hpat ()
       [#s(hpat:single sp) #'(first-desc:S sp)]
       [#s(hpat:var/p _ _ _ _ _ #s(scopts _ _ _ desc)) #'(quote desc)]
       [#s(hpat:seq lp) #'(first-desc:L lp)]
       [#s(hpat:describe _hp desc _t? _r)
        #`(quote #,(or (constant-desc #'desc) #'#f))]
       [#s(hpat:delimit hp) #'(first-desc:H hp)]
       [#s(hpat:commit hp) #'(first-desc:H hp)]
       [#s(hpat:ord hp _ _) #'(first-desc:H hp)]
       [#s(hpat:post hp) #'(first-desc:H hp)]
       [_ #'#f])]))

(define-syntax (first-desc:L stx)
  (syntax-case stx ()
    [(fdl lpat)
     (syntax-case #'lpat ()
       [#s(pat:pair sp lp) #'(first-desc:S sp)]
       [_ #'#f])]))

;; (disjunct (iattr ...) success (pre:expr ...) (id:id ...)) : expr[Ans]
(define-syntax (disjunct stx)
  (syntax-case stx ()
    [(disjunct (#s(attr sub-id _ _) ...) success (pre ...) (id ...))
     (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
       #`(let ([alt-sub-id (attribute sub-id)] ...)
           (let ([id #f] ...)
             (let ([sub-id alt-sub-id] ...)
               (success fail-handler undo-stack pre ... id ...)))))]))

;; (parse:A x cx A-pattern pr es k) : expr[Ans]
;; In k: attrs(A-pattern) are bound.
(define-syntax (parse:A stx)
  (syntax-case stx ()
    [(parse:A x cx pattern0 pr es k)
     (syntax-case #'pattern0 ()
       [#s(action:and (action ...))
        (for/fold ([k #'k]) ([action (in-list (reverse (syntax->list #'(action ...))))])
          #`(parse:A x cx #,action pr es #,k))]
       [#s(action:cut)
        #'(with ([fail-handler cut-prompt]) k)]
       [#s(action:bind a expr)
        #'(let-attributes ([a (wrap-user-code expr)]) k)]
       [#s(action:fail condition message)
        #`(let ([c (wrap-user-code condition)])
            (if c
                (let ([pr* (if (syntax? c) (ps-add-stx pr c) pr)]
                      [es* (es-add-message message es)])
                  (fail (failure* pr* es*)))
                k))]
       [#s(action:parse pattern expr)
        #`(let* ([y (datum->syntax/with-clause (wrap-user-code expr))]
                 [cy y]
                 [pr* (ps-add-stx pr y)])
            (parse:S y cy pattern pr* es k))]
       [#s(action:do (stmt ...))
        #'(parameterize ((current-state-writable? #t))
            (let ([init-state (current-state)])
              (no-shadow stmt) ...
              (parameterize ((current-state-writable? #f))
                (with ([undo-stack (maybe-add-state-undo init-state (current-state) undo-stack)])
                  (#%expression k)))))]
       [#s(action:undo (stmt ...))
        #'(with ([undo-stack (cons (lambda () stmt ... (void)) undo-stack)]
                 [cut-prompt illegal-cut-error])
            k)]
       [#s(action:ord pattern group index)
        #'(let ([pr* (ps-add pr '#s(ord group index))])
            (parse:A x cx pattern pr* es k))]
       [#s(action:post pattern)
        #'(let ([pr* (ps-add-post pr)])
            (parse:A x cx pattern pr* es k))]
       [_ (wrong-syntax stx "internal error: bad A pattern: ~e" #'pattern0)])]))

;; (parse:H x cx rest-x rest-cx rest-pr H-pattern pr es k)
;; In k: rest, rest-pr, attrs(H-pattern) are bound.
(define-syntax (parse:H stx)
  (syntax-case stx ()
    [(parse:H x cx rest-x rest-cx rest-pr head pr es k)
     (syntax-case #'head ()
       [#s(hpat:single pattern)
        #'(parse:S x cx
                   ;; FIXME: consider proper-list-pattern? (yes is consistent with ~seq)
                   #s(pat:pair pattern #s(pat:seq-end))
                   pr es (lambda (rest-x rest-cx rest-pr) k))]
       [#s(hpat:describe pattern description transparent? role)
        #`(let ([es* (es-add-thing pr description transparent? role es)]
                [pr* (if 'transparent? pr (ps-add-opaque pr))])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr* es*
                     (let ([rest-pr (if 'transparent? rest-pr (ps-pop-opaque rest-pr))])
                       k)))]
       [#s(hpat:var/p name parser argu (nested-a ...) role
                      #s(scopts attr-count commit? _delimit? _desc))
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t)
                               (stx-list-take x (ps-difference pr rest-pr))])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #`(app-argu parser x cx pr es undo-stack fail-handler cut-prompt role
                          (lambda (fh undos rest-x rest-cx rest-pr av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh] [undo-stack undos])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs undos rest-x rest-cx rest-pr av ...)
                              (with ([fail-handler
                                      (lambda (undos fs)
                                        (unwind-to undos undo-stack)
                                        (values fs undo-stack #f #f #f (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es undo-stack
                                            fail-handler cut-prompt role
                                            (lambda (fh undos rest-x rest-cx rest-pr av ...)
                                              (values #f undos rest-x rest-cx rest-pr av ...))
                                            argu)))])
                  (if fs
                      (fail fs)
                      (let-attributes (name-attr ...)
                        (let-attributes* ((nested-a ...) (av ...))
                          (with ([undo-stack undos])
                            k)))))))]
       [#s(hpat:reflect obj argu attr-decls name (nested-a ...))
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t)
                               (stx-list-take x (ps-difference pr rest-pr))])
                           #'())])
          (with-syntax ([arity (arguments->arity (syntax->datum #'argu))])
            #'(let ([parser (reflect-parser obj 'arity 'attr-decls #t)])
                (app-argu parser x cx pr es undo-stack fail-handler cut-prompt #f
                          (lambda (fh undos rest-x rest-cx rest-pr . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                 (with ([fail-handler fh] [undo-stack undos])
                                   k))))
                          argu))))]
       [#s(hpat:and head single)
        #`(let ([cx0 cx])
            (parse:H x cx rest-x rest-cx rest-pr head pr es
                     (let ([lst (stx-list-take x (ps-difference pr rest-pr))])
                       (parse:S lst cx0 single pr es k))))]
       [#s(hpat:or (a ...) (subpattern ...) (subattrs ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh undos rest-x rest-cx rest-pr id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh] [undo-stack undos])
                         k)))])
              (try (parse:H x cx rest-x rest-cx rest-pr subpattern pr es
                            (disjunct subattrs success (rest-x rest-cx rest-pr) (id ...)))
                   ...)))]
       [#s(hpat:seq pattern)
        #'(parse:S x cx pattern pr es (lambda (rest-x rest-cx rest-pr) k))]
       [#s(hpat:action action subpattern)
        #'(parse:A x cx action pr es (parse:H x cx rest-x rest-cx rest-pr subpattern pr es k))]
       [#s(hpat:delimit pattern)
        #'(let ([cp0 cut-prompt])
            (with ([cut-prompt fail-handler])
              (parse:H x cx rest-x rest-cx rest-pr pattern pr es
                       (with ([cut-prompt cp0]) k))))]
       [#s(hpat:commit pattern)
        #`(let ([fh0 fail-handler]
                [cp0 cut-prompt])
            (with ([cut-prompt fh0])
              (parse:H x cx rest-x rest-cx rest-pr pattern pr es
                       (with ([cut-prompt cp0]
                              [fail-handler fh0])
                             k))))]
       [#s(hpat:ord pattern group index)
        #`(let ([pr* (ps-add pr '#s(ord group index))])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr* es
                     (let ([rest-pr (ps-pop-ord rest-pr)]) k)))]
       [#s(hpat:post pattern)
        #'(let ([pr* (ps-add-post pr)])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr* es
                     (let ([rest-pr (ps-pop-post rest-pr)]) k)))]
       [#s(hpat:peek pattern)
        #`(let ([saved-x x] [saved-cx cx] [saved-pr pr])
            (parse:H x cx dummy-x dummy-cx dummy-pr pattern pr es
                     (let ([rest-x saved-x] [rest-cx saved-cx] [rest-pr saved-pr])
                       k)))]
       [#s(hpat:peek-not subpattern)
        #`(let* ([fh0 fail-handler]
                 [pr0 pr]
                 [es0 es]
                 [fail-to-succeed
                  (lambda (undos fs)
                    (unwind-to undos undo-stack)
                    (let ([rest-x x]
                          [rest-cx cx]
                          [rest-pr pr])
                      k))])
            ;; ~not implicitly prompts to be safe,
            ;; but ~! not allowed within ~not (unless within ~delimit-cut, etc)
            ;; (statically checked!)
            (with ([fail-handler fail-to-succeed]
                   [cut-prompt fail-to-succeed]) ;; to be safe
              (parse:H x cx rest-x rest-cx rest-pr subpattern pr es
                       (fh0 undo-stack (failure* pr0 es0)))))]
       [_ (wrong-syntax stx "internal error: bad H pattern: ~e" #'head)])]))

;; (parse:dots x cx EH-pattern S-pattern pr es k) : expr[Ans]
;; In k: attrs(EH-pattern, S-pattern) are bound.
(define-syntax (parse:dots stx)
  (syntax-case stx ()
    ;; == Specialized cases
    ;; -- (x ... . ())
    [(parse:dots x cx (#s(ehpat (attr0) #s(hpat:single #s(pat:svar name)) #f #f))
                 #s(pat:datum ()) pr es k)
     #'(let-values ([(status result) (predicate-ellipsis-parser x cx pr es void #f #f)])
         (case status
           ((ok) (let-attributes ([attr0 result]) k))
           (else (fail result))))]
    ;; -- (x:sc ... . ()) where sc is an integrable stxclass like id or expr
    [(parse:dots x cx (#s(ehpat (attr0) #s(hpat:single #s(pat:integrated _name pred? desc role)) #f #f))
                 #s(pat:datum ()) pr es k)
     #'(let-values ([(status result) (predicate-ellipsis-parser x cx pr es pred? desc role)])
         (case status
           ((ok) (let-attributes ([attr0 result]) k))
           (else (fail result))))]
    ;; -- (x:sc ... . ()) where sc is a stxclass with commit
    ;; Since head pattern does commit, no need to thread fail-handler, cut-prompt through.
    ;; Microbenchmark suggests this isn't a useful specialization
    ;; (probably try-or-pair/null-check already does the useful part)
    ;; == General case
    [(parse:dots x cx (#s(ehpat head-attrs head head-repc check-null?) ...) tail pr es k)
     (let ()
       (define repcs (wash-list wash #'(head-repc ...)))
       (define rep-ids (for/list ([repc (in-list repcs)])
                         (and repc (generate-temporary 'rep))))
       (define rel-repcs (filter values repcs))
       (define rel-rep-ids (filter values rep-ids))
       (define rel-heads (for/list ([head (in-list (syntax->list #'(head ...)))]
                                    [repc (in-list repcs)]
                                    #:when repc)
                           head))
       (define aattrs
         (for/list ([head-attrs (in-list (syntax->list #'(head-attrs ...)))]
                    [repc (in-list repcs)]
                    #:when #t
                    [a (in-list (wash-iattrs head-attrs))])
           (cons a repc)))
       (define attrs (map car aattrs))
       (define attr-repcs (map cdr aattrs))
       (define ids (map attr-name attrs))
       (define tail-pattern-is-null? (equal? (syntax->datum #'tail) '#s(pat:datum ())))
       (with-syntax ([(id ...) ids]
                     [(alt-id ...) (generate-temporaries ids)]
                     [reps rel-rep-ids]
                     [(head-rep ...) rep-ids]
                     [(rel-rep ...) rel-rep-ids]
                     [(rel-repc ...) rel-repcs]
                     [(rel-head ...) rel-heads]
                     [(a ...) attrs]
                     [(attr-repc ...) attr-repcs]
                     [do-pair/null?
                      ;; FIXME: do pair/null check only if no nullable head patterns
                      ;; (and tail-pattern-is-null? (andmap not (syntax->datum #'(nullable? ...))))
                      tail-pattern-is-null?])
         (define/with-syntax alt-map #'((id . alt-id) ...))
         (define/with-syntax loop-k
           #'(dots-loop dx* dcx* loop-pr* undo-stack fail-handler rel-rep ... alt-id ...))
         #`(let ()
             ;; dots-loop : stx progress rel-rep ... alt-id ... -> Ans
             (define (dots-loop dx dcx loop-pr undos fh rel-rep ... alt-id ...)
               (with ([fail-handler fh] [undo-stack undos])
                 (try-or-pair/null-check do-pair/null? dx dcx loop-pr es
                   (try (parse:EH dx dcx loop-pr head-attrs check-null? head-repc dx* dcx* loop-pr* 
                                  alt-map head-rep head es loop-k)
                        ...)
                   (cond [(< rel-rep (rep:min-number rel-repc))
                          (let ([es (expectation-of-reps/too-few es rel-rep rel-repc rel-head)])
                            (fail (failure* loop-pr es)))]
                         ...
                         [else
                          (let-attributes ([a (rep:finalize a attr-repc alt-id)] ...)
                            (parse:S dx dcx tail loop-pr es k))]))))
             (let ([rel-rep 0] ...
                   [alt-id (rep:initial-value attr-repc)] ...)
               (dots-loop x cx pr undo-stack fail-handler rel-rep ... alt-id ...)))))]))

;; (try-or-pair/null-check bool x cx es pr pair-alt maybe-null-alt)
(define-syntax try-or-pair/null-check
  (syntax-rules ()
    [(topc #t x cx pr es pair-alt null-alt)
     (cond [(stx-pair? x) pair-alt]
           [(stx-null? x) null-alt]
           [else (fail (failure* pr es))])]
    [(topc _ x cx pr es alt1 alt2)
     (try alt1 alt2)]))

;; (parse:EH x cx pr repc x* cx* pr* alts rep H-pattern es k) : expr[Ans]
;; In k: x*, cx*, pr*, alts`attrs(H-pattern) are bound and rep is shadowed.
(define-syntax (parse:EH stx)
  (syntax-case stx ()
    [(parse:EH x cx pr attrs check-null? repc x* cx* pr* alts rep head es k)
     (let ()
       (define/with-syntax k*
         (let* ([main-attrs (wash-iattrs #'attrs)]
                [ids (map attr-name main-attrs)]
                [alt-ids
                 (let ([table (make-bound-id-table)])
                   (for ([entry (in-list (syntax->list #'alts))])
                     (let ([entry (syntax-e entry)])
                       (bound-id-table-set! table (car entry) (cdr entry))))
                   (for/list ([id (in-list ids)]) (bound-id-table-ref table id)))])
           (with-syntax ([(id ...) ids]
                         [(alt-id ...) alt-ids])
             #`(let ([alt-id (rep:combine repc (attribute id) alt-id)] ...)
                 #,(if (syntax->datum #'check-null?)
                       #'(if (zero? (ps-difference pr pr*)) (error/null-eh-match) k)
                       #'k)))))
       (syntax-case #'repc ()
         [#f #`(parse:H x cx x* cx* pr* head pr es k*)]
         [_  #`(parse:H x cx x* cx* pr* head pr es
                        (if (< rep (rep:max-number repc))
                            (let ([rep (add1 rep)]) k*)
                            (let ([es* (expectation-of-reps/too-many es rep repc)])
                              (fail (failure* pr* es*)))))]))]))

;; (rep:initial-value RepConstraint) : expr
(define-syntax (rep:initial-value stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'#f]
    [(_ #s(rep:optional _ _ _)) #'#f]
    [(_ _) #'null]))

;; (rep:finalize RepConstraint expr) : expr
(define-syntax (rep:finalize stx)
  (syntax-case stx ()
    [(_ a #s(rep:optional _ _ defaults) v)
     (with-syntax ([#s(attr name _ _) #'a]
                   [(#s(action:bind da de) ...) #'defaults])
       (let ([default
              (for/or ([da (in-list (syntax->list #'(da ...)))]
                       [de (in-list (syntax->list #'(de ...)))])
                (with-syntax ([#s(attr dname _ _) da])
                  (and (bound-identifier=? #'name #'dname) de)))])
         (if default
             #`(or v #,default)
             #'v)))]
    [(_ a #s(rep:once _ _ _) v) #'v]
    [(_ a _ v) #'(reverse v)]))

;; (rep:min-number RepConstraint) : expr
(define-syntax (rep:min-number stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'1]
    [(_ #s(rep:optional _ _ _)) #'0]
    [(_ #s(rep:bounds min max _ _ _)) #'min]))

;; (rep:max-number RepConstraint) : expr
(define-syntax (rep:max-number stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'1]
    [(_ #s(rep:optional _ _ _)) #'1]
    [(_ #s(rep:bounds min max _ _ _)) #'max]))

;; (rep:combine RepConstraint expr expr) : expr
(define-syntax (rep:combine stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _) a b) #'a]
    [(_ #s(rep:optional _ _ _) a b) #'a]
    [(_ _ a b) #'(cons a b)]))

;; ----

(define-syntax expectation-of-reps/too-few
  (syntax-rules ()
    [(_ es rep #s(rep:once name too-few-msg too-many-msg) hpat)
     (cond [(or too-few-msg (name->too-few/once name))
            => (lambda (msg) (es-add-message msg es))]
           [(first-desc:H hpat) => (lambda (fd) (es-add-proper-pair fd es))]
           [else es])]
    [(_ es rep #s(rep:optional name too-many-msg _) hpat)
     (error 'syntax-parse "INTERNAL ERROR: impossible (too-few)")]
    [(_ es rep #s(rep:bounds min max name too-few-msg too-many-msg) hpat)
     (cond [(or too-few-msg (name->too-few name))
            => (lambda (msg) (es-add-message msg es))]
           [(first-desc:H hpat) => (lambda (fd) (es-add-proper-pair fd es))]
           [else es])]))

(define-syntax expectation-of-reps/too-many
  (syntax-rules ()
    [(_ es rep #s(rep:once name too-few-msg too-many-msg))
     (es-add-message (or too-many-msg (name->too-many name)) es)]
    [(_ es rep #s(rep:optional name too-many-msg _))
     (es-add-message (or too-many-msg (name->too-many name)) es)]
    [(_ es rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (es-add-message (or too-many-msg (name->too-many name)) es)]))
