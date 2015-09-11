#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     syntax/private/id-table
                     syntax/keyword
                     racket/syntax
                     "minimatch.rkt"
                     "rep-attrs.rkt"
                     "rep-data.rkt"
                     "rep-patterns.rkt"
                     "rep.rkt"
                     "kws.rkt"
                     "opt.rkt"
                     "txlift.rkt")
         "keywords.rkt"
         racket/syntax
         racket/stxparam
         syntax/stx
         racket/struct
         syntax/parse/private/residual ;; keep abs. path
         syntax/parse/private/runtime  ;; keep abs.path 
         syntax/parse/private/runtime-reflect) ;; keep abs. path

;; ============================================================

(provide define-syntax-class
         define-splicing-syntax-class
         define-integrable-syntax-class
         syntax-parse
         syntax-parser
         define/syntax-parse
         syntax-parser/template
         parser/rhs
         define-eh-alternative-set
         (for-syntax rhs->parser))

(begin-for-syntax
 (define (tx:define-*-syntax-class stx splicing?)
   (syntax-case stx ()
     [(_ header . rhss)
      (parameterize ((current-syntax-context stx))
        (let-values ([(name formals arity)
                      (let ([p (check-stxclass-header #'header stx)])
                        (values (car p) (cadr p) (caddr p)))])
          (let ([the-rhs (parse-rhs #'rhss #f splicing? #:context stx)])
            (with-syntax ([name name]
                          [formals formals]
                          [parser (generate-temporary (format-symbol "parse-~a" name))]
                          [arity arity]
                          [attrs (rhs-attrs the-rhs)]
                          [options (rhs-options the-rhs)])
              #`(begin (define-syntax name
                         (stxclass 'name 'arity
                                   'attrs
                                   (quote-syntax parser)
                                   '#,splicing?
                                   options
                                   #f))
                       (define-values (parser)
                         (parser/rhs name formals attrs rhss #,splicing? #,stx)))))))])))

(define-syntax define-syntax-class
  (lambda (stx) (tx:define-*-syntax-class stx #f)))
(define-syntax define-splicing-syntax-class
  (lambda (stx) (tx:define-*-syntax-class stx #t)))

(define-syntax (define-integrable-syntax-class stx)
  (syntax-case stx (quote)
    [(_ name (quote description) predicate)
     (with-syntax ([parser (generate-temporary (format-symbol "parse-~a" (syntax-e #'name)))]
                   [no-arity no-arity])
       #'(begin (define-syntax name
                  (stxclass 'name no-arity '()
                            (quote-syntax parser)
                            #f
                            '#s(options #t #t)
                            (integrate (quote-syntax predicate) 'description)))
                (define (parser x cx pr es fh0 cp0 rl success)
                  (if (predicate x)
                      (success fh0)
                      (let ([es (es-add-thing pr 'description #t rl es)])
                        (fh0 (failure pr es)))))))]))

(define-syntax (parser/rhs stx)
  (syntax-case stx ()
    [(parser/rhs name formals relsattrs rhss splicing? ctx)
     (with-disappeared-uses
      (let ()
        (define the-rhs
          (parameterize ((current-syntax-context #'ctx))
            (parse-rhs #'rhss (syntax->datum #'relsattrs) (syntax-e #'splicing?)
                       #:context #'ctx)))
        (rhs->parser #'name #'formals #'relsattrs the-rhs (syntax-e #'splicing?))))]))

(begin-for-syntax
 (define (rhs->parser name formals relsattrs the-rhs splicing?)
   (define-values (transparent? description variants defs commit? delimit-cut?)
     (match the-rhs
       [(rhs _ _ transparent? description variants defs (options commit? delimit-cut?) _)
        (values transparent? description variants defs commit? delimit-cut?)]))
   (define vdefss (map variant-definitions variants))
   (define formals* (rewrite-formals formals #'x #'rl))
   (define body
     (cond [(null? variants)
            #'(fail (failure pr es))]
           [splicing?
            (with-syntax ([(alternative ...)
                           (for/list ([variant (in-list variants)])
                             (define pattern (variant-pattern variant))
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
                                                           success cp0 commit?))))])
              #'(try alternative ...))]
           [else
            (with-syntax ([matrix
                           (optimize-matrix
                            (for/list ([variant (in-list variants)])
                              (define pattern (variant-pattern variant))
                              (with-syntax ([iattrs (pattern-attrs pattern)]
                                            [relsattrs relsattrs]
                                            [commit? commit?])
                                (pk1 (list pattern)
                                     #'(variant-success relsattrs iattrs ()
                                                        success cp0 commit?)))))])
              #'(parse:matrix ((x cx pr es)) matrix))]))
   (with-syntax ([formals* formals*]
                 [(def ...) defs]
                 [((vdef ...) ...) vdefss]
                 [description (or description (symbol->string (syntax-e name)))]
                 [transparent? transparent?]
                 [delimit-cut? delimit-cut?]
                 [body body])
     #`(lambda (x cx pr es fh0 cp0 rl success . formals*)
         (with ([this-syntax x]
                [this-role rl])
               def ...
               vdef ... ...
               (#%expression
                (syntax-parameterize ((this-context-syntax
                                       (syntax-rules ()
                                         [(tbs) (ps-context-syntax pr)])))
                  (let ([es (es-add-thing pr description 'transparent? rl es)]
                        [pr (if 'transparent? pr (ps-add-opaque pr))])
                    (with ([fail-handler fh0]
                           [cut-prompt cp0])
                      ;; Update the prompt, if required
                      ;; FIXME: can be optimized away if no cut exposed within variants
                      (with-maybe-delimit-cut delimit-cut?
                        body))))))))))

(define-syntax (syntax-parse stx)
  (syntax-case stx ()
    [(syntax-parse stx-expr . clauses)
     (quasisyntax/loc stx
       (let ([x (datum->syntax #f stx-expr)])
         (with ([this-syntax x])
           (parse:clauses x clauses body-sequence #,((make-syntax-introducer) stx)))))]))

(define-syntax (syntax-parser stx)
  (syntax-case stx ()
    [(syntax-parser . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (with ([this-syntax x])
             (parse:clauses x clauses body-sequence #,((make-syntax-introducer) stx))))))]))

(define-syntax (syntax-parser/template stx)
  (syntax-case stx ()
    [(syntax-parser/template ctx . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (with ([this-syntax x])
             (parse:clauses x clauses one-template ctx)))))]))

(define-syntax (define/syntax-parse stx)
  (syntax-case stx ()
    [(define/syntax-parse pattern . rest)
     (let-values ([(rest pattern defs)
                   (parse-pattern+sides #'pattern
                                        #'rest
                                        #:splicing? #f
                                        #:decls (new-declenv null)
                                        #:context stx)])
       (let ([expr
              (syntax-case rest ()
                [( expr ) #'expr]
                [_ (raise-syntax-error #f "bad syntax" stx)])]
             [attrs (pattern-attrs pattern)])
         (with-syntax ([(a ...) attrs]
                       [(#s(attr name _ _) ...) attrs]
                       [pattern pattern]
                       [(def ...) defs]
                       [expr expr])
           #'(defattrs/unpack (a ...)
               (let* ([x (datum->syntax #f expr)]
                      [cx x]
                      [pr (ps-empty x x)]
                      [es #f]
                      [fh0 (syntax-patterns-fail x)])
                 (parameterize ((current-syntax-context x))
                   def ...
                   (#%expression
                    (with ([fail-handler fh0]
                           [cut-prompt fh0])
                          (parse:S x cx pattern pr es
                                   (list (attribute name) ...))))))))))]))

;; ============================================================

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

(stxclass-parser x cx pr es fail-handler cut-prompt role success-proc arg ...) : Ans

  success-proc:
    for stxclass, is (fail-handler attr-value ... -> Ans)
    for splicing-stxclass, is (fail-handler rest-x rest-cx rest-pr attr-value -> Ans)
  fail-handler, cut-prompt : failure -> Ans

Fail-handler is normally represented with stxparam 'fail-handler', but must be
threaded through stxclass calls (in through stxclass-parser, out through
success-proc) to support backtracking. Cut-prompt is never changed within
stxclass or within alternative, so no threading needed.

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

(begin-for-syntax
 (define (rewrite-formals fstx x-id rl-id)
   (with-syntax ([x x-id]
                 [rl rl-id])
     (let loop ([fstx fstx])
       (syntax-case fstx ()
         [([kw arg default] . more)
          (keyword? (syntax-e #'kw))
          (cons #'(kw arg (with ([this-syntax x] [this-role rl]) default))
                (loop #'more))]
         [([arg default] . more)
          (not (keyword? (syntax-e #'kw)))
          (cons #'(arg (with ([this-syntax x] [this-role rl]) default))
                (loop #'more))]
         [(formal . more)
          (cons #'formal (loop #'more))]
         [_ fstx])))))

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
    [(variant-success relsattrs iattrs (also ...) success cp0 commit?)
     #`(with-maybe-reset-fail commit? cp0
         (base-success-expr iattrs relsattrs (also ...) success))]))

;; (with-maybe-reset-fail bool id expr)
(define-syntax with-maybe-reset-fail
  (syntax-rules ()
    [(wmrs #t cp0 k)
     (with ([fail-handler cp0]) k)]
    [(wmrs #f cp0 k)
     k]))

;; (base-success-expr iattrs relsattrs (also:id ...) success) : expr[Ans]
(define-syntax (base-success-expr stx)
  (syntax-case stx ()
    [(base-success-expr iattrs relsattrs (also ...) success)
     (let ([reliattrs
            (reorder-iattrs (wash-sattrs #'relsattrs)
                            (wash-iattrs #'iattrs))])
       (with-syntax ([(#s(attr name _ _) ...) reliattrs])
         #'(success fail-handler also ... (attribute name) ...)))]))

;; ----

;; (parse:clauses x clauses ctx)
(define-syntax (parse:clauses stx)
  (syntax-case stx ()
    [(parse:clauses x clauses body-mode ctx)
     ;; if templates? is true, expect one form after kwargs in clause, wrap it with syntax
     ;; otherwise, expect non-empty body sequence (defs and exprs)
     (with-disappeared-uses
      (with-txlifts
       (lambda ()
        (define who
          (syntax-case #'ctx ()
            [(m . _) (identifier? #'m) #'m]
            [_ 'syntax-parse]))
        (define-values (chunks clauses-stx)
          (parse-keyword-options #'clauses parse-directive-table
                                 #:context #'ctx
                                 #:no-duplicates? #t))
        (define context
          (options-select-value chunks '#:context #:default #'x))
        (define colon-notation?
          (not (assq '#:disable-colon-notation chunks)))
        (define-values (decls0 defs)
          (get-decls+defs chunks #t #:context #'ctx))
        ;; for-clause : stx -> (values pattern stx (listof stx))
        (define (for-clause clause)
          (syntax-case clause ()
            [[p . rest]
             (let-values ([(rest pattern defs2)
                           (parameterize ((stxclass-colon-notation? colon-notation?))
                             (parse-pattern+sides #'p #'rest
                                                  #:splicing? #f
                                                  #:decls decls0
                                                  #:context #'ctx))])
               (let ([body-expr
                      (case (syntax-e #'body-mode)
                        ((one-template)
                         (syntax-case rest ()
                           [(template)
                            #'(syntax template)]
                           [_ (raise-syntax-error #f "expected exactly one template" #'ctx)]))
                        ((body-sequence)
                         (syntax-case rest ()
                           [(e0 e ...) #'(let () e0 e ...)]
                           [_ (raise-syntax-error #f "expected non-empty clause body"
                                                  #'ctx clause)]))
                        (else
                         (raise-syntax-error #f "internal error: unknown body mode" #'ctx #'body-mode)))])
                 (values pattern body-expr defs2)))]
            [_ (raise-syntax-error #f "expected clause" #'ctx clause)]))
        (unless (stx-list? clauses-stx)
          (raise-syntax-error #f "expected sequence of clauses" #'ctx))
        (define-values (patterns body-exprs defs2s)
          (for/lists (patterns body-exprs defs2s) ([clause (in-list (stx->list clauses-stx))])
            (for-clause clause)))
        (with-syntax ([(def ...) (apply append (get-txlifts-as-definitions) defs defs2s)])
          #`(let* ([ctx0 (normalize-context '#,who #,context x)]
                   [pr (ps-empty x (cadr ctx0))]
                   [es #f]
                   [cx x]
                   [fh0 (syntax-patterns-fail ctx0)])
              def ...
              (parameterize ((current-syntax-context (cadr ctx0)))
                (with ([fail-handler fh0]
                       [cut-prompt fh0])
                  #,(cond [(pair? patterns)
                           (with-syntax ([matrix
                                          (optimize-matrix
                                           (for/list ([pattern (in-list patterns)]
                                                      [body-expr (in-list body-exprs)])
                                             (pk1 (list pattern) body-expr)))])
                             #'(parse:matrix ((x cx pr es)) matrix))
                           #|
                           (with-syntax ([(alternative ...)
                                          (for/list ([pattern (in-list patterns)]
                                                     [body-expr (in-list body-exprs)])
                                            #`(parse:S x cx #,pattern pr es #,body-expr))])
                             #`(try alternative ...))
                           |#]
                          [else
                           #`(fail (failure pr es))]))))))))]))

;; ----

;; (parse:matrix ((x cx pr es) ...) (PK ...)) : expr[Ans]
;; (parse:matrix (in1 ... inN) (#s(pk1 (P11 ... P1N) e1) ... #s(pk1 (PM1 ... PMN) eM)))
;; represents the matching matrix
;;   [_in1_..._inN_|____]
;;   [ P11 ... P1N | e1 ]
;;   [  ⋮       ⋮  |  ⋮ ]
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
    [(parse:pk ((x cx pr es) . ins) #s(pk/pair proper? inner))
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
             (let ([es* (if (and 'proper? (null? datum))
                            (es-add-proper-pair (first-desc:matrix inner) es)
                            es)])
               (fail (failure pr es*)))))]
    [(parse:pk (in1 . ins) #s(pk/and inner))
     #'(parse:matrix (in1 in1 . ins) inner)]))

(define-syntax (first-desc:matrix stx)
  (syntax-case stx ()
    [(fdm (#s(pk (pat1 . pats) k)))
     #'(first-desc:S pat1)]
    [(fdm (pk ...))
     ;; FIXME
     #'#f]))

;; ----

;; (parse:S x cx S-pattern pr es k) : expr[Ans]
;; In k: attrs(S-pattern) are bound.
(define-syntax (parse:S stx)
  (syntax-case stx ()
    [(parse:S x cx pattern0 pr es k)
     (syntax-case #'pattern0 ()
       [#s(internal-rest-pattern rest-x rest-cx rest-pr)
        #`(let ([rest-x x]
                [rest-cx cx]
                [rest-pr pr])
            k)]
       [#s(pat:any _attrs)
        #'k]
       [#s(pat:var _attrs name #f _ () _ _ _)
        #'(let-attributes ([#s(attr name 0 #t) (datum->syntax cx x cx)])
            k)]
       [#s(pat:var _attrs name parser argu (nested-a ...) attr-count commit? role)
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) (datum->syntax cx x cx)])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #'(app-argu parser x cx pr es fail-handler cut-prompt role
                          (lambda (fh av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs av ...)
                              (with ([fail-handler (lambda (fs) (values fs (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es fail-handler cut-prompt role
                                            (lambda (fh av ...) (values #f av ...))
                                            argu)))])
                  (if fs
                      (fail fs)
                      (let-attributes (name-attr ...)
                        (let-attributes* ((nested-a ...) (av ...))
                          k))))))]
       [#s(pat:reflect _attrs obj argu attr-decls name (nested-a ...))
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) (datum->syntax cx x cx)])
                           #'())])
          (with-syntax ([arity (arguments->arity (syntax->datum #'argu))])
            #'(let ([parser (reflect-parser obj 'arity 'attr-decls #f)])
                (app-argu parser x cx pr es fail-handler cut-prompt #f
                          (lambda (fh . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                (with ([fail-handler fh])
                                  k))))
                          argu))))]
       [#s(pat:datum attrs datum)
        #`(let ([d (if (syntax? x) (syntax-e x) x)])
            (if (equal? d (quote datum))
                k
                (fail (failure pr (es-add-atom 'datum es)))))]
       [#s(pat:literal attrs literal input-phase lit-phase)
        #`(if (and (identifier? x)
                   (free-identifier=? x (quote-syntax literal) input-phase lit-phase))
              k
              (fail (failure pr (es-add-literal (quote-syntax literal) es))))]
       [#s(pat:action attrs action subpattern)
        #'(parse:A x cx action pr es (parse:S x cx subpattern pr es k))]
       [#s(pat:head attrs head tail)
        #`(parse:H x cx rest-x rest-cx rest-pr head pr es
                   (parse:S rest-x rest-cx tail rest-pr es k))]
       [#s(pat:dots attrs head tail)
        #`(parse:dots x cx head tail pr es k)]
       [#s(pat:and attrs subpatterns)
        (for/fold ([k #'k]) ([subpattern (in-list (reverse (syntax->list #'subpatterns)))])
          #`(parse:S x cx #,subpattern pr es #,k))]
       [#s(pat:or (a ...) (subpattern ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh])
                         k)))])
              (try (parse:S x cx subpattern pr es
                            (disjunct subpattern success () (id ...)))
                   ...)))]
       [#s(pat:not () subpattern)
        #`(let* ([fh0 fail-handler]
                 [pr0 pr]
                 [es0 es]
                 [fail-to-succeed
                  (lambda (fs) k)])
            ;; ~not implicitly prompts to be safe,
            ;; but ~! not allowed within ~not (unless within ~delimit-cut, etc)
            ;; (statically checked!)
            (with ([fail-handler fail-to-succeed]
                   [cut-prompt fail-to-succeed]) ;; to be safe
              (parse:S x cx subpattern pr es
                       (fh0 (failure pr0 es0)))))]
       [#s(pat:pair _attrs proper? head tail)
        #`(let-values ([(datum cx)
                        (if (syntax? x)
                            (values (syntax-e x) x)
                            (values x cx))])
            (if (pair? datum)
                (let ([hx (car datum)]
                      [hcx (car datum)]
                      [hpr (ps-add-car pr)]
                      [tx (cdr datum)]
                      [tpr (ps-add-cdr pr)])
                  (parse:S hx hcx head hpr es
                           (parse:S tx cx tail tpr es k)))
                (let ([es* (if (and 'proper? (null? datum)) (es-add-proper-pair (first-desc:S head) es) es)])
                  (fail (failure pr es*)))))]
       [#s(pat:vector _attrs subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (vector? datum)
                (let ([datum (vector->list datum)]
                      [vcx (if (syntax? x) x cx)] ;; FIXME: (vector? datum) => (syntax? x) ???
                      [pr* (ps-add-unvector pr)])
                  (parse:S datum vcx subpattern pr* es k))
                (fail (failure pr es))))]
       [#s(pat:box _attrs subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (box? datum)
                (let ([datum (unbox datum)]
                      [bcx (if (syntax? x) x cx)] ;; FIXME: (box? datum) => (syntax? x) ???
                      [pr* (ps-add-unbox pr)])
                  (parse:S datum bcx subpattern pr* es k))
                (fail (failure pr es))))]
       [#s(pat:pstruct _attrs key subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (let ([xkey (prefab-struct-key datum)])
                  (and xkey (equal? xkey 'key)))
                (let ([datum (struct->list datum)]
                      [scx (if (syntax? x) x cx)] ;; FIXME: (struct? datum) => (syntax? x) ???
                      [pr* (ps-add-unpstruct pr)])
                  (parse:S datum scx subpattern pr* es k))
                (fail (failure pr es))))]
       [#s(pat:describe attrs pattern description transparent? role)
        #`(let ([es* (es-add-thing pr description transparent? role es)]
                [pr* (if 'transparent? pr (ps-add-opaque pr))])
            (parse:S x cx pattern pr* es* k))]
       [#s(pat:delimit attrs pattern)
        #`(let ([cp0 cut-prompt])
            (with ([cut-prompt fail-handler])
              (parse:S x cx pattern pr es (with ([cut-prompt cp0]) k))))]
       [#s(pat:commit attrs pattern)
        #`(let ([fh0 fail-handler]
                [cp0 cut-prompt])
            (with ([cut-prompt fh0])
              (parse:S x cx pattern pr es
                       (with ([cut-prompt cp0]
                              [fail-handler fh0])
                         k))))]
       [#s(pat:post attrs pattern)
        #`(let ([pr* (ps-add-post pr)])
            (parse:S x cx pattern pr* es k))]
       [#s(pat:integrated _attrs name predicate description role)
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) x*])
                           #'())])
          #'(let ([x* (datum->syntax cx x cx)])
              (if (predicate x*)
                  (let-attributes (name-attr ...) k)
                  (let ([es* (es-add-thing pr 'description #t role es)])
                    (fail (failure pr es*))))))])]))

;; (first-desc:S S-pattern) : expr[FirstDesc]
(define-syntax (first-desc:S stx)
  (syntax-case stx ()
    [(fds p)
     (syntax-case #'p ()
       [#s(pat:any _as)
        #''(any)]
       [#s(pat:var _as name #f _ () _ _ _)
        #''(any)]
       [#s(pat:var _ ...)
        #'#f] ;; FIXME: need access to (constant) description as field
       [#s(pat:datum _as d)
        #''(datum d)]
       [#s(pat:literal _as id _ip _lp)
        #''(literal id)]
       [#s(pat:describe _as _p description _t? _role)
        #'description] ;; FIXME??? only constants?
       [#s(pat:delimit _a pattern)
        #'(first-desc:S pattern)]
       [#s(pat:commit _a pattern)
        #'(first-desc:S pattern)]
       [#s(pat:post _a pattern)
        #'(first-desc:S pattern)]
       [#s(pat:integrated _as _name _pred description _role)
        #''description]
       [_ #'#f])]))

;; (disjunct ???-pattern success (pre:expr ...) (id:id ...)) : expr[Ans]
(define-syntax (disjunct stx)
  (syntax-case stx ()
    [(disjunct pattern success (pre ...) (id ...))
     (with-syntax ([(#s(attr sub-id _ _) ...) (pattern-attrs (wash #'pattern))])
       (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
         #`(let ([alt-sub-id (attribute sub-id)] ...)
             (let ([id #f] ...)
               (let ([sub-id alt-sub-id] ...)
                 (success fail-handler pre ... id ...))))))]))

;; (disjunct/sides clauses success (pre:expr ...) (id:id ...)) : expr[Ans]
(define-syntax (disjunct/sides stx)
  (syntax-case stx ()
    [(disjunct/sides clauses success (pre ...) (id ...))
     (with-syntax ([(#s(clause:attr #s(attr sub-id _ _) _) ...) #'clauses])
       (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
         #`(let ([alt-sub-id (attribute sub-id)] ...)
             (let ([id #f] ...)
               (let ([sub-id alt-sub-id] ...)
                 (success fail-handler pre ... id ...))))))]))

;; (parse:A x cx A-pattern pr es k) : expr[Ans]
;; In k: attrs(A-pattern) are bound.
(define-syntax (parse:A stx)
  (syntax-case stx ()
    [(parse:A x cx pattern0 pr es k)
     (syntax-case #'pattern0 ()
       [#s(action:cut _)
        #'(with ([fail-handler cut-prompt]) k)]
       [#s(action:bind _ (side ...))
        #'(bind/sides (side ...) k)]
       [#s(action:fail _ condition message)
        #`(let ([c (wrap-user-code condition)])
            (if c
                (let ([pr* (if (syntax? c)
                               (ps-add-stx pr c)
                               pr)]
                      [es* (es-add-message message es)])
                  (fail (failure pr* es*)))
                k))]
       [#s(action:parse _ pattern expr)
        #`(let* ([y (datum->syntax/with-clause (wrap-user-code expr))]
                 [cy y]
                 [pr* (ps-add-stx pr y)])
            (parse:S y cy pattern pr* es k))]
       [#s(action:do _ (stmt ...))
        #'(let () (no-shadow stmt) ... (#%expression k))]
       [#s(action:post _ pattern)
        #'(let ([pr* (ps-add-post pr)])
            (parse:A x cx pattern pr* es k))])]))

;; (bind/sides clauses k) : expr[Ans]
;; In k: attrs(clauses) are bound.
(define-syntax (bind/sides stx)
  (syntax-case stx ()
    [(_ (side ...) k)
     (for/fold ([k #'k]) ([side (in-list (reverse (syntax->list #'(side ...))))])
       (syntax-case side ()
         [#s(clause:attr a expr)
            #`(let-attributes ([a (wrap-user-code expr)])
                #,k)]))]))

(begin-for-syntax
 ;; convert-list-pattern : ListPattern id -> SinglePattern
 ;; Converts '() datum pattern at end of list to bind (cons stx index)
 ;; to rest-var.
 (define (convert-list-pattern pattern end-pattern)
   (syntax-case pattern ()
     [#s(pat:datum () ())
      end-pattern]
     [#s(pat:action attrs action tail)
      (with-syntax ([tail (convert-list-pattern #'tail end-pattern)])
        #'#s(pat:action attrs action tail))]
     [#s(pat:head attrs head tail)
      (with-syntax ([tail (convert-list-pattern #'tail end-pattern)])
        #'#s(pat:head attrs head tail))]
     [#s(pat:dots attrs head tail)
      (with-syntax ([tail (convert-list-pattern #'tail end-pattern)])
        #'#s(pat:dots attrs head tail))]
     [#s(pat:pair attrs proper? head-part tail-part)
      (with-syntax ([tail-part (convert-list-pattern #'tail-part end-pattern)])
        #'#s(pat:pair attrs proper? head-part tail-part))])))

;; (parse:H x cx rest-x rest-cx rest-pr H-pattern pr es k)
;; In k: rest, rest-pr, attrs(H-pattern) are bound.
(define-syntax (parse:H stx)
  (syntax-case stx ()
    [(parse:H x cx rest-x rest-cx rest-pr head pr es k)
     (syntax-case #'head ()
       [#s(hpat:describe _ pattern description transparent? role)
        #`(let ([es* (es-add-thing pr description transparent? role es)]
                [pr* (if 'transparent? pr (ps-add-opaque pr))])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr* es*
                     (let ([rest-pr (if 'transparent? rest-pr (ps-pop-opaque rest-pr))])
                       k)))]
       [#s(hpat:var _attrs name parser argu (nested-a ...) attr-count commit? role)
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t)
                               (stx-list-take x (ps-difference pr rest-pr))])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #`(app-argu parser x cx pr es fail-handler cut-prompt role
                          (lambda (fh rest-x rest-cx rest-pr av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs rest-x rest-cx rest-pr av ...)
                              (with ([fail-handler (lambda (fs) (values fs #f #f #f (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es fail-handler cut-prompt role
                                            (lambda (fh rest-x rest-cx rest-pr av ...)
                                              (values #f rest-x rest-cx rest-pr av ...))
                                            argu)))])
                  (if fs
                      (fail fs)
                      (let-attributes (name-attr ...)
                        (let-attributes* ((nested-a ...) (av ...))
                          k))))))]
       [#s(hpat:reflect _attrs obj argu attr-decls name (nested-a ...))
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t)
                               (stx-list-take x (ps-difference pr rest-pr))])
                           #'())])
          (with-syntax ([arity (arguments->arity (syntax->datum #'argu))])
            #'(let ([parser (reflect-parser obj 'arity 'attr-decls #t)])
                (app-argu parser x cx pr es fail-handler cut-prompt #f
                          (lambda (fh rest-x rest-cx rest-pr . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                 (with ([fail-handler fh])
                                   k))))
                          argu))))]
       [#s(hpat:and (a ...) head single)
        #`(let ([cx0 cx])
            (parse:H x cx rest-x rest-cx rest-pr head pr es
                     (let ([lst (stx-list-take x (ps-difference pr rest-pr))])
                       (parse:S lst cx0 single pr es k))))]
       [#s(hpat:or (a ...) (subpattern ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh rest-x rest-cx rest-pr id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh])
                         k)))])
              (try (parse:H x cx rest-x rest-cx rest-pr subpattern pr es
                            (disjunct subpattern success
                                      (rest-x rest-cx rest-pr) (id ...)))
                   ...)))]
       [#s(hpat:seq attrs pattern)
        (with-syntax ([pattern
                       (convert-list-pattern
                        #'pattern
                        #'#s(internal-rest-pattern rest-x rest-cx rest-pr))])
          #'(parse:S x cx pattern pr es k))]
       [#s(hpat:action attrs action subpattern)
        #'(parse:A x cx action pr es (parse:H x cx rest-x rest-cx rest-pr subpattern pr es k))]
       [#s(hpat:optional (a ...) pattern defaults)
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh rest-x rest-cx rest-pr id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh])
                         k)))])
              (try (parse:H x cx rest-x rest-cx rest-pr pattern pr es 
                            (success fail-handler rest-x rest-cx rest-pr (attribute id) ...))
                   (let ([rest-x x]
                         [rest-cx cx]
                         [rest-pr pr])
                     (bind/sides defaults
                       (disjunct/sides defaults success
                                       (rest-x rest-cx rest-pr)
                                       (id ...)))))))]
       [#s(hpat:delimit _attrs pattern)
        #'(let ([cp0 cut-prompt])
            (with ([cut-prompt fail-handler])
              (parse:H x cx rest-x rest-cx rest-pr pattern pr es
                       (with ([cut-prompt cp0]) k))))]
       [#s(hpat:commit attrs pattern)
        #`(let ([fh0 fail-handler]
                [cp0 cut-prompt])
            (with ([cut-prompt fh0])
              (parse:H x cx rest-x rest-cx rest-pr pattern pr es
                       (with ([cut-prompt cp0]
                              [fail-handler fh0])
                         k))))]
       [#s(hpat:post _ pattern)
        #'(let ([pr (ps-add-post pr)])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr es k))]
       [#s(hpat:peek _ pattern)
        #`(let ([saved-x x] [saved-cx cx] [saved-pr pr])
            (parse:H x cx dummy-x dummy-cx dummy-pr pattern pr es
                     (let ([rest-x saved-x] [rest-cx saved-cx] [rest-pr saved-pr])
                       k)))]
       [#s(hpat:peek-not () subpattern)
        #`(let* ([fh0 fail-handler]
                 [pr0 pr]
                 [es0 es]
                 [fail-to-succeed
                  (lambda (fs)
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
                       (fh0 (failure pr0 es0)))))]
       [_
        (with-syntax ([attrs (pattern-attrs (wash #'head))])
          #'(parse:S x cx
                     ;; FIXME: consider proper-list-pattern? (yes is consistent with ~seq)
                     #s(pat:pair attrs #t head #s(internal-rest-pattern rest-x rest-cx rest-pr))
                     pr es k))])]))

;; (parse:dots x cx EH-pattern S-pattern pr es k) : expr[Ans]
;; In k: attrs(EH-pattern, S-pattern) are bound.
(define-syntax (parse:dots stx)
  (syntax-case stx ()
    ;; == Specialized cases
    ;; -- (x ... . ())
    [(parse:dots x cx (#s(ehpat (attr0)
                                #s(pat:var _attrs name #f _ () _ _ _)
                                #f))
                 #s(pat:datum () ()) pr es k)
     #'(let-values ([(status result) (predicate-ellipsis-parser x cx pr es void #f #f)])
         (case status
           ((ok) (let-attributes ([attr0 result]) k))
           (else (fail result))))]
    ;; -- (x:sc ... . ()) where sc is an integrable stxclass like id or expr
    [(parse:dots x cx (#s(ehpat (attr0)
                                #s(pat:integrated _attrs _name pred? desc role)
                                #f))
                 #s(pat:datum () ()) pr es k)
     #'(let-values ([(status result) (predicate-ellipsis-parser x cx pr es pred? desc role)])
         (case status
           ((ok) (let-attributes ([attr0 result]) k))
           (else (fail result))))]
    ;; -- (x:sc ... . ()) where sc is a stxclass with commit
    ;; Since head pattern does commit, no need to thread fail-handler, cut-prompt through.
    ;; Microbenchmark suggests this isn't a useful specialization
    ;; (probably try-or-pair/null-check already does the useful part)
    ;; == General case
    [(parse:dots x cx (#s(ehpat head-attrs head head-repc) ...) tail pr es k)
     (let ()
       (define repcs (wash-list wash #'(head-repc ...)))
       (define rep-ids (for/list ([repc (in-list repcs)])
                         (and repc (generate-temporary 'rep))))
       (define rel-repcs (filter values repcs))
       (define rel-rep-ids (filter values rep-ids))
       (define aattrs
         (for/list ([head-attrs (in-list (syntax->list #'(head-attrs ...)))]
                    [repc (in-list repcs)]
                    #:when #t
                    [a (in-list (wash-iattrs head-attrs))])
           (cons a repc)))
       (define attrs (map car aattrs))
       (define attr-repcs (map cdr aattrs))
       (define ids (map attr-name attrs))
       (with-syntax ([(id ...) ids]
                     [(alt-id ...) (generate-temporaries ids)]
                     [reps rel-rep-ids]
                     [(head-rep ...) rep-ids]
                     [(rel-rep ...) rel-rep-ids]
                     [(rel-repc ...) rel-repcs]
                     [(a ...) attrs]
                     [(attr-repc ...) attr-repcs]
                     [tail-pattern-is-null?
                      (equal? (syntax->datum #'tail) '#s(pat:datum () ()))])
         (define/with-syntax alt-map #'((id . alt-id) ...))
         (define/with-syntax loop-k
           #'(dots-loop dx* dcx* loop-pr* fail-handler rel-rep ... alt-id ...))
         #`(let ()
             ;; dots-loop : stx progress rel-rep ... alt-id ... -> Ans
             (define (dots-loop dx dcx loop-pr fh rel-rep ... alt-id ...)
               (with ([fail-handler fh])
                 (try-or-pair/null-check tail-pattern-is-null? dx dcx loop-pr es
                   (try (parse:EH dx dcx loop-pr head-repc dx* dcx* loop-pr* alt-map head-rep
                                  head es loop-k)
                        ...)
                   (cond [(< rel-rep (rep:min-number rel-repc))
                          (let ([es (expectation-of-reps/too-few es rel-rep rel-repc)])
                            (fail (failure loop-pr es)))]
                         ...
                         [else
                          (let-attributes ([a (rep:finalize a attr-repc alt-id)] ...)
                            (parse:S dx dcx tail loop-pr es k))]))))
             (let ([rel-rep 0] ...
                   [alt-id (rep:initial-value attr-repc)] ...)
               (dots-loop x cx pr fail-handler rel-rep ... alt-id ...)))))]))

;; (try-or-pair/null-check bool x cx es pr pair-alt maybe-null-alt)
(define-syntax try-or-pair/null-check
  (syntax-rules ()
    [(topc #t x cx pr es pair-alt null-alt)
     (cond [(stx-pair? x) pair-alt]
           [(stx-null? x) null-alt]
           [else (fail (failure pr es))])]
    [(topc _ x cx pr es alt1 alt2)
     (try alt1 alt2)]))

;; (parse:EH x cx pr repc x* cx* pr* alts rep H-pattern es k) : expr[Ans]
;; In k: x*, cx*, pr*, alts`attrs(H-pattern) are bound and rep is shadowed.
(define-syntax (parse:EH stx)
  (syntax-case stx ()
    [(parse:EH x cx pr repc x* cx* pr* alts rep head es k)
     (let ()
       (define/with-syntax k*
         (let* ([main-attrs (wash-iattrs (pattern-attrs (wash #'head)))]
                [ids (map attr-name main-attrs)]
                [alt-ids
                 (let ([table (make-bound-id-table)])
                   (for ([entry (in-list (syntax->list #'alts))])
                     (let ([entry (syntax-e entry)])
                       (bound-id-table-set! table (car entry) (cdr entry))))
                   (for/list ([id (in-list ids)]) (bound-id-table-ref table id)))])
           (with-syntax ([(id ...) ids]
                         [(alt-id ...) alt-ids]
                         [(alt-a ...) (map rename-attr main-attrs alt-ids)])
             #`(let ([alt-id (rep:combine repc (attribute id) alt-id)] ...)
                 k))))
       (syntax-case #'repc ()
         [#f #`(parse:H x cx x* cx* pr* head pr es k*)]
         [_  #`(parse:H x cx x* cx* pr* head pr es
                        (if (< rep (rep:max-number repc))
                            (let ([rep (add1 rep)]) k*)
                            (let ([es* (expectation-of-reps/too-many es rep repc)])
                              (fail (failure pr* es*)))))]))]))

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
                   [(#s(clause:attr da de) ...) #'defaults])
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
    [(_ es rep #s(rep:once name too-few-msg too-many-msg))
     (es-add-message (or too-few-msg (name->too-few/once name)) es)]
    [(_ es rep #s(rep:optional name too-many-msg _))
     (error 'syntax-parse "INTERNAL ERROR: impossible (too-few)")]
    [(_ es rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (es-add-message (or too-few-msg (name->too-few name)) es)]))

(define-syntax expectation-of-reps/too-many
  (syntax-rules ()
    [(_ es rep #s(rep:once name too-few-msg too-many-msg))
     (es-add-message (or too-many-msg (name->too-many name)) es)]
    [(_ es rep #s(rep:optional name too-many-msg _))
     (es-add-message (or too-many-msg (name->too-many name)) es)]
    [(_ es rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (es-add-message (or too-many-msg (name->too-many name)) es)]))

;; ====

(define-syntax (define-eh-alternative-set stx)
  (define (parse-alt x)
    (syntax-case x (pattern)
      [(pattern alt)
       #'alt]
      [else
       (wrong-syntax x "expected eh-alternative-set alternative")]))
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(_ name a ...)
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected identifier"))
       (let* ([alts (map parse-alt (syntax->list #'(a ...)))]
              [decls (new-declenv null #:conventions null)]
              [ehpat+hstx-list
               (apply append
                      (for/list ([alt (in-list alts)])
                        (parse*-ellipsis-head-pattern alt decls #t #:context stx)))]
              [eh-alt+defs-list
               (for/list ([ehpat+hstx (in-list ehpat+hstx-list)])
                 (let ([ehpat (car ehpat+hstx)]
                       [hstx (cadr ehpat+hstx)])
                   (cond [(syntax? hstx)
                          (with-syntax ([(parser) (generate-temporaries '(eh-alt-parser))])
                            (let ([attrs (iattrs->sattrs (pattern-attrs (ehpat-head ehpat)))])
                              (list (eh-alternative (ehpat-repc ehpat) attrs #'parser)
                                    (list #`(define parser
                                              (parser/rhs parser () #,attrs
                                                          [#:description #f (pattern #,hstx)]
                                                          #t
                                                          #,stx))))))]
                         [(eh-alternative? hstx)
                          (list hstx null)]
                         [else
                          (error 'define-eh-alternative-set "internal error: unexpected ~e"
                                 hstx)])))]
              [eh-alts (map car eh-alt+defs-list)]
              [defs (apply append (map cadr eh-alt+defs-list))])
         (with-syntax ([(def ...) defs]
                       [(alt-expr ...)
                        (for/list ([alt (in-list eh-alts)])
                          (with-syntax ([repc-expr
                                         ;; repc structs are prefab; recreate using prefab
                                         ;; quasiquote exprs to avoid moving constructors
                                         ;; to residual module
                                         (syntax-case (eh-alternative-repc alt) ()
                                           [#f
                                            #''#f]
                                           [#s(rep:once n u o)
                                            #'`#s(rep:once ,(quote-syntax n)
                                                           ,(quote-syntax u)
                                                           ,(quote-syntax o))]
                                           [#s(rep:optional n o d)
                                            #'`#s(rep:optional ,(quote-syntax n)
                                                               ,(quote-syntax o)
                                                               ,(quote-syntax d))]
                                           [#s(rep:bounds min max n u o)
                                            #'`#s(rep:bounds ,(quote min)
                                                             ,(quote max)
                                                             ,(quote-syntax n)
                                                             ,(quote-syntax u)
                                                             ,(quote-syntax o))])]
                                        [attrs-expr
                                         #`(quote #,(eh-alternative-attrs alt))]
                                        [parser-expr
                                         #`(quote-syntax #,(eh-alternative-parser alt))])
                            #'(eh-alternative repc-expr attrs-expr parser-expr)))])
           #'(begin def ...
                    (define-syntax name
                      (eh-alternative-set (list alt-expr ...))))))])))
