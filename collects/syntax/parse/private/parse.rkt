#lang racket/base
(require (for-syntax racket/base
                     racket/private/sc
                     syntax/stx
                     syntax/id-table
                     syntax/keyword
                     unstable/syntax
                     "rep-data.rkt"
                     "rep.rkt"
                     "kws.rkt"
                     "txlift.rkt")
         racket/stxparam
         racket/list
         syntax/stx
         unstable/struct
         "runtime.rkt"
         "runtime-report.rkt"
         "runtime-reflect.rkt")
(provide (all-defined-out))

#|
Parsing protocol:

(parse:* <*> * progress-var expectstack-var success-expr) : Ans

*-stxclass-parser
  : stxish stx progress expectstack fail-handler cut-prompt success-proc arg ... -> Ans

<S> : x cx
<H> : x cx rest-x rest-cx rest-pr
<EH> : x cx ???
<A> : x cx

x is term to parse, usually syntax but can be pair, empty in cdr patterns
cx is most recent syntax object:
  if x must be coerced to syntax, use cx as lexctx and src

Usually sub-patterns processed in tail position,
but *can* do non-tail calls for:
  - ~commit
  - var of stxclass with ~commit
(Also safe to keep normal tail-call protocol.)
There is no real benefit to specializing ~commit, since it does not involve
creating a success closure.

|#

#|
Optimizations
  - commit protocol for stxclasses (but not ~commit, no point)
  - avoid choice point in (EH ... . ()) by eager pair check
  - integrable stxclasses (identifier, keyword, expr)
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
   (wash-list wash-sattr stx)))

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
  - fh, cp : id (var)
|#

;; (parse:rhs rhs relsattrs (arg:id ...) get-description:id splicing?)
;;   : expr[stxclass-parser]
;; Takes a list of the relevant attrs; order is significant!
(define-syntax (parse:rhs stx)
  (syntax-case stx ()
    [(parse:rhs #s(rhs _ _ transparent? _ variants (def ...)
                       #s(options commit? delimit-cut?) _integrate)
                relsattrs formals splicing? description)
     #'(lambda (x cx pr es fh0 cp0 success . formals)
         def ...
         (#%expression
          (with ([this-syntax x])
            (syntax-parameterize ((this-context-syntax
                                   (syntax-rules ()
                                     [(tbs) (ps-context-syntax pr)])))
              (let ([es (cons (cons (expect:thing description 'transparent?) x) es)]
                    [pr (if 'transparent? pr (ps-add-opaque pr))])
                (with ([fail-handler fh0]
                       [cut-prompt cp0])
                  ;; Update the prompt, if required
                  ;; FIXME: can be optimized away if no cut immediately within variants...
                  (with-maybe-delimit-cut delimit-cut?
                    (parse:variants x cx relsattrs variants splicing?
                                    pr es success cp0 commit?))))))))]))

;; (with-maybe-delimit-cut bool expr)
(define-syntax with-maybe-delimit-cut
  (syntax-rules ()
    [(wmdc #t k)
     (with ([cut-prompt fail-handler]) k)]
    [(wmdc #f k)
     k]))

;; (parse:variants x cx relsattrs variants splicing? pr es success cp0) : expr[Ans]
(define-syntax (parse:variants stx)
  (syntax-case stx ()
    [(parse:variants x cx relsattrs (variant ...) splicing? pr es success cp0 commit?)
     #'(try (parse:variant x cx relsattrs variant splicing? pr es success cp0 commit?) ...)]))

;; (parse:variant x cx relsattrs variant splicing? pr es success cp0) : expr[Ans]
(define-syntax (parse:variant stx)
  (syntax-case stx ()
    [(parse:variant x cx relsattrs variant #f pr es success cp0 commit?)
     (with-syntax ([#s(variant _ _ pattern (def ...)) #'variant])
       #`(let ()
           def ...
           (parse:S x cx pattern pr es
                    (variant-success relsattrs variant () success cp0 commit?))))]
    [(parse:variant x cx relsattrs variant #t pr es success cp0 commit?)
     (with-syntax ([#s(variant _ _ pattern (def ...)) #'variant])
       #`(let ()
           def ...
           (parse:H x cx rest-x rest-cx rest-pr pattern pr es
                    (variant-success relsattrs variant (rest-x rest-cx rest-pr)
                                     success cp0 commit?))))]))

;; (variant-success relsattrs variant (also:id ...) success bool) : expr[Ans]
(define-syntax (variant-success stx)
  (syntax-case stx ()
    [(variant-success relsattrs #s(variant _ _ pattern _) (also ...) success cp0 commit?)
     #`(with-maybe-reset-fail commit? cp0
         (base-success-expr #,(pattern-attrs (wash #'pattern))
                            relsattrs
                            (also ...)
                            success))]))

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
         #'(success fail-handler cut-prompt also ... (attribute name) ...)))]))

;; ----

;; (parse:clauses x clauses ctx)
(define-syntax (parse:clauses stx)
  (syntax-case stx ()
    [(parse:clauses x clauses ctx)
     (with-disappeared-uses
      (with-txlifts
       (lambda ()
        (define-values (chunks clauses-stx)
          (parse-keyword-options #'clauses parse-directive-table
                                 #:context #'ctx
                                 #:no-duplicates? #t))
        (define context
          (options-select-value chunks '#:context #:default #'x))
        (define-values (decls0 defs)
          (get-decls+defs chunks #t #:context #'ctx))
        (define (for-clause clause)
          (syntax-case clause ()
            [[p . rest]
             (let-values ([(rest pattern defs2)
                           (parse-pattern+sides #'p #'rest
                                                #:splicing? #f
                                                #:decls decls0
                                                #:context #'ctx)])
               (unless (and (stx-list? rest) (stx-pair? rest))
                 (raise-syntax-error #f
                                     "expected non-empty clause body"
                                     #'ctx
                                     clause))
               (with-syntax ([rest rest]
                             [pattern pattern]
                             [(local-def ...) (append defs defs2)])
                 #`(let ()
                     local-def ...
                     (parse:S x cx pattern pr es (let () . rest)))))]))
        (unless (and (stx-list? clauses-stx) (stx-pair? clauses-stx))
          (raise-syntax-error #f "expected non-empty sequence of clauses" #'ctx))
        (with-syntax ([(def ...) (append (get-txlifts-as-definitions) defs)]
                      [(alternative ...)
                       (map for-clause (stx->list clauses-stx))])
          #`(let* ([ctx0 #,context]
                   [pr (ps-empty x ctx0)]
                   [es null]
                   [cx x]
                   [fh0 (syntax-patterns-fail ctx0)])
              (with ([fail-handler fh0]
                     [cut-prompt fh0])
                (try alternative ...)))))))]))

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
       [#s(pat:var _attrs name #f _ () _ _)
        #'(let-attributes ([#s(attr name 0 #t) (datum->syntax cx x cx)])
            k)]
       [#s(pat:var _attrs name parser argu (nested-a ...) attr-count commit?)
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) (datum->syntax cx x cx)])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #'(app-argu parser x cx pr es fail-handler cut-prompt
                          (lambda (fh cp av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh]
                                       [cut-prompt cp])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs av ...)
                              (with ([fail-handler (lambda (fs) (values fs (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es fail-handler cut-prompt
                                            (lambda (fh cp av ...) (values #f av ...))
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
                (app-argu parser x cx pr es fail-handler cut-prompt
                          (lambda (fh cp . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                (with ([fail-handler fh]
                                       [cut-prompt cp])
                                  k))))
                          argu))))]
       [#s(pat:datum attrs datum)
        #`(let ([d (if (syntax? x) (syntax-e x) x)])
            (if (equal? d (quote datum))
                k
                (fail (failure pr (cons (cons (expect:atom 'datum) x) es)))))]
       [#s(pat:literal attrs literal input-phase lit-phase)
        #`(if (and (identifier? x)
                   (free-identifier=?/phases
                    x input-phase
                    (quote-syntax literal) lit-phase))
              k
              (fail (failure pr (cons (cons (expect:literal (quote-syntax literal)) x) es))))]
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
                   (lambda (fh cp id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh]
                              [cut-prompt cp])
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
       [#s(pat:pair _attrs head tail)
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
                (fail (failure pr es))))]
       [#s(pat:vector _attrs subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (vector? datum)
                (let ([datum (vector->list datum)]
                      [vcx (if (syntax? x) x cx)] ;; FIXME: (vector? datum) => (syntax? x) ???
                      [pr (ps-add-unvector pr)])
                  (parse:S datum vcx subpattern pr es k))
                (fail (failure pr es))))]
       [#s(pat:box _attrs subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (box? datum)
                (let ([datum (unbox datum)]
                      [bcx (if (syntax? x) x cx)] ;; FIXME: (box? datum) => (syntax? x) ???
                      [pr (ps-add-unbox pr)])
                  (parse:S datum bcx subpattern pr es k))
                (fail (failure pr es))))]
       [#s(pat:pstruct _attrs key subpattern)
        #`(let ([datum (if (syntax? x) (syntax-e x) x)])
            (if (let ([xkey (prefab-struct-key datum)])
                  (and xkey (equal? xkey 'key)))
                (let ([datum (struct->list datum)]
                      [scx (if (syntax? x) x cx)] ;; FIXME: (struct? datum) => (syntax? x) ???
                      [pr (ps-add-unpstruct pr)])
                  (parse:S datum scx subpattern pr es k))
                (fail (failure pr es))))]
       [#s(pat:describe attrs description transparent? pattern)
        #`(let ([es (cons (cons (expect:thing description transparent?) x) es)]
                [pr (if 'transparent? pr (ps-add-opaque pr))])
            (parse:S x cx pattern pr es k))]
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
        #`(let ([pr (ps-add-post pr)])
            (parse:S x cx pattern pr es k))]
       [#s(pat:integrated _attrs name argu predicate description)
        (with-syntax ([(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t) x])
                           #'())])
          ;; NOTE: predicate must not assume x (ie, this-syntax) is stx
          #'(if (app-argu predicate x argu)
                (let-attributes (name-attr ...) k)
                (let ([es (cons (cons (expect:thing 'description #t) x) es)])
                  (fail (failure pr es)))))])]))

;; (disjunct ???-pattern success (pre:expr ...) (id:id ...)) : expr[Ans]
(define-syntax (disjunct stx)
  (syntax-case stx ()
    [(disjunct pattern success (pre ...) (id ...))
     (with-syntax ([(#s(attr sub-id _ _) ...) (pattern-attrs (wash #'pattern))])
       (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
         #`(let ([alt-sub-id (attribute sub-id)] ...)
             (let ([id #f] ...)
               (let ([sub-id alt-sub-id] ...)
                 (success fail-handler cut-prompt pre ... id ...))))))]))

;; (disjunct/sides clauses success (pre:expr ...) (id:id ...)) : expr[Ans]
(define-syntax (disjunct/sides stx)
  (syntax-case stx ()
    [(disjunct/sides clauses success (pre ...) (id ...))
     (with-syntax ([(#s(clause:attr #s(attr sub-id _ _) _) ...) #'clauses])
       (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
         #`(let ([alt-sub-id (attribute sub-id)] ...)
             (let ([id #f] ...)
               (let ([sub-id alt-sub-id] ...)
                 (success fail-handler cut-prompt pre ... id ...))))))]))

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
                      [es* (cons (cons (expect:message message)
                                       (if (syntax? c) c x))
                                 es)])
                  (fail (failure pr* es*)))
                k))]
       [#s(action:parse _ pattern expr)
        #`(let* ([y (datum->syntax #f (wrap-user-code expr) #f)]
                 [cy y]
                 [pr* (ps-add-stx pr y)])
            (parse:S y cy pattern pr* es k))]
       [#s(action:do _ (stmt ...))
        #'(let () (no-shadow stmt) ... (#%expression k))]
       [#s(action:post _ pattern)
        #'(let ([pr (ps-add-post pr)])
            (parse:A x cx pattern pr es k))])]))

;; (bind/sides clauses k) : expr[Ans]
;; In k: attrs(clauses) are bound.
(define-syntax (bind/sides stx)
  (syntax-case stx ()
    [(_ (side ...) k)
     (for/fold ([k #'k]) ([side (in-list (reverse (syntax->list #'(side ...))))])
       (syntax-case side ()
         [#s(clause:attr a expr)
            #`(let-attributes ([a (wrap-user-code (check-list^depth a expr))])
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
     [#s(pat:pair attrs head-part tail-part)
      (with-syntax ([tail-part (convert-list-pattern #'tail-part end-pattern)])
        #'#s(pat:pair attrs head-part tail-part))])))

;; (parse:H x cx rest-x rest-cx rest-pr H-pattern pr es k)
;; In k: rest, rest-pr, attrs(H-pattern) are bound.
(define-syntax (parse:H stx)
  (syntax-case stx ()
    [(parse:H x cx rest-x rest-cx rest-pr head pr es k)
     (syntax-case #'head ()
       [#s(hpat:describe _ description transparent? pattern)
        #`(let ([es (cons (cons (expect:thing description transparent?) x) es)])
            (parse:H x cx rest-x rest-cx rest-pr pattern pr es k))]
       [#s(hpat:var _attrs name parser argu (nested-a ...) attr-count commit?)
        (with-syntax ([(av ...) (generate-n-temporaries (syntax-e #'attr-count))]
                      [(name-attr ...)
                       (if (identifier? #'name)
                           #'([#s(attr name 0 #t)
                               (stx-list-take x (ps-difference pr rest-pr))])
                           #'())])
          (if (not (syntax-e #'commit?))
              ;; The normal protocol
              #`(app-argu parser x cx pr es fail-handler cut-prompt
                          (lambda (fh cp rest-x rest-cx rest-pr av ...)
                            (let-attributes (name-attr ...)
                              (let-attributes* ((nested-a ...) (av ...))
                                (with ([fail-handler fh]
                                       [cut-prompt cp])
                                  k))))
                          argu)
              ;; The commit protocol
              ;; (Avoids putting k in procedure)
              #'(let-values ([(fs rest-x rest-cx rest-pr av ...)
                              (with ([fail-handler (lambda (fs) (values fs #f #f #f (let ([av #f]) av) ...))])
                                (with ([cut-prompt fail-handler])
                                  (app-argu parser x cx pr es fail-handler cut-prompt
                                            (lambda (fh cp rest-x rest-cx rest-pr av ...)
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
                (app-argu parser x cx pr es fail-handler cut-prompt
                          (lambda (fh cp rest-x rest-cx rest-pr . result)
                            (let-attributes (name-attr ...)
                              (let/unpack ((nested-a ...) result)
                                 (with ([fail-handler fh]
                                       [cut-prompt cp])
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
                   (lambda (fh cp rest-x rest-cx rest-pr id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh]
                              [cut-prompt cp])
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
       [#s(hpat:optional (a ...) pattern defaults)
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fh cp rest-x rest-cx rest-pr id ...)
                     (let-attributes ([a id] ...)
                       (with ([fail-handler fh]
                              [cut-prompt cp])
                         k)))])
              (try (parse:H x cx rest-x rest-cx rest-pr pattern pr es 
                            (success fail-handler cut-prompt
                                     rest-x rest-cx rest-pr (attribute id) ...))
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
       [_
        (with-syntax ([attrs (pattern-attrs (wash #'head))])
          #'(parse:S x cx
                     #s(pat:pair attrs head #s(internal-rest-pattern rest-x rest-cx rest-pr))
                     pr es k))])]))

;; (parse:dots x cx EH-pattern S-pattern pr es k) : expr[Ans]
;; In k: attrs(EH-pattern, S-pattern) are bound.
(define-syntax (parse:dots stx)
  (syntax-case stx ()
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
         (define-pattern-variable alt-map #'((id . alt-id) ...))
         (define-pattern-variable loop-k
           #'(dots-loop dx* dcx* loop-pr* fail-handler cut-prompt rel-rep ... alt-id ...))
         #`(let ()
             ;; dots-loop : stx progress rel-rep ... alt-id ... -> Ans
             (define (dots-loop dx dcx loop-pr fh cp rel-rep ... alt-id ...)
               (with ([fail-handler fh]
                      [cut-prompt cp])
                 (try-or-pair/null-check tail-pattern-is-null? dx dcx loop-pr es
                   (try (parse:EH dx dcx loop-pr head-repc dx* dcx* loop-pr* alt-map head-rep
                                  head es loop-k)
                        ...)
                   (cond [(< rel-rep (rep:min-number rel-repc))
                          (let ([es (cons (cons (expectation-of-reps/too-few rel-rep rel-repc) dx) es)])
                            (fail (failure loop-pr es)))]
                         ...
                         [else
                          (let-attributes ([a (rep:finalize a attr-repc alt-id)] ...)
                            (parse:S dx dcx tail loop-pr es k))]))))
             (let ([rel-rep 0] ...
                   [alt-id (rep:initial-value attr-repc)] ...)
               (dots-loop x cx pr fail-handler cut-prompt rel-rep ... alt-id ...)))))]))

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
       (define-pattern-variable k*
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
                            (let ([es (cons (cons (expectation-of-reps/too-many rep repc) x*) es)])
                              (fail (failure pr* es)))))]))]))

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

(define-syntax-rule (expectation-of-message message)
  (expect:message message))

(define-syntax expectation-of-reps/too-few
  (syntax-rules ()
    [(_ rep #s(rep:once name too-few-msg too-many-msg))
     (expect:message (or too-few-msg (name->too-few/once name)))]
    [(_ rep #s(rep:optional name too-many-msg _))
     (error 'syntax-parse "INTERNAL ERROR: impossible (too-few)")]
    [(_ rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (expect:message (or too-few-msg (name->too-few name)))]))

(define-syntax expectation-of-reps/too-many
  (syntax-rules ()
    [(_ rep #s(rep:once name too-few-msg too-many-msg))
     (expect:message (or too-many-msg (name->too-many name)))]
    [(_ rep #s(rep:optional name too-many-msg _))
     (expect:message (or too-many-msg (name->too-many name)))]
    [(_ rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (expect:message (or too-many-msg (name->too-many name)))]))

(define (name->too-few/once name)
  (and name (format "missing required occurrence of ~a" name)))

(define (name->too-few name)
  (and name (format "too few occurrences of ~a" name)))

(define (name->too-many name)
  (and name (format "too many occurrences of ~a" name)))
