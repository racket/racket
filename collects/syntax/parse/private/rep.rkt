#lang racket/base
(require (for-template racket/base
                       racket/stxparam
                       syntax/parse/private/keywords
                       syntax/parse/private/residual ;; keep abs. path
                       syntax/parse/private/runtime)
         racket/contract/base
         "minimatch.rkt"
         syntax/private/id-table
         syntax/stx
         syntax/keyword
         racket/syntax
         unstable/struct
         "txlift.rkt"
         "rep-attrs.rkt"
         "rep-data.rkt"
         "rep-patterns.rkt"
         syntax/parse/private/residual-ct ;; keep abs. path
         "kws.rkt")

;; Error reporting
;; All entry points should have explicit, mandatory #:context arg
;; (mandatory from outside, at least)

(provide/contract
 [parse-rhs
  (-> syntax? (or/c false/c (listof sattr?)) boolean?
      #:context (or/c false/c syntax?)
      rhs?)]
 [parse-pattern+sides
  (-> syntax? syntax?
      #:splicing? boolean?
      #:decls DeclEnv/c
      #:context syntax?
      any)]
 [parse*-ellipsis-head-pattern
  (-> syntax? DeclEnv/c boolean?
      #:context syntax?
      any)]
 [parse-directive-table any/c]
 [get-decls+defs
  (-> list? boolean? #:context (or/c false/c syntax?)
      (values DeclEnv/c (listof syntax?)))]
 [create-aux-def
  (-> DeclEntry/c
      (values DeclEntry/c (listof syntax?)))]
 [parse-argu
  (-> (listof syntax?)
      #:context syntax?
      arguments?)]
 [parse-kw-formals
  (-> syntax?
      #:context syntax?
      arity?)]
 [check-stxclass-header
  (-> syntax? syntax?
      (list/c identifier? syntax? arity?))]
 [check-stxclass-application
  (-> syntax? syntax?
      (cons/c identifier? arguments?))]
 [check-conventions-rules
  (-> syntax? syntax?
      (listof (list/c regexp? any/c)))]
 [check-attr-arity-list
  (-> syntax? syntax?
      (listof sattr?))])

;; ----

(define (atomic-datum? stx)
  (let ([datum (syntax-e stx)])
    (or (null? datum)
        (boolean? datum)
        (string? datum)
        (number? datum)
        (keyword? datum)
        (bytes? datum)
        (char? datum)
        (regexp? datum))))

(define (id-predicate kw)
  (lambda (stx)
    (and (identifier? stx)
         (free-identifier=? stx kw)
         (begin (disappeared! stx) #t))))

(define wildcard?  (id-predicate (quote-syntax _)))
(define epsilon?   (id-predicate (quote-syntax ||)))
(define dots?      (id-predicate (quote-syntax ...)))
(define plus-dots? (id-predicate (quote-syntax ...+)))

(define keywords
  (list (quote-syntax _)
        (quote-syntax ||)
        (quote-syntax ...)
        (quote-syntax ~var)
        (quote-syntax ~datum)
        (quote-syntax ~literal)
        (quote-syntax ~and)
        (quote-syntax ~or)
        (quote-syntax ~not)
        (quote-syntax ~seq)
        (quote-syntax ~rep)
        (quote-syntax ~once)
        (quote-syntax ~optional)
        (quote-syntax ~between)
        (quote-syntax ~rest)
        (quote-syntax ~describe)
        (quote-syntax ~!)
        (quote-syntax ~bind)
        (quote-syntax ~fail)
        (quote-syntax ~parse)
        (quote-syntax ~do)
        (quote-syntax ...+)
        (quote-syntax ~delimit-cut)
        (quote-syntax ~commit)
        (quote-syntax ~reflect)
        (quote-syntax ~splicing-reflect)
        (quote-syntax ~eh-var)
        (quote-syntax ~peek)
        (quote-syntax ~peek-not)))

(define (reserved? stx)
  (and (identifier? stx)
       (for/or ([kw (in-list keywords)])
         (free-identifier=? stx kw))))

(define (safe-name? stx)
  (and (identifier? stx)
       (not (regexp-match? #rx"^~" (symbol->string (syntax-e stx))))))

;; cut-allowed? : (paramter/c boolean?)
;; Used to detect ~cut within ~not pattern.
;; (Also #:no-delimit-cut stxclass within ~not)
(define cut-allowed? (make-parameter #t))

;; ---

(define (disappeared! x)
  (cond [(identifier? x)
         (record-disappeared-uses (list x))]
        [(and (stx-pair? x) (identifier? (stx-car x)))
         (record-disappeared-uses (list (stx-car x)))]
        [else
         (raise-type-error 'disappeared!
                           "identifier or syntax with leading identifier"
                           x)]))

;; ---

;; parse-rhs : stx boolean (or #f (listof SAttr)) stx -> RHS
;; If expected-attrs is true, then referenced stxclasses must be defined and
;; literals must be bound. Set to #f for pass1 (attr collection);
;; parser requires stxclasses to be bound.
(define (parse-rhs stx expected-attrs splicing? #:context ctx)
  (call/txlifts
   (lambda ()
     (parameterize ((current-syntax-context ctx))
       (define-values (rest description transp? attributes auto-nested? colon-notation?
                            decls defs options)
         (parse-rhs/part1 stx splicing? (and expected-attrs #t)))
       (define variants
         (parameterize ((stxclass-lookup-config
                         (cond [expected-attrs 'yes]
                               [auto-nested? 'try]
                               [else 'no]))
                        (stxclass-colon-notation? colon-notation?))
           (parse-variants rest decls splicing? expected-attrs)))
       (let ([sattrs
              (or attributes
                  (intersect-sattrss (map variant-attrs variants)))])
         (make rhs stx sattrs transp? description variants 
               (append (get-txlifts-as-definitions) defs)
               options #f))))))

(define (parse-rhs/part1 stx splicing? strict?)
  (define-values (chunks rest)
    (parse-keyword-options stx rhs-directive-table
                           #:context (current-syntax-context)
                           #:incompatible '((#:attributes #:auto-nested-attributes)
                                            (#:commit #:no-delimit-cut))
                           #:no-duplicates? #t))
  (define description (options-select-value chunks '#:description #:default #f))
  (define opaque? (and (assq '#:opaque chunks) #t))
  (define transparent? (not opaque?))
  (define auto-nested? (and (assq '#:auto-nested-attributes chunks) #t))
  (define colon-notation? (not (assq '#:disable-colon-notation chunks)))
  (define commit?
    (and (assq '#:commit chunks) #t))
  (define delimit-cut?
    (not (assq '#:no-delimit-cut chunks)))
  (define attributes (options-select-value chunks '#:attributes #:default #f))
  (define-values (decls defs) (get-decls+defs chunks strict?))
  (values rest description transparent? attributes auto-nested? colon-notation?
          decls defs (options commit? delimit-cut?)))

;; ----

(define (parse-variants rest decls splicing? expected-attrs)
  (define (gather-variants stx)
    (syntax-case stx (pattern)
      [((pattern . _) . rest)
       (begin (disappeared! (stx-car stx))
              (cons (parse-variant (stx-car stx) splicing? decls expected-attrs)
                    (gather-variants #'rest)))]
      [(bad-variant . rest)
       (wrong-syntax #'bad-variant "expected syntax-class variant")]
      [()
       null]))
  (gather-variants rest))

;; get-decls+defs : chunks boolean -> (values DeclEnv (listof syntax))
(define (get-decls+defs chunks strict?
                        #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (let*-values ([(decls defs1) (get-decls chunks strict?)]
                  [(decls defs2) (decls-create-defs decls)])
      (values decls (append defs1 defs2)))))

;; get-decls : chunks -> (values DeclEnv (listof syntax))
(define (get-decls chunks strict?)
  (define lits (options-select-value chunks '#:literals #:default null))
  (define litsets (options-select-value chunks '#:literal-sets #:default null))
  (define convs (options-select-value chunks '#:conventions #:default null))
  (define localconvs (options-select-value chunks '#:local-conventions #:default null))
  (define literals
    (append-lits+litsets lits litsets))
  (define-values (convs-rules convs-defs)
    (for/fold ([convs-rules null] [convs-defs null])
        ([conv-entry (in-list convs)])
      (let* ([c (car conv-entry)]
             [argu (cdr conv-entry)]
             [get-parser-id (conventions-get-procedures c)]
             [rules ((conventions-get-rules c))])
        (values (append rules convs-rules)
                (cons (make-conventions-def (map cadr rules) get-parser-id argu)
                      convs-defs)))))
  (define convention-rules (append localconvs convs-rules))
  (values (new-declenv literals #:conventions convention-rules)
          (reverse convs-defs)))

;; make-conventions-def : (listof den:delay) id Argument -> syntax
(define (make-conventions-def dens get-parsers-id argu)
  (with-syntax ([(parser ...) (map den:delayed-parser dens)]
                [get-parsers get-parsers-id]
                [argu argu])
    #'(define-values (parser ...)
        (apply values (app-argu get-parsers argu)))))

;; decls-create-defs : DeclEnv -> (values DeclEnv (listof stx))
(define (decls-create-defs decls0)
  (define (updater key value defs)
    (let-values ([(value newdefs) (create-aux-def value)])
      (values value (append newdefs defs))))
  (declenv-update/fold decls0 updater null))

;; create-aux-def : DeclEntry -> (values DeclEntry (listof stx))
;; FIXME: replace with txlift mechanism
(define (create-aux-def entry)
  (match entry
    [(den:lit _i _e _ip _lp)
     (values entry null)]
    [(den:magic-class name class argu role)
     (values entry null)]
    [(den:class name class argu)
     ;; FIXME: integrable syntax classes?
     (cond [(identifier? name)
            (let* ([pos-count (length (arguments-pargs argu))]
                   [kws (arguments-kws argu)]
                   [sc (get-stxclass/check-arity class class pos-count kws)])
              (with-syntax ([sc-parser (stxclass-parser sc)])
                (with-syntax ([parser (generate-temporary class)])
                  (values (make den:parser #'parser
                                (stxclass-attrs sc) (stxclass/h? sc)
                                (stxclass-commit? sc) (stxclass-delimit-cut? sc))
                          (list #`(define-values (parser)
                                    (curried-stxclass-parser #,class #,argu)))))))]
           [(regexp? name)
            ;; Conventions rule; delay class lookup until module/intdefs pass2
            ;; to allow forward references
            (with-syntax ([parser (generate-temporary class)]
                          [description (generate-temporary class)])
              (values (make den:delayed #'parser class)
                      (list #`(define-values (parser)
                                (curried-stxclass-parser #,class #,argu)))))])]
    [(den:parser _p _a _sp _c _dc?)
     (values entry null)]
    [(den:delayed _p _c)
     (values entry null)]))

(define (append-lits+litsets lits litsets)
  (define seen (make-bound-id-table lits))
  (for ([litset (in-list litsets)])
    (for ([lit (in-list litset)])
      (when (bound-id-table-ref seen (car lit) #f)
        (wrong-syntax (car lit) "duplicate literal declaration"))
      (bound-id-table-set! seen (car lit) #t)))
  (apply append lits litsets))

;; parse-variant : stx boolean DeclEnv #f/(listof Sattr) -> RHS
(define (parse-variant stx splicing? decls0 expected-attrs)
  (syntax-case stx (pattern)
    [(pattern p . rest)
     (let-values ([(rest pattern defs)
                   (parse-pattern+sides #'p #'rest
                                        #:splicing? splicing?
                                        #:decls decls0
                                        #:context stx)])
       (disappeared! stx)
       (unless (stx-null? rest)
         (wrong-syntax (if (pair? rest) (car rest) rest)
                       "unexpected terms after pattern directives"))
       (let* ([attrs (pattern-attrs pattern)]
              [sattrs (iattrs->sattrs attrs)])
         (when expected-attrs
           (parameterize ((current-syntax-context stx))
             ;; Called just for error-reporting
             (reorder-iattrs expected-attrs attrs)))
         (make variant stx sattrs pattern defs)))]))

;; parse-pattern+sides : stx stx <options> -> (values stx Pattern (listof stx))
;; Parses pattern, side clauses; desugars side clauses & merges with pattern
(define (parse-pattern+sides p-stx s-stx
                             #:splicing? splicing?
                             #:decls decls0
                             #:context ctx)
  (let-values ([(rest decls defs sides)
                (parse-pattern-directives s-stx
                                          #:allow-declare? #t
                                          #:decls decls0
                                          #:context ctx)])
    (let* ([pattern0 (parse-whole-pattern p-stx decls splicing? #:context ctx)]
           [pattern (combine-pattern+sides pattern0 sides splicing?)])
      (values rest pattern defs))))

(define (side-clauses-attrss clauses)
  (for/list ([c (in-list clauses)]
             #:when (or (clause:with? c) (clause:attr? c)))
    (if (clause:with? c)
        (pattern-attrs (clause:with-pattern c))
        (list (clause:attr-attr c)))))

;; parse-whole-pattern : stx DeclEnv boolean -> Pattern
(define (parse-whole-pattern stx decls [splicing? #f]
                             #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define pattern
      (if splicing?
          (parse-head-pattern stx decls)
          (parse-single-pattern stx decls)))
    (define pvars (map attr-name (pattern-attrs pattern)))
    (define excess-domain (declenv-domain-difference decls pvars))
    (when (pair? excess-domain)
      (wrong-syntax #f "declared pattern variables do not appear in pattern"
                    #:extra excess-domain))
    pattern))

;; combine-pattern+sides : Pattern (listof SideClause) -> Pattern
(define (combine-pattern+sides pattern sides splicing?)
  (define actions-pattern
    (create-action:and
     (for/list ([side (in-list sides)])
       (match side
         [(clause:fail condition message)
          (create-action:post
           (create-action:fail condition message))]
         [(clause:with wpat expr defs)
          (let ([ap (create-action:post
                     (create-action:parse wpat expr))])
            (if (pair? defs)
                (create-action:and (list (create-action:do defs) ap))
                ap))]
         [(clause:attr attr expr)
          (create-action:bind (list side))]
         [(clause:do stmts)
          (create-action:do stmts)]))))
  (define dummy-pattern
    (and (pair? sides)
         (create-pat:action actions-pattern (create-pat:any))))
  (if dummy-pattern
      (if splicing?
          (create-hpat:and pattern dummy-pattern)
          (create-pat:and (list pattern dummy-pattern)))
      pattern))

;; ----

;; parse-single-pattern : stx DeclEnv -> SinglePattern
(define (parse-single-pattern stx decls)
  (parse-*-pattern stx decls #f #f))

;; parse-head-pattern : stx DeclEnv -> HeadPattern
(define (parse-head-pattern stx decls)
  (parse-*-pattern stx decls #t #f))

;; parse-*-pattern : stx DeclEnv boolean boolean -> Pattern
(define (parse-*-pattern stx decls allow-head? allow-action?)
  (define (check-head! x)
    (unless allow-head?
      (wrong-syntax stx "head pattern not allowed here"))
    x)
  (define (check-action! x)
    ;; Coerce to S-pattern IF only S-patterns allowed
    (cond [allow-action? x]
          [(not allow-head?) (action-pattern->single-pattern x)]
          [else
           (wrong-syntax stx "action pattern not allowed here")]))
  (syntax-case stx (~var ~literal ~datum ~and ~or ~not ~rest ~describe
                    ~seq ~optional ~! ~bind ~fail ~parse ~do
                    ~post ~peek ~peek-not ~delimit-cut ~commit ~reflect
                    ~splicing-reflect)
    [wildcard
     (wildcard? #'wildcard)
     (begin (disappeared! stx)
            (create-pat:any))]
    [~!
     (disappeared! stx)
     (begin
       (unless (cut-allowed?)
         (wrong-syntax stx
                       "cut (~~!) not allowed within ~~not pattern"))
       (check-action!
        (create-action:cut)))]
    [reserved
     (reserved? #'reserved)
     (wrong-syntax stx "pattern keyword not allowed here")]
    [id
     (identifier? #'id)
     (parse-pat:id stx decls allow-head?)]
    [datum
     (atomic-datum? #'datum)
     (create-pat:datum (syntax->datum #'datum))]
    [(~var . rest)
     (disappeared! stx)
     (parse-pat:var stx decls allow-head?)]
    [(~datum . rest)
     (disappeared! stx)
     (syntax-case stx (~datum)
       [(~datum d)
        (create-pat:datum (syntax->datum #'d))]
       [_ (wrong-syntax stx "bad ~~datum form")])]
    [(~literal . rest)
     (disappeared! stx)
     (parse-pat:literal stx decls)]
    [(~and . rest)
     (disappeared! stx)
     (parse-pat:and stx decls allow-head? allow-action?)]
    [(~or . rest)
     (disappeared! stx)
     (parse-pat:or stx decls allow-head?)]
    [(~not . rest)
     (disappeared! stx)
     (parse-pat:not stx decls)]
    [(~rest . rest)
     (disappeared! stx)
     (parse-pat:rest stx decls)]
    [(~describe . rest)
     (disappeared! stx)
     (parse-pat:describe stx decls allow-head?)]
    [(~delimit-cut . rest)
     (disappeared! stx)
     (parse-pat:delimit stx decls allow-head?)]
    [(~commit . rest)
     (disappeared! stx)
     (parse-pat:commit stx decls allow-head?)]
    [(~reflect . rest)
     (disappeared! stx)
     (parse-pat:reflect stx decls #f)]
    [(~seq . rest)
     (disappeared! stx)
     (check-head!
      (parse-hpat:seq stx #'rest decls))]
    [(~optional . rest)
     (disappeared! stx)
     (check-head!
      (parse-hpat:optional stx decls))]
    [(~splicing-reflect . rest)
     (disappeared! stx)
     (check-head!
      (parse-pat:reflect stx decls #t))]
    [(~bind . rest)
     (disappeared! stx)
     (check-action!
      (parse-pat:bind stx decls))]
    [(~fail . rest)
     (disappeared! stx)
     (check-action!
      (parse-pat:fail stx decls))]
    [(~post . rest)
     (disappeared! stx)
     (parse-pat:post stx decls allow-head? allow-action?)]
    [(~peek . rest)
     (disappeared! stx)
     (check-head!
      (parse-pat:peek stx decls))]
    [(~peek-not . rest)
     (disappeared! stx)
     (check-head!
      (parse-pat:peek-not stx decls))]
    [(~parse . rest)
     (disappeared! stx)
     (check-action!
      (parse-pat:parse stx decls))]
    [(~do . rest)
     (disappeared! stx)
     (check-action!
      (parse-pat:do stx decls))]
    [(head dots . tail)
     (dots? #'dots)
     (begin (disappeared! #'dots)
            (parse-pat:dots stx #'head #'tail decls))]
    [(head plus-dots . tail)
     (plus-dots? #'plus-dots)
     (begin (disappeared! #'plus-dots)
            (parse-pat:plus-dots stx #'head #'tail decls))]
    [(head . tail)
     (let ([headp (parse-*-pattern #'head decls #t #t)]
           [tailp (parse-single-pattern #'tail decls)])
       (cond [(action-pattern? headp)
              (create-pat:action headp tailp)]
             [(head-pattern? headp)
              (create-pat:head headp tailp)]
             [else
              (create-pat:pair headp tailp)]))]
    [#(a ...)
     (let ([lp (parse-single-pattern (syntax/loc stx (a ...)) decls)])
       (create-pat:vector lp))]
    [b
     (box? (syntax-e #'b))
     (let ([bp (parse-single-pattern (unbox (syntax-e #'b)) decls)])
       (create-pat:box bp))]
    [s
     (and (struct? (syntax-e #'s)) (prefab-struct-key (syntax-e #'s)))
     (let* ([s (syntax-e #'s)]
            [key (prefab-struct-key s)]
            [contents (struct->list s)])
       (let ([lp (parse-single-pattern (datum->syntax #f contents #'s) decls)])
         (create-pat:pstruct key lp)))]))

;; parse-ellipsis-head-pattern : stx DeclEnv -> (listof EllipsisHeadPattern)
(define (parse-ellipsis-head-pattern stx decls)
  (for/list ([ehpat+hstx (in-list (parse*-ellipsis-head-pattern stx decls #t))])
    (car ehpat+hstx)))

;; parse*-ellipsis-head-pattern : stx DeclEnv bool
;;                             -> (listof (list EllipsisHeadPattern stx/eh-alternative))
(define (parse*-ellipsis-head-pattern stx decls allow-or?
                                      #:context [ctx (current-syntax-context)])
  (syntax-case stx (~eh-var ~or ~between ~optional ~once)
    [(~eh-var name eh-alt-set-id)
     (let ()
       (define prefix (name->prefix #'name "."))
       (define eh-alt-set (get-eh-alternative-set #'eh-alt-set-id))
       (for/list ([alt (in-list (eh-alternative-set-alts eh-alt-set))])
         (let* ([iattrs (id-pattern-attrs (eh-alternative-attrs alt) prefix)]
                [attr-count (length iattrs)])
           (list (make ehpat (repc-adjust-attrs iattrs (eh-alternative-repc alt))
                       (create-hpat:var #f (eh-alternative-parser alt) no-arguments iattrs attr-count #f #f)
                       (eh-alternative-repc alt))
                 (replace-eh-alternative-attrs
                  alt (iattrs->sattrs iattrs))))))]
    [(~or . _)
     allow-or?
     (begin
       (unless (stx-list? stx)
         (wrong-syntax stx "expected sequence of patterns"))
       (apply append
              (for/list ([sub (in-list (cdr (stx->list stx)))])
                (parse*-ellipsis-head-pattern sub decls allow-or?))))]
    [(~optional . _)
     (disappeared! stx)
     (list (parse*-ehpat/optional stx decls))]
    [(~once . _)
     (disappeared! stx)
     (list (parse*-ehpat/once stx decls))]
    [(~between . _)
     (disappeared! stx)
     (list (parse*-ehpat/bounds stx decls))]
    [_
     (let ([head (parse-head-pattern stx decls)])
       (list (list (make ehpat (map increase-depth (pattern-attrs head))
                         head
                         #f)
                   stx)))]))

(define (repc-adjust-attrs iattrs repc)
  (cond [(or (rep:once? repc) (rep:optional? repc))
         iattrs]
        [(or (rep:bounds? repc) (eq? #f repc))
         (map increase-depth iattrs)]
        [else
         (error 'repc-adjust-attrs "INTERNAL ERROR: unexpected: ~e" repc)]))

(define (replace-eh-alternative-attrs alt sattrs)
  (match alt
    [(eh-alternative repc _attrs parser)
     (eh-alternative repc sattrs parser)]))

;; ----

(define (check-no-delimit-cut-in-not id delimit-cut?)
  (unless (or delimit-cut? (cut-allowed?))
    (wrong-syntax id
                  (string-append "syntax class with #:no-delimit-cut option "
                                 "not allowed within ~~not pattern"))))

(define (parse-pat:id id decls allow-head?)
  (define entry (declenv-lookup decls id))
  (match entry
    [(den:lit internal literal input-phase lit-phase)
     (create-pat:literal literal input-phase lit-phase)]
    [(den:magic-class name class argu role)
     (let* ([pos-count (length (arguments-pargs argu))]
            [kws (arguments-kws argu)]
            [sc (get-stxclass/check-arity class class pos-count kws)])
       (parse-pat:var* id allow-head? id sc argu "." role #f))]
    [(den:class _n _c _a)
     (error 'parse-pat:id
            "(internal error) decls had leftover stxclass entry: ~s"
            entry)]
    [(den:parser parser attrs splicing? commit? delimit-cut?)
     (check-no-delimit-cut-in-not id delimit-cut?)
     (cond [splicing?
            (unless allow-head?
              (wrong-syntax id "splicing syntax class not allowed here"))
            (parse-pat:id/h id parser no-arguments attrs commit? "." #f)]
           [else
            (parse-pat:id/s id parser no-arguments attrs commit? "." #f)])]
    [(den:delayed parser class)
     (let ([sc (get-stxclass class)])
       (parse-pat:var* id allow-head? id sc no-arguments "." #f parser))]
    ['#f
     (unless (safe-name? id)
       (wrong-syntax id "expected identifier not starting with ~~ character"))
     (let-values ([(name sc) (split-id/get-stxclass id decls)])
       (if sc
           (parse-pat:var* id allow-head? name sc no-arguments "." #f #f)
           (create-pat:var name #f no-arguments null #f #t #f)))]))

(define (parse-pat:var stx decls allow-head?)
  (define name0
    (syntax-case stx (~var)
      [(~var name . _)
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected identifier"))
       #'name]
      [_
       (wrong-syntax stx "bad ~~var form")]))
  (define-values (scname sc+args-stx argu pfx role)
    (syntax-case stx (~var)
      [(~var _name)
       (values #f #f null #f #f)]
      [(~var _name sc/sc+args . rest)
       (let-values ([(sc argu)
                     (let ([p (check-stxclass-application #'sc/sc+args stx)])
                       (values (car p) (cdr p)))])
         (define chunks
           (parse-keyword-options/eol #'rest var-pattern-directive-table
                                      #:no-duplicates? #t
                                      #:context stx))
         (define sep
           (options-select-value chunks '#:attr-name-separator #:default #f))
         (define role (options-select-value chunks '#:role #:default #'#f))
         (values sc #'sc/sc+args argu (if sep (syntax-e sep) ".") role))]
      [_
       (wrong-syntax stx "bad ~~var form")]))
  (cond [(and (epsilon? name0) (not scname))
         (wrong-syntax name0 "illegal pattern variable name")]
        [(and (wildcard? name0) (not scname))
         (create-pat:any)]
        [scname
         (let ([sc (get-stxclass/check-arity scname sc+args-stx
                                             (length (arguments-pargs argu))
                                             (arguments-kws argu))])
           (parse-pat:var* stx allow-head? name0 sc argu pfx role #f))]
        [else ;; Just proper name
         (create-pat:var name0 #f (arguments null null null) null #f #t #f)]))

(define (parse-pat:var* stx allow-head? name sc argu pfx role parser*)
  ;; if parser* not #f, overrides sc parser
  (check-no-delimit-cut-in-not stx (stxclass-delimit-cut? sc))
  (cond [(and (stxclass/s? sc)
              (stxclass-integrate sc)
              (equal? argu no-arguments))
         (parse-pat:id/s/integrate name (stxclass-integrate sc) role)]
        [(stxclass/s? sc)
         (parse-pat:id/s name
                         (or parser* (stxclass-parser sc))
                         argu
                         (stxclass-attrs sc)
                         (stxclass-commit? sc)
                         pfx
                         role)]
        [(stxclass/h? sc)
         (unless allow-head?
           (wrong-syntax stx "splicing syntax class not allowed here"))
         (parse-pat:id/h name
                         (or parser* (stxclass-parser sc))
                         argu
                         (stxclass-attrs sc)
                         (stxclass-commit? sc)
                         pfx
                         role)]))

(define (parse-pat:id/s name parser argu attrs commit? pfx role)
  (define prefix (name->prefix name pfx))
  (define bind (name->bind name))
  (create-pat:var bind parser argu (id-pattern-attrs attrs prefix) (length attrs) commit? role))

(define (parse-pat:id/s/integrate name integrate role)
  (define bind (name->bind name))
  (let ([predicate (integrate-predicate integrate)]
        [description (integrate-description integrate)])
    (create-pat:integrated bind predicate description role)))

(define (parse-pat:id/h name parser argu attrs commit? pfx role)
  (define prefix (name->prefix name pfx))
  (define bind (name->bind name))
  (create-hpat:var bind parser argu (id-pattern-attrs attrs prefix) (length attrs) commit? role))

(define (name->prefix id pfx)
  (cond [(wildcard? id) #f]
        [(epsilon? id) id]
        [else (format-id id "~a~a" (syntax-e id) pfx)]))

(define (name->bind id)
  (cond [(wildcard? id) #f]
        [(epsilon? id) #f]
        [else id]))

;; id-pattern-attrs : (listof SAttr)IdPrefix -> (listof IAttr)
(define (id-pattern-attrs sattrs prefix)
  (if prefix
      (for/list ([a (in-list sattrs)])
        (prefix-attr a prefix))
      null))

;; prefix-attr : SAttr identifier -> IAttr
(define (prefix-attr a prefix)
  (make attr (prefix-attr-name prefix (attr-name a))
        (attr-depth a)
        (attr-syntax? a)))

;; prefix-attr-name : id symbol -> id
(define (prefix-attr-name prefix name)
  (format-id prefix "~a~a" (syntax-e prefix) name))

;; ----

(define (parse-pat:reflect stx decls splicing?)
  (syntax-case stx ()
    [(_ name (obj arg ...) . maybe-signature)
     (let ()
       (unless (identifier? #'var)
         (raise-syntax-error #f "expected identifier" stx #'name))
       (define attr-decls
         (syntax-case #'maybe-signature ()
           [(#:attributes attr-decls)
            (check-attr-arity-list #'attr-decls stx)]
           [() null]
           [_ (raise-syntax-error #f "bad syntax" stx)]))
       (define prefix (name->prefix #'name "."))
       (define bind (name->bind #'name))
       (define ctor (if splicing? create-hpat:reflect create-pat:reflect))
       (ctor #'obj (parse-argu (syntax->list #'(arg ...))) attr-decls bind
             (id-pattern-attrs attr-decls prefix)))]))

;; ---

(define (parse-pat:literal stx decls)
  (syntax-case stx (~literal)
    [(~literal lit . more)
     (unless (identifier? #'lit)
       (wrong-syntax #'lit "expected identifier"))
     (let* ([chunks (parse-keyword-options/eol #'more phase-directive-table
                                               #:no-duplicates? #t
                                               #:context stx)]
            [phase (options-select-value chunks '#:phase
                                         #:default #'(syntax-local-phase-level))])
       ;; FIXME: Duplicates phase expr!
       (create-pat:literal #'lit phase phase))]
    [_
     (wrong-syntax stx "bad ~~literal pattern")]))

(define (parse-pat:describe stx decls allow-head?)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options #'rest describe-option-table
                                          #:no-duplicates? #t
                                          #:context stx)])
       (define transparent? (not (assq '#:opaque chunks)))
       (define role (options-select-value chunks '#:role #:default #'#f))
       (syntax-case rest ()
         [(description pattern)
          (let ([p (parse-*-pattern #'pattern decls allow-head? #f)])
            (if (head-pattern? p)
                (create-hpat:describe p #'description transparent? role)
                (create-pat:describe p #'description transparent? role)))]))]))

(define (parse-pat:delimit stx decls allow-head?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parameterize ((cut-allowed? #t))
                (parse-*-pattern #'pattern decls allow-head? #f))])
       (if (head-pattern? p)
           (create-hpat:delimit p)
           (create-pat:delimit p)))]))

(define (parse-pat:commit stx decls allow-head?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parameterize ((cut-allowed? #t))
                (parse-*-pattern #'pattern decls allow-head? #f))])
       (if (head-pattern? p)
           (create-hpat:commit p)
           (create-pat:commit p)))]))

(define (split-prefix xs pred)
  (let loop ([xs xs] [rprefix null])
    (cond [(and (pair? xs) (pred (car xs)))
           (loop (cdr xs) (cons (car xs) rprefix))]
          [else
           (values (reverse rprefix) xs)])))

(define (parse-pat:and stx decls allow-head? allow-action?)
  ;; allow-action? = allowed to *return* pure action pattern;
  ;; all ~and patterns are allowed to *contain* action patterns
  (define patterns0 (parse-cdr-patterns stx decls allow-head? #t))
  (define-values (actions patterns) (split-prefix patterns0 action-pattern?))
  (cond [(null? patterns)
         (cond [allow-action?
                (create-action:and actions)]
               [allow-head?
                (wrong-syntax stx "expected at least one head pattern")]
               [else
                (wrong-syntax stx "expected at least one single-term pattern")])]
        [else
         (let ([p (parse-pat:and* stx patterns)])
           (if (head-pattern? p)
               (for/fold ([p p]) ([action (in-list (reverse actions))])
                 (create-hpat:action action p))
               (for/fold ([p p]) ([action (in-list (reverse actions))])
                 (create-pat:action action p))))]))

(define (parse-pat:and* stx patterns)
  ;; patterns is non-empty (empty case handled above)
  (cond [(null? (cdr patterns))
         (car patterns)]
        [(ormap head-pattern? patterns)
         ;; Check to make sure *all* are head patterns
         (for ([pattern (in-list patterns)]
               [pattern-stx (in-list (stx->list (stx-cdr stx)))])
           (unless (or (action-pattern? pattern) (head-pattern? pattern))
             (wrong-syntax
              pattern-stx
              "single-term pattern not allowed after head pattern")))
         (let ([p0 (car patterns)]
               [lps (map action/head-pattern->list-pattern (cdr patterns))])
           (create-hpat:and p0 (create-pat:and lps)))]
        [else
         (create-pat:and
          (for/list ([p (in-list patterns)])
            (if (action-pattern? p)
                (action-pattern->single-pattern p)
                p)))]))

(define (parse-pat:or stx decls allow-head?)
  (define patterns (parse-cdr-patterns stx decls allow-head? #f))
  (cond [(null? (cdr patterns))
         (car patterns)]
        [else
         (cond [(ormap head-pattern? patterns)
                (create-hpat:or patterns)]
               [else
                (create-pat:or patterns)])]))

(define (parse-pat:not stx decls)
  (syntax-case stx (~not)
    [(~not pattern)
     (let ([p (parameterize ((cut-allowed? #f))
                (parse-single-pattern #'pattern decls))])
       (create-pat:not p))]
    [_
     (wrong-syntax stx "expected a single subpattern")]))

(define (parse-hpat:seq stx list-stx decls)
  (define pattern (parse-single-pattern list-stx decls))
  (check-list-pattern pattern stx)
  (create-hpat:seq pattern))

(define (parse-cdr-patterns stx decls allow-head? allow-action?)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected sequence of patterns"))
  (let ([result
         (for/list ([sub (in-list (cdr (stx->list stx)))])
           (parse-*-pattern sub decls allow-head? allow-action?))])
    (when (null? result)
      (wrong-syntax stx "expected at least one pattern"))
    result))

(define (parse-pat:dots stx head tail decls)
  (define headps (parse-ellipsis-head-pattern head decls))
  (define tailp (parse-single-pattern tail decls))
  (unless (pair? headps)
    (wrong-syntax head "expected at least one pattern"))
  (create-pat:dots headps tailp))

(define (parse-pat:plus-dots stx head tail decls)
  (define headp (parse-head-pattern head decls))
  (define tailp (parse-single-pattern tail decls))
  (define head/rep
    (make-ehpat (map increase-depth (pattern-attrs headp))
                headp
                (make-rep:bounds 1 +inf.0 #f #f #f)))
  (create-pat:dots (list head/rep) tailp))

(define (parse-pat:bind stx decls)
  (syntax-case stx ()
    [(_ clause ...)
     (let ([clauses (check-bind-clause-list #'(clause ...) stx)])
       (make action:bind
         (append-iattrs (side-clauses-attrss clauses))
         clauses))]))

(define (parse-pat:fail stx decls)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options #'rest fail-directive-table
                                          #:context stx
                                          #:incompatible '((#:when #:unless))
                                          #:no-duplicates? #t)])
       (let ([condition
              (if (null? chunks)
                  #'#t
                  (let ([chunk (car chunks)])
                    (if (eq? (car chunk) '#:when)
                        (caddr chunk)
                        #`(not #,(caddr chunk)))))])
         (syntax-case rest ()
           [(message)
            (create-action:fail condition #'message)]
           [()
            (create-action:fail condition #''#f)]
           [_
            (wrong-syntax stx "bad ~~fail pattern")])))]))

(define (parse-pat:post stx decls allow-head? allow-action?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parse-*-pattern #'pattern decls allow-head? allow-action?)])
       (cond [(action-pattern? p)
              (cond [allow-action? (create-action:post p)]
                    [(not allow-head?) (create-pat:post (action-pattern->single-pattern p))]
                    [else (wrong-syntax stx "action pattern not allowed here")])]
             [(head-pattern? p)
              (cond [allow-head? (create-hpat:post p)]
                    [else (wrong-syntax stx "head pattern now allowed here")])]
             [else
              (create-pat:post p)]))]))

(define (parse-pat:peek stx decls)
  (syntax-case stx (~peek)
    [(~peek pattern)
     (let ([p (parse-head-pattern #'pattern decls)])
       (create-hpat:peek p))]))

(define (parse-pat:peek-not stx decls)
  (syntax-case stx (~peek-not)
    [(~peek-not pattern)
     (let ([p (parse-head-pattern #'pattern decls)])
       (create-hpat:peek-not p))]))

(define (parse-pat:parse stx decls)
  (syntax-case stx (~parse)
    [(~parse pattern expr)
     (let ([p (parse-single-pattern #'pattern decls)])
       (create-action:parse p #'expr))]
    [_
     (wrong-syntax stx "bad ~~parse pattern")]))

(define (parse-pat:do stx decls)
  (syntax-case stx (~do)
    [(~do stmt ...)
     (create-action:do (syntax->list #'(stmt ...)))]
    [_
     (wrong-syntax stx "bad ~~do pattern")]))

(define (parse-pat:rest stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (parse-single-pattern #'pattern decls)]))

(define (check-list-pattern pattern stx)
  (match pattern
    [(pat:datum _base '())
     #t]
    [(pat:head _base _head tail)
     (check-list-pattern tail stx)]
    [(pat:action _base _action tail)
     (check-list-pattern tail stx)]
    [(pat:dots _base _head tail)
     (check-list-pattern tail stx)]
    [(pat:pair _base _head tail)
     (check-list-pattern tail stx)]
    [_
     (wrong-syntax stx "expected proper list pattern")]))

(define (parse-hpat:optional stx decls)
  (define-values (head-stx head iattrs _name _tmm defaults)
    (parse*-optional-pattern stx decls h-optional-directive-table))
  (make hpat:optional iattrs head defaults))

;; parse*-optional-pattern : stx DeclEnv table
;;                        -> (values 
(define (parse*-optional-pattern stx decls optional-directive-table)
  (syntax-case stx (~optional)
    [(~optional p . options)
     (let* ([head (parse-head-pattern #'p decls)]
            [chunks
             (parse-keyword-options/eol #'options optional-directive-table
                                        #:no-duplicates? #t
                                        #:context stx)]
            [too-many-msg
             (options-select-value chunks '#:too-many #:default #'#f)]
            [name
             (options-select-value chunks '#:name #:default #'#f)]
            [defaults
              (options-select-value chunks '#:defaults #:default '())]
            [pattern-iattrs (pattern-attrs head)]
            [defaults-iattrs
             (append-iattrs (side-clauses-attrss defaults))]
            [all-iattrs
             (union-iattrs (list pattern-iattrs defaults-iattrs))])
       (check-iattrs-subset defaults-iattrs pattern-iattrs stx)
       (values #'p head all-iattrs name too-many-msg defaults))]))

;; -- EH patterns
;; Only parse the rep-constraint part; don't parse the head pattern within.
;; (To support eh-alternative-sets.)

;; parse*-ehpat/optional : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/optional stx decls)
  (define-values (head-stx head iattrs name too-many-msg defaults)
    (parse*-optional-pattern stx decls eh-optional-directive-table))
  (list (make ehpat iattrs
              head
              (make rep:optional name too-many-msg defaults))
        head-stx))

;; parse*-ehpat/once : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/once stx decls)
  (syntax-case stx (~once)
    [(~once p . options)
     (let* ([head (parse-head-pattern #'p decls)]
            [chunks
             (parse-keyword-options/eol #'options
                                        (list (list '#:too-few check-expression)
                                              (list '#:too-many check-expression)
                                              (list '#:name check-expression))
                                        #:context stx)]
            [too-few-msg
             (options-select-value chunks '#:too-few #:default #'#f)]
            [too-many-msg
             (options-select-value chunks '#:too-many #:default #'#f)]
            [name
             (options-select-value chunks '#:name #:default #'#f)])
       (list (make ehpat (pattern-attrs head)
                   head
                   (make rep:once name too-few-msg too-many-msg))
             #'p))]))

;; parse*-ehpat/bounds : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/bounds stx decls)
  (syntax-case stx (~between)
    [(~between p min max . options)
     (let ()
       (define head (parse-head-pattern #'p decls))
       (define minN (syntax-e #'min))
       (define maxN (syntax-e #'max))
       (unless (exact-nonnegative-integer? minN)
         (wrong-syntax #'min
                       "expected exact nonnegative integer"))
       (unless (or (exact-nonnegative-integer? maxN) (equal? maxN +inf.0))
         (wrong-syntax #'max
                       "expected exact nonnegative integer or +inf.0"))
       (when (> minN maxN)
         (wrong-syntax stx "minimum larger than maximum repetition constraint"))
       (let* ([chunks (parse-keyword-options/eol
                       #'options
                       (list (list '#:too-few check-expression)
                             (list '#:too-many check-expression)
                             (list '#:name check-expression))
                       #:context stx)]
              [too-few-msg
               (options-select-value chunks '#:too-few #:default #'#f)]
              [too-many-msg
               (options-select-value chunks '#:too-many #:default #'#f)]
              [name
               (options-select-value chunks '#:name #:default #'#f)])
         (list (make ehpat (map increase-depth (pattern-attrs head))
                     head
                     (make rep:bounds #'min #'max
                           name too-few-msg too-many-msg))
               #'p)))]))

;; -----

;; parse-pattern-directives : stxs(PatternDirective) <kw-args>
;;                         -> stx DeclEnv (listof stx) (listof SideClause)
(define (parse-pattern-directives stx
                                  #:allow-declare? allow-declare?
                                  #:decls decls
                                  #:context ctx)
  (parameterize ((current-syntax-context ctx))
    (define-values (chunks rest)
      (parse-keyword-options stx pattern-directive-table #:context ctx))
    (define-values (decls2 chunks2)
      (if allow-declare?
          (grab-decls chunks decls)
          (values decls chunks)))
    (define sides
      ;; NOTE: use *original* decls
      ;; because decls2 has #:declares for *above* pattern
      (parse-pattern-sides chunks2 decls))
    (define-values (decls3 defs)
      (decls-create-defs decls2))
    (values rest decls3 defs (parse-pattern-sides chunks2 decls))))

;; parse-pattern-sides : (listof chunk) DeclEnv
;;                    -> (listof SideClause/c)
;; Invariant: decls contains only literals bindings
(define (parse-pattern-sides chunks decls)
  (match chunks
    [(cons (list '#:declare declare-stx _ _) rest)
     (wrong-syntax declare-stx
                   "#:declare can only follow pattern or #:with clause")]
    [(cons (list '#:role role-stx _) rest)
     (wrong-syntax role-stx
                   "#:role can only follow immediately after #:declare clause")]
    [(cons (list '#:fail-when fw-stx when-condition expr) rest)
     (cons (make clause:fail when-condition expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:fail-unless fu-stx unless-condition expr) rest)
     (cons (make clause:fail #`(not #,unless-condition) expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:when w-stx unless-condition) rest)
     ;; Bleh: when is basically fail-unless without the msg argument
     (cons (make clause:fail #`(not #,unless-condition) #'#f)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:with with-stx pattern expr) rest)
     (let-values ([(decls2 rest) (grab-decls rest decls)])
       (let-values ([(decls2a defs) (decls-create-defs decls2)])
         (cons (make clause:with (parse-whole-pattern pattern decls2a) expr defs)
               (parse-pattern-sides rest decls))))]
    [(cons (list '#:attr attr-stx a expr) rest)
     (cons (make clause:attr a expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:do do-stx stmts) rest)
     (cons (make clause:do stmts)
           (parse-pattern-sides rest decls))]
    ['()
     '()]))

;; grab-decls : (listof chunk) DeclEnv
;;           -> (values DeclEnv (listof chunk))
(define (grab-decls chunks decls0)
  (define (add-decl stx role-stx decls)
    (let ([role
           (and role-stx
                (syntax-case role-stx ()
                  [(#:role role) #'role]))])
      (syntax-case stx ()
        [(#:declare name sc)
         (identifier? #'sc)
         (add-decl* decls #'name #'sc (parse-argu null) role)]
        [(#:declare name (sc expr ...))
         (identifier? #'sc)
         (add-decl* decls #'name #'sc (parse-argu (syntax->list #'(expr ...))) role)]
        [(#:declare name bad-sc)
         (wrong-syntax #'bad-sc
                       "expected syntax class name (possibly with parameters)")])))
  (define (add-decl* decls id sc-name argu role)
    (declenv-put-stxclass decls id sc-name argu role))
  (define (loop chunks decls)
    (match chunks
      [(cons (cons '#:declare decl-stx)
             (cons (cons '#:role role-stx) rest))
       (loop rest (add-decl decl-stx role-stx decls))]
      [(cons (cons '#:declare decl-stx) rest)
       (loop rest (add-decl decl-stx #f decls))]
      [_ (values decls chunks)]))
  (loop chunks decls0))


;; ----

;; Keyword Options & Checkers

;; check-attr-arity-list : stx stx -> (listof SAttr)
(define (check-attr-arity-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected list of attribute declarations" ctx stx))
  (let ([iattrs
         (for/list ([x (in-list (stx->list stx))])
           (check-attr-arity x ctx))])
    (iattrs->sattrs (append-iattrs (map list iattrs)))))

;; check-attr-arity : stx stx -> IAttr
(define (check-attr-arity stx ctx)
  (syntax-case stx ()
    [attr
     (identifier? #'attr)
     (make-attr #'attr 0 #f)]
    [(attr depth)
     (begin (unless (identifier? #'attr)
              (raise-syntax-error #f "expected attribute name" ctx #'attr))
            (unless (exact-nonnegative-integer? (syntax-e #'depth))
              (raise-syntax-error #f "expected depth (nonnegative integer)" ctx #'depth))
            (make-attr #'attr (syntax-e #'depth) #f))]
    [_
     (raise-syntax-error #f "expected attribute name with optional depth declaration" ctx stx)]))

;; check-literals-list : stx stx -> (listof (list id id ct-phase ct-phase))
;;  - txlifts defs of phase expressions
;;  - txlifts checks that literals are bound
(define (check-literals-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literals list" ctx stx))
  (let ([lits
         (for/list ([x (in-list (stx->list stx))])
           (check-literal-entry x ctx))])
    (let ([dup (check-duplicate-identifier (map car lits))])
      (when dup (raise-syntax-error #f "duplicate literal identifier" ctx dup)))
    lits))

;; check-literal-entry : stx stx -> (list id id ct-phase ct-phase)
(define (check-literal-entry stx ctx)
  (define (go internal external phase)
    (txlift #`(check-literal #,external #,phase #,ctx))
    (list internal external phase phase))
  (syntax-case stx ()
    [(internal external #:phase phase)
     (and (identifier? #'internal) (identifier? #'external))
     (go #'internal #'external (txlift #'phase))]
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (go #'internal #'external #'(syntax-local-phase-level))]
    [id
     (identifier? #'id)
     (go #'id #'id #'(syntax-local-phase-level))]
    [_
     (raise-syntax-error #f "expected literal entry"
                         ctx stx)]))

;; Literal sets - Import

;; check-literal-sets-list : stx stx -> (listof (listof (list id id ct-phase^2)))
(define (check-literal-sets-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literal-set list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-literal-set-entry x ctx)))

;; check-literal-set-entry : stx stx -> (listof (list id id ct-phase^2))
(define (check-literal-set-entry stx ctx)
  (define (elaborate litset-id lctx phase)
    (let ([litset (syntax-local-value/record litset-id literalset?)])
      (unless litset
        (raise-syntax-error #f "expected identifier defined as a literal-set"
                            ctx litset-id))
      (elaborate2 litset lctx phase)))
  (define (elaborate2 litset lctx phase)
    (for/list ([entry (in-list (literalset-literals litset))])
      (list (datum->syntax lctx (car entry) stx)
            (cadr entry)
            phase
            (caddr entry))))
  (syntax-case stx ()
    [(litset . more)
     (and (identifier? #'litset))
     (let* ([chunks (parse-keyword-options/eol #'more litset-directive-table
                                               #:no-duplicates? #t
                                               #:context ctx)]
            [lctx (options-select-value chunks '#:at #:default #'litset)]
            [phase (options-select-value chunks '#:phase
                                         #:default #'(syntax-local-phase-level))])
       (elaborate #'litset lctx (txlift phase)))]
    [litset
     (identifier? #'litset)
     (elaborate #'litset #'litset #'(syntax-local-phase-level))]
    [_
     (raise-syntax-error #f "expected literal-set entry" ctx stx)]))

;; Conventions

;; returns (listof (cons Conventions (listof syntax)))
(define (check-conventions-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected conventions list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-conventions x ctx)))

;; returns (cons Conventions (listof syntax))
(define (check-conventions stx ctx)
  (define (elaborate conventions-id argu)
    (let ([cs (syntax-local-value/record conventions-id conventions?)])
      (unless cs
        (raise-syntax-error #f "expected identifier defined as a conventions"
                            ctx conventions-id))
      (cons cs argu)))
  (syntax-case stx ()
    [(conventions arg ...)
     (identifier? #'conventions)
     (elaborate #'conventions (parse-argu (syntax->list #'(arg ...))))]
    [conventions
     (identifier? #'conventions)
     (elaborate #'conventions no-arguments)]
    [_
     (raise-syntax-error "expected conventions entry" ctx stx)]))

;; returns (listof (list regexp DeclEntry))
(define (check-conventions-rules stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected convention rule list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-conventions-rule x ctx)))

;; returns (list regexp DeclEntry)
(define (check-conventions-rule stx ctx)
  (define (check-conventions-pattern x blame)
    (cond [(symbol? x)
           (regexp (string-append "^" (regexp-quote (symbol->string x)) "$"))]
          [(regexp? x) x]
          [else
           (raise-syntax-error #f "expected identifier convention pattern"
                               ctx blame)]))
  (define (check-sc-expr x rx)
    (let ([x (check-stxclass-application x ctx)])
      (make den:class rx (car x) (cdr x))))
  (syntax-case stx ()
    [(rx sc)
     (let ([name-pattern (check-conventions-pattern (syntax-e #'rx) #'rx)])
       (list name-pattern (check-sc-expr #'sc name-pattern)))]))

(define (check-stxclass-header stx ctx)
  (syntax-case stx ()
    [name
     (identifier? #'name)
     (list #'name #'() no-arity)]
    [(name . formals)
     (identifier? #'name)
     (list #'name #'formals (parse-kw-formals #'formals #:context ctx))]
    [_ (raise-syntax-error #f "expected syntax class header" stx ctx)]))

(define (check-stxclass-application stx ctx)
  ;; Doesn't check "operator" is actually a stxclass
  (syntax-case stx ()
    [op
     (identifier? #'op)
     (cons #'op no-arguments)]
    [(op arg ...)
     (identifier? #'op)
     (cons #'op (parse-argu (syntax->list #'(arg ...))))]
    [_ (raise-syntax-error #f "expected syntax class use" ctx stx)]))

;; bind clauses
(define (check-bind-clause-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected sequence of bind clauses" ctx stx))
  (for/list ([clause (in-list (stx->list stx))])
    (check-bind-clause clause ctx)))

(define (check-bind-clause clause ctx)
  (syntax-case clause ()
    [(attr-decl expr)
     (make clause:attr (check-attr-arity #'attr-decl ctx) #'expr)]
    [_ (raise-syntax-error #f "expected bind clause" ctx clause)]))

(define (check-stmt-list stx ctx)
  (syntax-case stx ()
    [(e ...)
     (syntax->list #'(e ...))]
    [_
     (raise-syntax-error #f "expected list of expressions and definitions" ctx stx)]))
     
;; Arguments and Arities

;; parse-argu : (listof stx) -> Arguments
(define (parse-argu args #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define (loop args rpargs rkws rkwargs)
      (cond [(null? args)
             (arguments (reverse rpargs) (reverse rkws) (reverse rkwargs))]
            [(keyword? (syntax-e (car args)))
             (let ([kw (syntax-e (car args))]
                   [rest (cdr args)])
               (cond [(memq kw rkws)
                      (wrong-syntax (car args) "duplicate keyword")]
                     [(null? rest)
                      (wrong-syntax (car args)
                                    "missing argument expression after keyword")]
                     #| Overzealous, perhaps?
                     [(keyword? (syntax-e (car rest)))
                      (wrong-syntax (car rest) "expected expression following keyword")]
                     |#
                     [else
                      (loop (cdr rest) rpargs (cons kw rkws) (cons (car rest) rkwargs))]))]
            [else
             (loop (cdr args) (cons (car args) rpargs) rkws rkwargs)]))
    (loop args null null null)))

;; parse-kw-formals : stx -> Arity
(define (parse-kw-formals formals #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define id-h (make-bound-id-table))
    (define kw-h (make-hasheq)) ;; keyword => 'mandatory or 'optional
    (define pos 0)
    (define opts 0)
    (define (add-id! id)
      (when (bound-id-table-ref id-h id #f)
        (wrong-syntax id "duplicate formal parameter" ))
      (bound-id-table-set! id-h id #t))
    (define (loop formals)
      (cond [(and (stx-pair? formals) (keyword? (syntax-e (stx-car formals))))
             (let* ([kw-stx (stx-car formals)]
                    [kw (syntax-e kw-stx)]
                    [rest (stx-cdr formals)])
               (cond [(hash-ref kw-h kw #f)
                      (wrong-syntax kw-stx "duplicate keyword")]
                     [(stx-null? rest)
                      (wrong-syntax kw-stx "missing formal parameter after keyword")]
                     [else
                      (let-values ([(formal opt?) (parse-formal (stx-car rest))])
                        (add-id! formal)
                        (hash-set! kw-h kw (if opt? 'optional 'mandatory)))
                      (loop (stx-cdr rest))]))]
            [(stx-pair? formals)
             (let-values ([(formal opt?) (parse-formal (stx-car formals))])
               (when (and (positive? opts) (not opt?))
                 (wrong-syntax (stx-car formals)
                               "mandatory argument may not follow optional argument"))
               (add-id! formal)
               (set! pos (add1 pos))
               (when opt? (set! opts (add1 opts)))
               (loop (stx-cdr formals)))]
            [(identifier? formals)
             (add-id! formals)
             (finish #t)]
            [(stx-null? formals)
             (finish #f)]
            [else
             (wrong-syntax formals "bad argument sequence")]))
    (define (finish has-rest?)
      (arity (- pos opts)
             (if has-rest? +inf.0 pos)
             (sort (for/list ([(k v) (in-hash kw-h)]
                              #:when (eq? v 'mandatory))
                     k)
                   keyword<?)
             (sort (hash-map kw-h (lambda (k v) k))
                   keyword<?)))
    (loop formals)))

;; parse-formal : stx -> (values id bool)
(define (parse-formal formal)
  (syntax-case formal ()
    [param
     (identifier? #'param)
     (values #'param #f)]
    [(param default)
     (identifier? #'param)
     (values #'param #t)]
    [_
     (wrong-syntax formal
                   "expected formal parameter with optional default")]))


;; Directive tables

;; common-parse-directive-table
(define common-parse-directive-table
  (list (list '#:disable-colon-notation)
        (list '#:literals check-literals-list)
        (list '#:literal-sets check-literal-sets-list)
        (list '#:conventions check-conventions-list)
        (list '#:local-conventions check-conventions-rules)))

;; parse-directive-table
(define parse-directive-table
  (list* (list '#:context check-expression)
         common-parse-directive-table))

;; rhs-directive-table
(define rhs-directive-table
  (list* (list '#:description check-expression)
         (list '#:transparent)
         (list '#:opaque)
         (list '#:attributes check-attr-arity-list)
         (list '#:auto-nested-attributes)
         (list '#:commit)
         (list '#:no-delimit-cut)
         common-parse-directive-table))

;; pattern-directive-table
(define pattern-directive-table
  (list (list '#:declare check-identifier check-expression)
        (list '#:role check-expression) ;; attached to preceding #:declare
        (list '#:fail-when check-expression check-expression)
        (list '#:fail-unless check-expression check-expression)
        (list '#:when check-expression)
        (list '#:with check-expression check-expression)
        (list '#:attr check-attr-arity check-expression)
        (list '#:do check-stmt-list)))

;; fail-directive-table
(define fail-directive-table
  (list (list '#:when check-expression)
        (list '#:unless check-expression)))

;; describe-option-table
(define describe-option-table
  (list (list '#:opaque)
        (list '#:role check-expression)))

;; eh-optional-directive-table
(define eh-optional-directive-table
  (list (list '#:too-many check-expression)
        (list '#:name check-expression)
        (list '#:defaults check-bind-clause-list)))

;; h-optional-directive-table
(define h-optional-directive-table
  (list (list '#:defaults check-bind-clause-list)))

;; phase-directive-table
(define phase-directive-table
  (list (list '#:phase check-expression)))

;; litset-directive-table
(define litset-directive-table
  (cons (list '#:at (lambda (stx ctx) stx))
        phase-directive-table))

;; var-pattern-directive-table
(define var-pattern-directive-table
  (list (list '#:attr-name-separator check-stx-string)
        (list '#:role check-expression)))
