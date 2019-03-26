#lang racket/base
(require (for-template racket/base
                       syntax/parse/private/keywords
                       syntax/parse/private/residual ;; keep abs. path
                       syntax/parse/private/runtime)
         racket/list
         racket/contract/base
         "make.rkt"
         "minimatch.rkt"
         syntax/apply-transformer
         syntax/private/id-table
         syntax/stx
         syntax/keyword
         racket/syntax
         racket/struct
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
 [atomic-datum-stx?
  (-> syntax?
      boolean?)]
 [parse-rhs
  (->* [syntax? boolean? #:context (or/c false/c syntax?)]
       [#:default-description (or/c #f string?)]
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
  (-> list? #:context (or/c false/c syntax?)
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
 [check-datum-literals-list
  (-> syntax? syntax?
      (listof den:datum-lit?))]
 [check-attr-arity-list
  (-> syntax? syntax?
      (listof sattr?))]
 [stxclass-colon-notation?
  (parameter/c boolean?)]
 [fixup-rhs
  (-> rhs? boolean? (listof sattr?) rhs?)])

;; ----

(define (atomic-datum-stx? stx)
  (let ([datum (syntax-e stx)])
    (or (null? datum)
        (boolean? datum)
        (string? datum)
        (number? datum)
        (keyword? datum)
        (bytes? datum)
        (char? datum)
        (regexp? datum)
        (byte-regexp? datum))))

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
        (quote-syntax ~or*)
        (quote-syntax ~alt)
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
        (quote-syntax ~undo)
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

;; A LookupConfig is one of 'no, 'try, 'yes
;;  'no means don't lookup, always use dummy (no nested attrs)
;;  'try means lookup, but on failure use dummy (-> nested attrs only from prev.)
;;  'yes means lookup, raise error on failure

;; stxclass-lookup-config : parameterof LookupConfig
(define stxclass-lookup-config (make-parameter 'yes))

;; stxclass-colon-notation? : (parameterof boolean)
;;   if #t, then x:sc notation means (~var x sc)
;;   otherwise, just a var
(define stxclass-colon-notation? (make-parameter #t))


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

(define (propagate-disappeared! stx)
  (cond [(and (syntax? stx) (syntax-property stx 'disappeared-use))
         => (lambda (xs) (record-disappeared-uses (filter identifier? (flatten xs)) #f))]))

;; ---

;; parse-rhs : Syntax Boolean #:context Syntax #:default-description (U String #f) -> RHS
(define (parse-rhs stx splicing? #:context ctx #:default-description [default-description #f])
  (call/txlifts
   (lambda ()
     (parameterize ((current-syntax-context ctx))
       (define-values (rest description transp? attributes auto-nested? colon-notation?
                            decls defs commit? delimit-cut?)
         (parse-rhs/part1 stx splicing?))
       (define variants
         (parameterize ((stxclass-lookup-config (if auto-nested? 'try 'no))
                        (stxclass-colon-notation? colon-notation?))
           (parse-variants rest decls splicing?)))
       (define sattrs
         (or attributes
             (filter (lambda (a) (symbol-interned? (attr-name a)))
                     (intersect-sattrss (map variant-attrs variants)))))
       (make rhs sattrs transp? (or description #`(quote #,default-description)) variants
             (append (get-txlifts-as-definitions) defs)
             commit? delimit-cut?)))))

(define (parse-rhs/part1 stx splicing?)
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
  (define-values (decls defs) (get-decls+defs chunks))
  (values rest description transparent? attributes auto-nested? colon-notation?
          decls defs commit? delimit-cut?))

;; ----

(define (parse-variants rest decls splicing?)
  (define (gather-variants stx)
    (syntax-case stx (pattern)
      [((pattern . _) . rest)
       (begin (disappeared! (stx-car stx))
              (cons (parse-variant (stx-car stx) splicing? decls)
                    (gather-variants #'rest)))]
      [(bad-variant . rest)
       (wrong-syntax #'bad-variant "expected syntax-class variant")]
      [()
       null]))
  (gather-variants rest))

;; get-decls+defs : chunks boolean -> (values DeclEnv (listof syntax))
(define (get-decls+defs chunks #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (let*-values ([(decls defs1) (get-decls chunks)]
                  [(decls defs2) (decls-create-defs decls)])
      (values decls (append defs1 defs2)))))

;; get-decls : chunks -> (values DeclEnv (listof syntax))
(define (get-decls chunks)
  (define lits (options-select-value chunks '#:literals #:default null))
  (define datum-lits (options-select-value chunks '#:datum-literals #:default null))
  (define litsets (options-select-value chunks '#:literal-sets #:default null))
  (define convs (options-select-value chunks '#:conventions #:default null))
  (define localconvs (options-select-value chunks '#:local-conventions #:default null))
  (define literals
    (append/check-lits+litsets lits datum-lits litsets))
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
    [(? den:lit?)
     (values entry null)]
    [(? den:datum-lit?)
     (values entry null)]
    [(? den:magic-class?)
     (values entry null)]
    [(den:class name scname argu)
     (with-syntax ([parser (generate-temporary scname)])
       (values (make den:delayed #'parser scname)
               (list #`(define-values (parser) (curried-stxclass-parser #,scname #,argu)))))]
    [(? den:delayed?)
     (values entry null)]))

;; append/check-lits+litsets : .... -> (listof (U den:lit den:datum-lit))
(define (append/check-lits+litsets lits datum-lits litsets)
  (define seen (make-bound-id-table))
  (define (check-id id [blame-ctx id])
    (if (bound-id-table-ref seen id #f)
        (wrong-syntax blame-ctx "duplicate literal declaration: ~s" (syntax-e id))
        (bound-id-table-set! seen id #t))
    id)
  (let* ([litsets*
          (for/list ([entry (in-list litsets)])
            (let ([litset-id (first entry)]
                  [litset (second entry)]
                  [lctx (third entry)]
                  [input-phase (fourth entry)])
              (define (get/check-id sym)
                (check-id (datum->syntax lctx sym) litset-id))
              (for/list ([lse (in-list (literalset-literals litset))])
                (match lse
                  [(lse:lit internal external lit-phase)
                   (let ([internal (get/check-id internal)]
                         [external (syntax-property external 'literal (gensym))])
                     (make den:lit internal external input-phase lit-phase))]
                  [(lse:datum-lit internal external)
                   (let ([internal (get/check-id internal)])
                     (make den:datum-lit internal external))]))))]
         [lits*
          (for/list ([lit (in-list lits)])
            (check-id (den:lit-internal lit))
            lit)]
         [datum-lits*
          (for/list ([datum-lit (in-list datum-lits)])
            (check-id (den:datum-lit-internal datum-lit))
            datum-lit)])
    (apply append lits* datum-lits* litsets*)))

;; parse-variant : stx boolean DeclEnv -> RHS
(define (parse-variant stx splicing? decls0)
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
    (let* ([pattern0 (parse-whole-pattern p-stx decls splicing? #:context ctx #:kind 'main)]
           [pattern (combine-pattern+sides pattern0 sides splicing?)])
      (values rest pattern defs))))

;; parse-whole-pattern : stx DeclEnv boolean -> Pattern
;; kind is either 'main or 'with, indicates what kind of pattern declare affects
(define (parse-whole-pattern stx decls [splicing? #f]
                             #:kind kind
                             #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define pattern
      (if splicing?
          (parse-head-pattern stx decls)
          (parse-single-pattern stx decls)))
    (define pvars (map attr-name (pattern-attrs pattern)))
    (define excess-domain (declenv-domain-difference decls pvars))
    (when (pair? excess-domain)
      (wrong-syntax (car excess-domain)
                    (string-append
                     "identifier in #:declare clause does not appear in pattern"
                     (case kind
                       [(main) ""] ;; ";\n this #:declare clause affects only the main pattern"]
                       [(with) ";\n this #:declare clause affects only the preceding #:with pattern"]))))
    pattern))

;; combine-pattern+sides : Pattern (listof SideClause) -> Pattern
(define (combine-pattern+sides pattern sides splicing?)
  (check-pattern
   (cond [(pair? sides)
          (define actions-pattern
            (create-action:and (ord-and-patterns sides (gensym*))))
          (define and-patterns
            (ord-and-patterns (list pattern (pat:action actions-pattern (pat:any)))
                              (gensym*)))
          (cond [splicing? (apply hpat:and and-patterns)]
                [else (pat:and and-patterns)])]
         [else pattern])))

;; gensym* : -> UninternedSymbol
;; Like gensym, but with deterministic name from compilation-local counter.
(define gensym*-counter 0)
(define (gensym*)
  (set! gensym*-counter (add1 gensym*-counter))
  (string->uninterned-symbol (format "group~a" gensym*-counter)))

;; ----

;; parse-single-pattern : stx DeclEnv -> SinglePattern
(define (parse-single-pattern stx decls)
  (parse-*-pattern stx decls #f #f))

;; parse-head-pattern : stx DeclEnv -> HeadPattern
(define (parse-head-pattern stx decls)
  (parse-*-pattern stx decls #t #f))

;; parse-action-pattern : Stx DeclEnv -> ActionPattern
(define (parse-action-pattern stx decls)
  (define p (parse-*-pattern stx decls #f #t))
  (unless (action-pattern? p)
    (wrong-syntax stx "expected action pattern"))
  p)

(define ((make-not-shadowed? decls) id)
  ;; Returns #f if id is in literals/datum-literals list.
  ;; Conventions to not shadow pattern-form bindings, under the
  ;; theory that conventions only apply to things already determined
  ;; to be pattern variables.
  (not (declenv-lookup decls id)))
;; suitable as id=? argument to syntax-case*
(define ((make-not-shadowed-id=? decls) lit-id pat-id)
  (and (free-identifier=? lit-id pat-id)
       (not (declenv-lookup decls pat-id))))

;; parse-*-pattern : stx DeclEnv boolean boolean -> Pattern
(define (parse-*-pattern stx decls allow-head? allow-action?)
  (define (recur stx)
    (parse-*-pattern stx decls allow-head? allow-action?))
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
  (define not-shadowed? (make-not-shadowed? decls))
  (propagate-disappeared! stx)
  (check-pattern
  (syntax-case* stx (~var ~literal ~datum ~and ~or ~or* ~alt ~not ~rest ~describe
                     ~seq ~optional ~! ~bind ~fail ~parse ~do ~undo
                     ~post ~peek ~peek-not ~delimit-cut ~commit ~reflect
                     ~splicing-reflect)
                (make-not-shadowed-id=? decls)
    [id
     (and (identifier? #'id)
          (not-shadowed? #'id)
          (pattern-expander? (syntax-local-value #'id (λ () #f))))
     (begin (disappeared! #'id)
            (recur (expand-pattern (syntax-local-value #'id) stx)))]
    [(id . rst)
     (and (identifier? #'id)
          (not-shadowed? #'id)
          (pattern-expander? (syntax-local-value #'id (λ () #f))))
     (begin (disappeared! #'id)
            (recur (expand-pattern (syntax-local-value #'id) stx)))]
    [wildcard
     (and (wildcard? #'wildcard)
          (not-shadowed? #'wildcard))
     (begin (disappeared! stx)
            (pat:any))]
    [~!
     (disappeared! stx)
     (begin
       (unless (cut-allowed?)
         (wrong-syntax stx
                       "cut (~~!) not allowed within ~~not pattern"))
       (check-action!
        (action:cut)))]
    [reserved
     (and (reserved? #'reserved)
          (not-shadowed? #'reserved))
     (wrong-syntax stx "pattern keyword not allowed here")]
    [id
     (identifier? #'id)
     (parse-pat:id stx decls allow-head?)]
    [datum
     (atomic-datum-stx? #'datum)
     (pat:datum (syntax->datum #'datum))]
    [(~var . rest)
     (disappeared! stx)
     (parse-pat:var stx decls allow-head?)]
    [(~datum . rest)
     (disappeared! stx)
     (syntax-case stx (~datum)
       [(~datum d)
        (pat:datum (syntax->datum #'d))]
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
    [(~or* . rest)
     (disappeared! stx)
     (parse-pat:or stx decls allow-head?)]
    [(~alt . rest)
     (wrong-syntax stx "ellipsis-head pattern allowed only before ellipsis")]
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
    [(~undo . rest)
     (disappeared! stx)
     (check-action!
      (parse-pat:undo stx decls))]
    [(head dots . tail)
     (and (dots? #'dots) (not-shadowed? #'dots))
     (begin (disappeared! #'dots)
            (parse-pat:dots stx #'head #'tail decls))]
    [(head plus-dots . tail)
     (and (plus-dots? #'plus-dots) (not-shadowed? #'plus-dots))
     (begin (disappeared! #'plus-dots)
            (parse-pat:plus-dots stx #'head #'tail decls))]
    [(head . tail)
     (let ([headp (parse-*-pattern #'head decls #t #t)]
           [tailp (parse-single-pattern #'tail decls)])
       (cond [(action-pattern? headp)
              (pat:action headp tailp)]
             [(head-pattern? headp)
              (pat:head headp tailp)]
             [else (pat:pair headp tailp)]))]
    [#(a ...)
     (let ([lp (parse-single-pattern (syntax/loc stx (a ...)) decls)])
       (pat:vector lp))]
    [b
     (box? (syntax-e #'b))
     (let ([bp (parse-single-pattern (unbox (syntax-e #'b)) decls)])
       (pat:box bp))]
    [s
     (and (struct? (syntax-e #'s)) (prefab-struct-key (syntax-e #'s)))
     (let* ([s (syntax-e #'s)]
            [key (prefab-struct-key s)]
            [contents (struct->list s)])
       (let ([lp (parse-single-pattern (datum->syntax #f contents #'s) decls)])
         (pat:pstruct key lp)))])))

;; expand-pattern : pattern-expander Syntax -> Syntax
(define (expand-pattern pe stx)
  (let ([proc (pattern-expander-proc pe)])
    (local-apply-transformer proc stx 'expression)))

;; parse-ellipsis-head-pattern : stx DeclEnv -> (listof EllipsisHeadPattern)
(define (parse-ellipsis-head-pattern stx decls)
  (for/list ([ehpat+hstx (in-list (parse*-ellipsis-head-pattern stx decls #t))])
    (car ehpat+hstx)))

;; parse*-ellipsis-head-pattern : stx DeclEnv bool
;;                             -> (listof (list EllipsisHeadPattern stx/eh-alternative))
(define (parse*-ellipsis-head-pattern stx decls allow-or?
                                      #:context [ctx (current-syntax-context)])
  (define (recur stx) (parse*-ellipsis-head-pattern stx decls allow-or? #:context ctx))
  (define (recur-cdr-list stx)
    (unless (stx-list? stx) (wrong-syntax stx "expected sequence of patterns"))
    (apply append (map recur (cdr (stx->list stx)))))
  (define not-shadowed? (make-not-shadowed? decls))
  (propagate-disappeared! stx)
  (syntax-case* stx (~eh-var ~or ~alt ~between ~optional ~once)
                (make-not-shadowed-id=? decls)
    [id
     (and (identifier? #'id)
          (not-shadowed? #'id)
          (pattern-expander? (syntax-local-value #'id (lambda () #f))))
     (begin (disappeared! #'id)
            (recur (expand-pattern (syntax-local-value #'id) stx)))]
    [(id . rst)
     (and (identifier? #'id)
          (not-shadowed? #'id)
          (pattern-expander? (syntax-local-value #'id (lambda () #f))))
     (begin (disappeared! #'id)
            (recur (expand-pattern (syntax-local-value #'id) stx)))]
    [(~eh-var name eh-alt-set-id)
     (disappeared! stx)
     (let ()
       (define prefix (name->prefix #'name "."))
       (define eh-alt-set (get-eh-alternative-set #'eh-alt-set-id))
       (for/list ([alt (in-list (eh-alternative-set-alts eh-alt-set))])
         (let* ([iattrs (id-pattern-attrs (eh-alternative-attrs alt) prefix)]
                [attr-count (length iattrs)])
           (list (create-ehpat
                  (hpat:var/p #f (eh-alternative-parser alt) no-arguments iattrs #f
                              (scopts attr-count #f #t #f))
                  (eh-alternative-repc alt)
                  #f)
                 (replace-eh-alternative-attrs
                  alt (iattrs->sattrs iattrs))))))]
    [(~or . _)
     (disappeared! stx)
     (recur-cdr-list stx)]
    [(~alt . _)
     (disappeared! stx)
     (recur-cdr-list stx)]
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
       (list (list (create-ehpat head #f stx) stx)))]))

(define (replace-eh-alternative-attrs alt sattrs)
  (match alt
    [(eh-alternative repc _attrs parser)
     (eh-alternative repc sattrs parser)]))

;; ----------------------------------------
;; Identifiers, ~var, and stxclasses

(define (check-no-delimit-cut-in-not id delimit-cut?)
  (unless (or delimit-cut? (cut-allowed?))
    (wrong-syntax id
                  (string-append "syntax class with #:no-delimit-cut option "
                                 "not allowed within ~~not pattern"))))

(define (parse-pat:id id decls allow-head?)
  (cond [(declenv-lookup decls id)
         => (lambda (entry) (parse-pat:id/entry id allow-head? entry))]
        [(not (safe-name? id))
         (wrong-syntax id "expected identifier not starting with ~~ character")]
        [(and (stxclass-colon-notation?) (split-id id))
         => (match-lambda
              [(cons name suffix)
               (declenv-check-unbound decls name (syntax-e suffix) #:blame-declare? #t)
               (define entry (declenv-lookup decls suffix))
               (cond [(or (den:lit? entry) (den:datum-lit? entry))
                      (pat:and (list (pat:svar name) (parse-pat:id/entry id allow-head? entry)))]
                     [else (parse-stxclass-use id allow-head? name suffix no-arguments "." #f)])])]
        [(declenv-apply-conventions decls id)
         => (lambda (entry) (parse-pat:id/entry id allow-head? entry))]
        [else (pat:svar id)]))

(define (split-id id0)
  (cond [(regexp-match #rx"^([^:]*):(.+)$" (symbol->string (syntax-e id0)))
         => (lambda (m)
              (define src (syntax-source id0))
              (define ln (syntax-line id0))
              (define col (syntax-column id0))
              (define pos (syntax-position id0))
              (define span (syntax-span id0))
              (define id-str (cadr m))
              (define id-len (string-length id-str))
              (define suffix-str (caddr m))
              (define suffix-len (string-length suffix-str))
              (define id
                (datum->syntax id0 (string->symbol id-str)
                               (list src ln col pos id-len)
                               id0))
              (define suffix
                (datum->syntax id0 (string->symbol suffix-str)
                               (list src ln (and col (+ col id-len 1)) (and pos (+ pos id-len 1)) suffix-len)
                               id0))
              (cons id suffix))]
        [else #f]))

;; parse-pat:id/entry : Identifier .... DeclEntry -> SinglePattern
;; Handle when meaning of identifier pattern is given by declenv entry.
(define (parse-pat:id/entry id allow-head? entry)
  (match entry
    [(den:lit internal literal input-phase lit-phase)
     (pat:literal literal input-phase lit-phase)]
    [(den:datum-lit internal sym)
     (pat:datum sym)]
    [(den:magic-class name scname argu role)
     (parse-stxclass-use scname allow-head? id scname argu "." role)]
    [(den:class _n _c _a)
     (error 'parse-pat:id
            "(internal error) decls had leftover stxclass entry: ~s"
            entry)]
    [(den:delayed parser scname)
     (parse-stxclass-use id allow-head? id scname no-arguments "." #f parser)]))

(define (parse-pat:var stx decls allow-head?)
  (define name0
    (syntax-case stx ()
      [(_ name . _)
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected identifier"))
       #'name]
      [_
       (wrong-syntax stx "bad ~~var form")]))
  (define-values (scname sc+args-stx argu pfx role)
    (syntax-case stx ()
      [(_ _name)
       (values #f #f null #f #f)]
      [(_ _name sc/sc+args . rest)
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
         (pat:any)]
        [scname
         (parse-stxclass-use stx allow-head? name0 scname argu pfx role)]
        [else ;; Just proper name
         (pat:svar name0)]))

;; ----

(define (parse-stxclass-use stx allow-head? varname scname argu pfx role [parser* #f])
  (define config (stxclass-lookup-config))
  (cond [(and (memq config '(yes try)) (get-stxclass scname (eq? config 'try)))
         => (lambda (sc)
              (unless parser*
                (check-stxclass-arity sc stx (length (arguments-pargs argu)) (arguments-kws argu)))
              (parse-stxclass-use* stx allow-head? varname sc argu pfx role parser*))]
        [else
         (define bind (name->bind varname))
         (pat:fixup stx bind varname scname argu pfx role parser*)]))

;; ----

(define (parse-stxclass-use* stx allow-head? name sc argu pfx role parser*)
  ;; if parser* not #f, overrides sc parser
  (check-no-delimit-cut-in-not stx (scopts-delimit-cut? (stxclass-opts sc)))
  (define bind (name->bind name))
  (define prefix (name->prefix name pfx))
  (define parser (or parser* (stxclass-parser sc)))
  (define nested-attrs (id-pattern-attrs (stxclass-attrs sc) prefix))
  (define opts (stxclass-opts sc))
  (cond [(and (stxclass/s? sc) (stxclass-inline sc) (equal? argu no-arguments))
         (pat:integrated bind (stxclass-inline sc) (scopts-desc opts) role)]
        [(stxclass/s? sc)
         (pat:var/p bind parser argu nested-attrs role opts)]
        [(stxclass/h? sc)
         (unless allow-head?
           (wrong-syntax stx "splicing syntax class not allowed here"))
         (hpat:var/p bind parser argu nested-attrs role opts)]))

(define (name->prefix id pfx)
  (cond [(wildcard? id) #f]
        [(epsilon? id) id]
        [else (format-id id "~a~a" (syntax-e id) pfx #:source id)]))

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
  (orig (format-id prefix "~a~a" (syntax-e prefix) name #:source prefix)))

(define (orig stx)
  (syntax-property stx 'original-for-check-syntax #t))

;; ----------------------------------------
;; Other pattern forms

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
       (define ctor (if splicing? hpat:reflect pat:reflect))
       (ctor #'obj (parse-argu (syntax->list #'(arg ...))) attr-decls bind
             (id-pattern-attrs attr-decls prefix)))]))

(define (parse-pat:literal stx decls)
  (syntax-case stx ()
    [(_ lit . more)
     (unless (identifier? #'lit)
       (wrong-syntax #'lit "expected identifier"))
     (let* ([chunks (parse-keyword-options/eol #'more phase-directive-table
                                               #:no-duplicates? #t
                                               #:context stx)]
            [phase (options-select-value chunks '#:phase
                                         #:default #'(syntax-local-phase-level))])
       ;; FIXME: Duplicates phase expr!
       (pat:literal #'lit phase phase))]
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
                (hpat:describe p #'description transparent? role)
                (pat:describe p #'description transparent? role)))]))]))

(define (parse-pat:delimit stx decls allow-head?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parameterize ((cut-allowed? #t))
                (parse-*-pattern #'pattern decls allow-head? #f))])
       (if (head-pattern? p)
           (hpat:delimit p)
           (pat:delimit p)))]))

(define (parse-pat:commit stx decls allow-head?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parameterize ((cut-allowed? #t))
                (parse-*-pattern #'pattern decls allow-head? #f))])
       (if (head-pattern? p)
           (hpat:commit p)
           (pat:commit p)))]))

(define (parse-pat:and stx decls allow-head? allow-action?)
  ;; allow-action? = allowed to *return* pure action pattern;
  ;; all ~and patterns are allowed to *contain* action patterns
  (define patterns0 (parse-cdr-patterns stx decls allow-head? #t))
  (cond [(andmap action-pattern? patterns0)
         (cond [allow-action?
                (define patterns1 (ord-and-patterns patterns0 (gensym*)))
                (action:and patterns1)]
               [allow-head?
                (wrong-syntax stx "expected at least one head or single-term pattern")]
               [else
                (wrong-syntax stx "expected at least one single-term pattern")])]
        [(memq (stxclass-lookup-config) '(no try))
         (pat:and/fixup stx patterns0)]
        [else (parse-pat:and/k stx patterns0)]))

(define (parse-pat:and/k stx patterns0)
  ;; PRE: patterns0 not all action patterns
  (define patterns1 (ord-and-patterns patterns0 (gensym*)))
  (define-values (actions patterns) (split-prefix patterns1 action-pattern?))
  (add-actions actions (parse-pat:and/k* stx (length actions) patterns)))

(define (parse-pat:and/k* stx actions-len patterns)
  ;; PRE: patterns non-empty, starts with non-action pattern
  (cond [(null? (cdr patterns))
         (car patterns)]
        [(ormap head-pattern? patterns)
         ;; Check to make sure *all* are head patterns
         (for ([pattern (in-list patterns)]
               [pattern-stx (in-list (drop (stx->list (stx-cdr stx)) actions-len))])
           (unless (or (action-pattern? pattern) (head-pattern? pattern))
             (wrong-syntax
              pattern-stx
              "single-term pattern not allowed after head pattern")))
         (let ([p0 (car patterns)]
               [lps (map action/head-pattern->list-pattern (cdr patterns))])
           (hpat:and p0 (pat:and lps)))]
        [else
         (pat:and
          (for/list ([p (in-list patterns)])
            (if (action-pattern? p)
                (action-pattern->single-pattern p)
                p)))]))

(define (split-prefix xs pred)
  (let loop ([xs xs] [rprefix null])
    (cond [(and (pair? xs) (pred (car xs)))
           (loop (cdr xs) (cons (car xs) rprefix))]
          [else
           (values (reverse rprefix) xs)])))

(define (add-actions actions p)
  (if (head-pattern? p)
      (for/fold ([p p]) ([action (in-list (reverse actions))])
        (hpat:action action p))
      (for/fold ([p p]) ([action (in-list (reverse actions))])
        (pat:action action p))))

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
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parameterize ((cut-allowed? #f))
                (parse-single-pattern #'pattern decls))])
       (pat:not p))]
    [_
     (wrong-syntax stx "expected a single subpattern")]))

(define (parse-hpat:seq stx list-stx decls)
  (define pattern (parse-single-pattern list-stx decls))
  (unless (proper-list-pattern? pattern)
    (wrong-syntax stx "expected proper list pattern"))
  (hpat:seq pattern))

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
  (pat:dots headps tailp))

(define (parse-pat:plus-dots stx head tail decls)
  (define headp (parse-head-pattern head decls))
  (define tailp (parse-single-pattern tail decls))
  (define head/rep (create-ehpat headp (make-rep:bounds 1 +inf.0 #f #f #f) head))
  (pat:dots (list head/rep) tailp))

(define (parse-pat:bind stx decls)
  (syntax-case stx ()
    [(_ clause ...)
     (let ([clauses (check-bind-clause-list #'(clause ...) stx)])
       (create-action:and clauses))]))

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
            (action:fail condition #'message)]
           [()
            (action:fail condition #''#f)]
           [_
            (wrong-syntax stx "bad ~~fail pattern")])))]))

(define (parse-pat:post stx decls allow-head? allow-action?)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parse-*-pattern #'pattern decls allow-head? allow-action?)])
       (cond [(action-pattern? p)
              (cond [allow-action? (action:post p)]
                    [(not allow-head?) (pat:post (action-pattern->single-pattern p))]
                    [else (wrong-syntax stx "action pattern not allowed here")])]
             [(head-pattern? p)
              (cond [allow-head? (hpat:post p)]
                    [else (wrong-syntax stx "head pattern not allowed here")])]
             [else (pat:post p)]))]))

(define (parse-pat:peek stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parse-head-pattern #'pattern decls)])
       (hpat:peek p))]))

(define (parse-pat:peek-not stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (let ([p (parse-head-pattern #'pattern decls)])
       (hpat:peek-not p))]))

(define (parse-pat:parse stx decls)
  (syntax-case stx ()
    [(_ pattern expr)
     (let ([p (parse-single-pattern #'pattern decls)])
       (action:parse p #'expr))]
    [_
     (wrong-syntax stx "bad ~~parse pattern")]))

(define (parse-pat:do stx decls)
  (syntax-case stx ()
    [(_ stmt ...)
     (action:do (syntax->list #'(stmt ...)))]
    [_
     (wrong-syntax stx "bad ~~do pattern")]))

(define (parse-pat:undo stx decls)
  (syntax-case stx ()
    [(_ stmt ...)
     (action:undo (syntax->list #'(stmt ...)))]
    [_
     (wrong-syntax stx "bad ~~undo pattern")]))

(define (parse-pat:rest stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (parse-single-pattern #'pattern decls)]))

(define (parse-hpat:optional stx decls)
  (define-values (head-stx head iattrs _name _tmm defaults)
    (parse*-optional-pattern stx decls h-optional-directive-table))
  (create-hpat:or
   (list head
         (hpat:action (create-action:and defaults)
                      (hpat:seq (pat:datum '()))))))

;; parse*-optional-pattern : stx DeclEnv table
;;                        -> (values Syntax HeadPattern IAttrs Stx Stx (Listof BindClause))
(define (parse*-optional-pattern stx decls optional-directive-table)
  (syntax-case stx ()
    [(_ p . options)
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
             (append-iattrs (map pattern-attrs defaults))]
            [all-iattrs
             (union-iattrs (list pattern-iattrs defaults-iattrs))])
       (when (eq? (stxclass-lookup-config) 'yes)
         ;; Only check that attrs in defaults clause agree with attrs
         ;; in pattern when attrs in pattern are known to be complete.
         (check-iattrs-subset defaults-iattrs pattern-iattrs stx))
       (values #'p head all-iattrs name too-many-msg defaults))]))

;; -- EH patterns
;; Only parse the rep-constraint part; don't parse the head pattern within.
;; (To support eh-alternative-sets.)

;; parse*-ehpat/optional : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/optional stx decls)
  (define-values (head-stx head iattrs name too-many-msg defaults)
    (parse*-optional-pattern stx decls eh-optional-directive-table))
  (list (create-ehpat head (make rep:optional name too-many-msg defaults) head-stx)
        head-stx))

;; parse*-ehpat/once : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/once stx decls)
  (syntax-case stx ()
    [(_ p . options)
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
       (list (create-ehpat head (make rep:once name too-few-msg too-many-msg) #'p)
             #'p))]))

;; parse*-ehpat/bounds : stx DeclEnv -> (list EllipsisHeadPattern stx)
(define (parse*-ehpat/bounds stx decls)
  (syntax-case stx ()
    [(_ p min max . options)
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
         (list (create-ehpat head
                             (make rep:bounds #'min #'max
                                   name too-few-msg too-many-msg)
                             #'p)
               #'p)))]))


;; ============================================================

(define (fixup-rhs the-rhs allow-head? expected-attrs)
  (match the-rhs
    [(rhs attrs tr? desc vs defs commit? delimit-cut?)
     (define vs* (for/list ([v (in-list vs)]) (fixup-variant v allow-head? expected-attrs)))
     (rhs attrs tr? desc vs* defs commit? delimit-cut?)]))

(define (fixup-variant v allow-head? expected-attrs)
  (match v
    [(variant stx sattrs p defs)
     (parameterize ((current-syntax-context stx))
       (define p*
         (parameterize ((stxclass-lookup-config 'yes))
           (fixup-pattern p allow-head?)))
       ;; (eprintf "~v\n===>\n~v\n\n" p p*)
       ;; Called just for error-reporting
       (reorder-iattrs expected-attrs (pattern-attrs p*))
       (variant stx sattrs p* defs))]))

(define (fixup-pattern p0 allow-head?)
  (define (S p) (fixup p #f))
  (define (S* p) (fixup p #t))
  (define (A/S* p) (if (action-pattern? p) (A p) (S* p)))

  (define (A p)
    (match p
      ;; [(action:cut)
      ;;  (action:cut)]
      ;; [(action:fail when msg)
      ;;  (action:fail when msg)]
      ;; [(action:bind attr expr)
      ;;  (action:bind attr expr)]
      [(action:and ps)
       (action:and (map A ps))]
      [(action:parse sp expr)
       (action:parse (S sp) expr)]
      ;; [(action:do stmts)
      ;;  (action:do stmts)]
      ;; [(action:undo stmts)
      ;;  (action:undo stmts)]
      [(action:ord sp group index)
       (create-ord-pattern (A sp) group index)]
      [(action:post sp)
       (create-post-pattern (A sp))]
      ;; ----
      ;; Default: no sub-patterns, just return
      [p p]))
  (define (EH p)
    (match p
      [(ehpat iattrs hp repc check-null?)
       (create-ehpat (H hp) repc #f)]))

  (define (fixup p allow-head?)
    (define (I p) (fixup p allow-head?))
    (match p
      [(pat:fixup stx bind varname scname argu pfx role parser*)
       (parse-stxclass-use stx allow-head? varname scname argu pfx role parser*)]
      ;; ----
      ;; [(pat:any)
      ;;  (pat:any)]
      ;; [(pat:svar name)
      ;;  (pat:svar name)]
      ;; [(pat:var/p name parser argu nested-attrs role opts)
      ;;  (pat:var/p name parser argu nested-attrs role opts)]
      ;; [(pat:integrated name predicate desc role)
      ;;  (pat:integrated name predicate desc role)]
      ;; [(pat:reflect obj argu attr-decls name nested-attrs)
      ;;  (pat:reflect obj argu attr-decls name nested-attrs)]
      ;; [(pat:datum d)
      ;;  (pat:datum d)]
      ;; [(pat:literal id input-phase lit-phase)
      ;;  (pat:literal id input-phase lit-phase)]
      [(pat:vector sp)
       (pat:vector (S sp))]
      [(pat:box sp)
       (pat:box (S sp))]
      [(pat:pstruct key sp)
       (pat:pstruct key (S sp))]
      [(pat:not sp)
       (parameterize ((cut-allowed? #f))
         (pat:not (S sp)))]
      [(pat:dots headps tailp)
       (pat:dots (map EH headps) (S tailp))]
      [(pat:head headp tailp)
       (pat:head (H headp) (S tailp))]
      ;; --- The following patterns may change if a subpattern switches to head pattern ----
      [(pat:pair headp tailp)
       (let ([headp (S* headp)] [tailp (S tailp)])
         (if (head-pattern? headp) (pat:head headp tailp) (pat:pair headp tailp)))]
      [(pat:action a sp)
       (let ([a (A a)] [sp (I sp)])
         (if (head-pattern? sp) (hpat:action a sp) (pat:action a sp)))]
      [(pat:describe sp desc tr? role)
       (let ([sp (I sp)])
         (if (head-pattern? sp) (hpat:describe sp desc tr? role) (pat:describe sp desc tr? role)))]
      [(pat:and ps)
       (let ([ps (map I ps)])
         (pat:and ps))]
      [(pat:and/fixup stx ps)
       (let ([ps (for/list ([p (in-list ps)])
                   (cond [(action-pattern? p) (A p)]
                         [allow-head? (H p)]
                         [else (I p)]))])
         (parse-pat:and/k stx ps))]
      [(pat:or _ ps _)
       (let ([ps (map I ps)])
         (if (ormap head-pattern? ps) (create-hpat:or ps) (create-pat:or ps)))]
      [(pat:delimit sp)
       (let ([sp (parameterize ((cut-allowed? #t)) (I sp))])
         (if (head-pattern? sp) (hpat:delimit sp) (pat:delimit sp)))]
      [(pat:commit sp)
       (let ([sp (parameterize ((cut-allowed? #t)) (I sp))])
         (if (head-pattern? sp) (hpat:commit sp) (pat:commit sp)))]
      [(pat:ord sp group index)
       (create-ord-pattern (I sp) group index)]
      [(pat:post sp)
       (create-post-pattern (I sp))]
      ;; ----
      ;; Default: no sub-patterns, just return
      [p p]))

  (define (H p)
    (match p
      ;; [(hpat:var/p name parser argu nested-attrs role scopts)
      ;;  (hpat:var/p name parser argu nested-attrs role scopts)]
      ;; [(hpat:reflect obj argu attr-decls name nested-attrs)
      ;;  (hpat:reflect obj argu attr-decls name nested-attrs)]
      [(hpat:seq lp)
       (hpat:seq (S lp))]
      [(hpat:action a hp)
       (hpat:action (A a) (H hp))]
      [(hpat:describe hp desc tr? role)
       (hpat:describe (H hp) desc tr? role)]
      [(hpat:and hp sp)
       (hpat:and (H hp) (S sp))]
      [(hpat:or _ ps _)
       (create-hpat:or (map H ps))]
      [(hpat:delimit hp)
       (parameterize ((cut-allowed? #t))
         (hpat:delimit (H hp)))]
      [(hpat:commit hp)
       (parameterize ((cut-allowed? #t))
         (hpat:commit (H hp)))]
      [(hpat:ord hp group index)
       (create-ord-pattern (H hp) group index)]
      [(hpat:post hp)
       (create-post-pattern (H hp))]
      [(hpat:peek hp)
       (hpat:peek (H hp))]
      [(hpat:peek-not hp)
       (hpat:peek-not (H hp))]
      [(? pattern? sp)
       (S* sp)]
      ;; ----
      ;; Default: no sub-patterns, just return
      [p p]))

  (if allow-head? (H p0) (S p0)))

;; ============================================================

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
    (values rest decls3 defs sides)))

;; parse-pattern-sides : (listof chunk) DeclEnv -> (listof SideClause)
;; Invariant: decls contains only literals bindings
(define (parse-pattern-sides chunks decls)
  (match chunks
    [(cons (list '#:declare declare-stx _ _) rest)
     (wrong-syntax declare-stx
                   "#:declare can only appear immediately after pattern or #:with clause")]
    [(cons (list '#:role role-stx _) rest)
     (wrong-syntax role-stx "#:role can only appear immediately after #:declare clause")]
    [(cons (list '#:fail-when fw-stx when-expr msg-expr) rest)
     (cons (create-post-pattern (action:fail when-expr msg-expr))
           (parse-pattern-sides rest decls))]
    [(cons (list '#:fail-unless fu-stx unless-expr msg-expr) rest)
     (cons (create-post-pattern (action:fail #`(not #,unless-expr) msg-expr))
           (parse-pattern-sides rest decls))]
    [(cons (list '#:when w-stx unless-expr) rest)
     (cons (create-post-pattern (action:fail #`(not #,unless-expr) #'#f))
           (parse-pattern-sides rest decls))]
    [(cons (list '#:with with-stx pattern expr) rest)
     (let-values ([(decls2 rest) (grab-decls rest decls)])
       (let-values ([(decls2a defs) (decls-create-defs decls2)])
         (list* (action:do defs)
                (create-post-pattern
                 (action:parse (parse-whole-pattern pattern decls2a #:kind 'with) expr))
                (parse-pattern-sides rest decls))))]
    [(cons (list '#:attr attr-stx a expr) rest)
     (cons (action:bind a expr) ;; no POST wrapper, cannot fail
           (parse-pattern-sides rest decls))]
    [(cons (list '#:post post-stx pattern) rest)
     (cons (create-post-pattern (parse-action-pattern pattern decls))
           (parse-pattern-sides rest decls))]
    [(cons (list '#:and and-stx pattern) rest)
     (cons (parse-action-pattern pattern decls) ;; no POST wrapper
           (parse-pattern-sides rest decls))]
    [(cons (list '#:do do-stx stmts) rest)
     (cons (action:do stmts)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:undo undo-stx stmts) rest)
     (cons (action:undo stmts)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:cut cut-stx) rest)
     (cons (action:cut)
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

;; check-literals-list : stx stx -> (listof den:lit)
;;  - txlifts defs of phase expressions
;;  - txlifts checks that literals are bound
(define (check-literals-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literals list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-literal-entry x ctx)))

;; check-literal-entry : stx stx -> den:lit
(define (check-literal-entry stx ctx)
  (define (go internal external phase)
    (txlift #`(check-literal #,external #,phase #,ctx))
    (let ([external (syntax-property external 'literal (gensym))])
      (make den:lit internal external phase phase)))
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
     (raise-syntax-error #f "expected literal entry" ctx stx)]))

;; check-datum-literals-list : stx stx -> (listof den:datum-lit)
(define (check-datum-literals-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected datum-literals list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-datum-literal-entry x ctx)))

;; check-datum-literal-entry : stx stx -> den:datum-lit
(define (check-datum-literal-entry stx ctx)
  (syntax-case stx ()
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (make den:datum-lit #'internal (syntax-e #'external))]
    [id
     (identifier? #'id)
     (make den:datum-lit #'id (syntax-e #'id))]
    [_
     (raise-syntax-error #f "expected datum-literal entry" ctx stx)]))

;; Literal sets - Import

;; check-literal-sets-list : stx stx -> (listof (list id literalset stx stx))
(define (check-literal-sets-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literal-set list" ctx stx))
  (for/list ([x (in-list (stx->list stx))])
    (check-literal-set-entry x ctx)))

;; check-literal-set-entry : stx stx -> (list id literalset stx stx)
(define (check-literal-set-entry stx ctx)
  (define (elaborate litset-id lctx phase)
    (let ([litset (syntax-local-value/record litset-id literalset?)])
      (unless litset
        (raise-syntax-error #f "expected identifier defined as a literal-set"
                            ctx litset-id))
      (list litset-id litset lctx phase)))
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
     (action:bind (check-attr-arity #'attr-decl ctx) #'expr)]
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
        (list '#:datum-literals check-datum-literals-list)
        (list '#:literal-sets check-literal-sets-list)
        (list '#:conventions check-conventions-list)
        (list '#:local-conventions check-conventions-rules)))

;; parse-directive-table
(define parse-directive-table
  (list* (list '#:context check-expression)
         (list '#:track-literals)
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
        (list '#:and check-expression)
        (list '#:post check-expression)
        (list '#:do check-stmt-list)
        (list '#:undo check-stmt-list)
        (list '#:cut)))

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
