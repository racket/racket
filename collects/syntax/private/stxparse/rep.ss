#lang scheme/base
(require (for-template scheme/base)
         (for-template "runtime.ss")
         scheme/contract
         scheme/match
         scheme/dict
         syntax/id-table
         syntax/stx
         syntax/keyword
         "../util.ss"
         "rep-data.ss"
         "codegen-data.ss")

;; Error reporting
;; All entry points should have explicit, mandatory #:context arg
;; (mandatory from outside, at least)

(provide/contract
 [parse-rhs
  (-> syntax? boolean? boolean? #:context (or/c false/c syntax?)
      rhs?)]
 [parse-whole-pattern 
  (-> syntax? DeclEnv/c #:context (or/c false/c syntax?)
      pattern?)]
 [parse-pattern-directives
  (-> stx-list?
      #:allow-declare? boolean?
      #:decls (or/c false/c DeclEnv/c)
      #:context (or/c false/c syntax?)
      (values stx-list? DeclEnv/c (listof syntax?) (listof SideClause/c)))]
 [parse-directive-table any/c]
 [get-decls+defs
  (-> list? boolean? #:context (or/c false/c syntax?)
      (values DeclEnv/c (listof syntax?)))]
 #|
 [decls-create-defs
  (-> DeclEnv/c
      (values DeclEnv/c (listof syntax?)))]
 |#
 [create-aux-def
  (-> list? ;; DeclEntry
      (values identifier? identifier? (listof sattr?) (listof syntax?)))]
 [check-literals-list
  (-> syntax? syntax?
      (listof (list/c identifier? identifier?)))]
 #|
 [check-literal-sets-list
  (-> syntax? syntax?
      (listof (listof (list/c identifier? identifier?))))]
 |#
 [check-conventions-rules
  (-> syntax? syntax?
      (listof (list/c regexp? any/c)))])

(define (atomic-datum? stx)
  (let ([datum (syntax-e stx)])
    (or (null? datum)
        (boolean? datum)
        (string? datum)
        (number? datum)
        (keyword? datum))))

(define (id-predicate kw)
  (lambda (stx)
    (and (identifier? stx)
         (free-identifier=? stx kw))))

(define wildcard? (id-predicate (quote-syntax _)))
(define epsilon?  (id-predicate (quote-syntax ||)))
(define dots?     (id-predicate (quote-syntax ...)))

(define keywords
  (list (quote-syntax _)
        (quote-syntax ||)
        (quote-syntax ...)
        (quote-syntax ~and)
        (quote-syntax ~or)
        (quote-syntax ~seq)
        (quote-syntax ~rep)
        (quote-syntax ~once)
        (quote-syntax ~optional)
        (quote-syntax ~bounds)
        (quote-syntax ~rest)
        (quote-syntax ~struct)
        (quote-syntax ~!)
        (quote-syntax ~describe)
        (quote-syntax ~bind)
        (quote-syntax ~fail)))

(define (reserved? stx)
  (and (identifier? stx)
       (for/or ([kw keywords])
         (free-identifier=? stx kw))))

;; ---

;; parse-rhs : stx boolean boolean stx -> RHS
;; If strict? is true, then referenced stxclasses must be defined and
;; literals must be bound. Set to #f for pass1 (attr collection);
;; parser requires stxclasses to be bound.
(define (parse-rhs stx strict? splicing? #:context ctx)
  (parameterize ((current-syntax-context ctx))
    (define-values (rest description transp? attributes auto-nested? decls defs)
      (parse-rhs/part1 stx strict?))
    (define patterns
      (parameterize ((stxclass-lookup-config
                      (cond [strict? 'yes]
                            [auto-nested? 'try]
                            [else 'no])))
        (parse-variants rest decls splicing?)))
    (when (null? patterns)
      (wrong-syntax #f "expected at least one variant"))
    (let ([sattrs
           (or attributes
               (intersect-sattrss (map variant-attrs patterns)))])
      (make rhs stx sattrs transp? description patterns defs))))

(define (parse-rhs/part1 stx strict?)
  (define-values (chunks rest)
    (parse-keyword-options stx rhs-directive-table
                           #:context (current-syntax-context)
                           #:incompatible '((#:attributes #:auto-nested-attributes))
                           #:no-duplicates? #t))
  (define description (options-select-value chunks '#:description #:default #f))
  (define opaque? (and (assq '#:opaque chunks) #t))
  (define transparent? (not opaque?))
  (define auto-nested? (and (assq '#:auto-nested-attributes chunks) #t))
  (define attributes (options-select-value chunks '#:attributes #:default #f))
  (define-values (decls defs) (get-decls+defs chunks strict?))
  (values rest description transparent? attributes auto-nested? decls defs))

(define (parse-variants rest decls splicing?)
  (define (gather-patterns stx)
    (syntax-case stx (pattern)
      [((pattern . _) . rest)
       (cons (parse-variant (stx-car stx) splicing? decls)
             (gather-patterns #'rest))]
      [(bad-variant . rest)
       (wrong-syntax #'bad-variant "expected syntax-class variant")]
      [()
       null]))
  (gather-patterns rest))

;; get-decls+defs : chunks boolean -> (values DeclEnv (listof syntax))
(define (get-decls+defs chunks strict?
                        #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (decls-create-defs (get-decls chunks strict?))))

;; get-decls : chunks -> DeclEnv
(define (get-decls chunks strict?)
  (define lits (options-select-value chunks '#:literals #:default null))
  (define litsets (options-select-value chunks '#:literal-sets #:default null))
  (define convs (options-select-value chunks '#:conventions #:default null))
  (define literals
    (append-lits+litsets (check-literals-bound lits strict?)
                         litsets))
  (define convention-rules (apply append convs))
  (new-declenv literals #:conventions convention-rules))

(define (check-literals-bound lits strict?)
  (when strict?
    (for ([p lits])
      ;; FIXME: hack...
      (unless (or (identifier-binding (cadr p) 0)
                  (identifier-binding (cadr p) 1)
                  (identifier-binding (cadr p) #f)
                  (identifier-binding (cadr p) (syntax-local-phase-level)))
        (wrong-syntax (cadr p) "unbound identifier not allowed as literal"))))
  lits)

;; decls-create-defs : DeclEnv -> (values DeclEnv (listof stx))
(define (decls-create-defs decls0)
  (for/fold ([decls decls0] [defs null])
      ([(k v) (in-dict (declenv-table decls0))]
       #:when (memq (car v) '(stxclass splicing-stxclass)))
    (let-values ([(parser description attrs new-defs) (create-aux-def v)])
      (values (declenv-put-parser decls k parser description attrs
                                  (eq? (car v) 'splicing-stxclass))
              (append new-defs defs)))))

;; create-aux-def : DeclEntry -> (values id id (listof SAttr) (listof stx))
(define (create-aux-def entry)
  (let ([sc-name (caddr entry)]
        [args (cadddr entry)])
    (let ([sc (get-stxclass/check-arg-count sc-name (length args))])
      (with-syntax ([sc-parser (stxclass-parser-name sc)]
                    [sc-description (stxclass-description sc)])
        (if (pair? args)
            (with-syntax ([x (generate-temporary 'x)]
                          [parser (generate-temporary sc-name)]
                          [description (generate-temporary sc-name)]
                          [(arg ...) args])
              (values #'parser #'description (stxclass-attrs sc)
                      (list #'(define (parser x) (sc-parser x arg ...))
                            #'(define (description) (description arg ...)))))
            (values #'sc-parser #'sc-description (stxclass-attrs sc)
                    null))))))

(define (append-lits+litsets lits litsets)
  (define seen (make-bound-id-table lits))
  (for ([litset litsets])
    (for ([lit litset])
      (when (bound-id-table-ref seen (car lit) #f)
        (wrong-syntax (car lit) "duplicate literal declaration"))
      (bound-id-table-set! seen (car lit) #t)))
  (apply append lits litsets))

;; parse-variant : stx boolean DeclEnv -> RHS
(define (parse-variant stx splicing? decls0)
  (syntax-case stx (pattern)
    [(pattern p . rest)
     (let-values ([(rest decls defs clauses)
                   (parse-pattern-directives #'rest
                                             #:allow-declare? #t
                                             #:decls decls0)])
       (unless (stx-null? rest)
         (wrong-syntax (if (pair? rest) (car rest) rest)
                       "unexpected terms after pattern directives"))
       (let* ([pattern
               (parse-whole-pattern #'p decls splicing?)]
              [attrs
               (append-iattrs
                (cons (pattern-attrs pattern)
                      (side-clauses-attrss clauses)))]
              [sattrs (iattrs->sattrs attrs)])
         (make variant stx sattrs pattern clauses defs)))]))

(define (side-clauses-attrss clauses)
  (for/list ([c clauses]
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

;; ----

;; parse-single-pattern : stx DeclEnv -> SinglePattern
(define (parse-single-pattern stx decls)
  (syntax-case stx (~and ~or ~rest ~struct ~! ~describe ~bind ~fail)
    [wildcard
     (wildcard? #'wildcard)
     (make pat:any null)]
    [reserved
     (reserved? #'reserved)
     (wrong-syntax stx "not allowed here")]
    [id
     (identifier? #'id)
     (parse-pat:id stx decls #f)]
    [datum
     (atomic-datum? #'datum)
     (make pat:datum null (syntax->datum #'datum))]
    [(~and . rest)
     (parse-pat:and stx decls)]
    [(~or . rest)
     (parse-pat:or stx decls #f)]
    [(head dots . tail)
     (dots? #'dots)
     (parse-pat:dots stx #'head #'tail decls)]
    [(~struct key . contents)
     (let ([lp (parse-single-pattern (syntax/loc stx contents) decls)]
           [key (syntax->datum #'key)])
       (make pat:compound (pattern-attrs lp) `(#:pstruct ,key) (list lp)))]
    [(~! . rest)
     (let ([inner (parse-single-pattern (syntax/loc stx rest) decls)])
       (make pat:cut (pattern-attrs inner) inner))]
    [(~describe . rest)
     (parse-pat:describe stx decls #f)]
    [(~bind . rest)
     (parse-pat:bind stx decls)]
    [(~fail . rest)
     (parse-pat:fail stx decls)]
    [(~rest . rest)
     (parse-pat:rest stx decls)]
    [(head . tail)
     (parse-pat:pair stx #'head #'tail decls)]
    [#(a ...)
     (let ([lp (parse-single-pattern (syntax/loc stx (a ...)) decls)])
       (make pat:compound (pattern-attrs lp) '#:vector (list lp)))]
    [b
     (box? (syntax-e #'b))
     (let ([bp (parse-single-pattern (unbox (syntax-e #'b)) decls)])
       (make pat:compound (pattern-attrs bp) '#:box (list bp)))]
    [s
     (and (struct? (syntax-e #'s)) (prefab-struct-key (syntax-e #'s)))
     (let* ([s (syntax-e #'s)]
            [key (prefab-struct-key s)]
            [contents (cdr (vector->list (struct->vector s)))])
       (let ([lp (parse-single-pattern (datum->syntax #f contents #'s) decls)])
         (make pat:compound (pattern-attrs lp) `(#:pstruct ,key) (list lp))))]))

;; parse-head-pattern : stx DeclEnv -> HeadPattern
(define (parse-head-pattern stx decls)
  (syntax-case stx (~or ~seq ~describe ~optional)
    [id
     (and (identifier? #'id) (not (reserved? #'id)))
     (parse-pat:id stx decls #t)]
    [(~or . rest)
     (parse-pat:or stx decls #t)]
    [(~seq . rest)
     (parse-hpat:seq stx #'rest decls)]
    [(~describe . rest)
     (parse-pat:describe stx decls #t)]
    [(~optional . rest)
     (parse-hpat:optional stx decls)]
    [_
     (parse-single-pattern stx decls)]))

;; parse-ellipsis-head-pattern : stx DeclEnv number -> EllipsisHeadPattern
(define (parse-ellipsis-head-pattern stx decls)
  (syntax-case stx (~bounds ~optional ~once)
    [(~optional . _)
     (parse-ehpat/optional stx decls)]
    [(~once . _)
     (parse-ehpat/once stx decls)]
    [(~bounds . _)
     (parse-ehpat/bounds stx decls)]
    [_
     (let ([head (parse-head-pattern stx decls)])
       (make ehpat (map increase-depth (pattern-attrs head))
             head
             #f))]))

;; ----

(define (parse-pat:id id decls allow-head?)
  (define entry (declenv-lookup decls id))
  (match entry
    [(list 'literal internal-id literal-id)
     (make pat:literal null literal-id)]
    [(list 'stxclass _ _ _)
     (error 'parse-pat:id
            "(internal error) decls had leftover 'stxclass entry: ~s"
            entry)]
    [(list 'splicing-stxclass _ _ _)
     (error 'parse-pat:id
            "(internal error) decls had leftover 'splicing-stxclass entry: ~s"
            entry)]
    [(list 'parser parser description attrs)
     (parse-pat:id/s id id parser description attrs)]
    [(list 'splicing-parser parser description attrs)
     (parse-pat:id/h id id parser description attrs)]
    [#f
     (let-values ([(name sc) (split-id/get-stxclass id decls)])
       (cond [(stxclass/s? sc)
              (parse-pat:id/s id name
                              (stxclass-parser-name sc)
                              (stxclass-description sc)
                              (stxclass-attrs sc))]
             [(stxclass/h? sc)
              (unless allow-head?
                (wrong-syntax id "splicing syntax class not allowed here"))
              (parse-pat:id/h id name
                              (stxclass-parser-name sc)
                              (stxclass-description sc)
                              (stxclass-attrs sc))]
             [else
              (wrap/name name (make pat:any null))]))]))

(define (parse-pat:id/s stx name parser description attrs)
  (define prefix (name->prefix name))
  (define bind (name->bind name))
  (make pat:sc (id-pattern-attrs attrs bind prefix)
        parser description (and bind #t) (and prefix #t)))

(define (parse-pat:id/h stx name parser description attrs)
  (define prefix (name->prefix name))
  (define bind (name->bind name))
  (make hpat:ssc (id-pattern-attrs attrs bind prefix)
        parser description (and bind #t) (and prefix #t)))

(define (name->prefix id)
  (cond [(wildcard? id) #f]
        [(epsilon? id) id]
        [else (datum->syntax id (format-symbol "~a." (syntax-e id)))]))

(define (name->bind id)
  (cond [(wildcard? id) #f]
        [(epsilon? id) #f]
        [else id]))

(define (wrap/name id pattern)
  (cond [(wildcard? id) pattern]
        [(epsilon? id) pattern]
        [else
         (let ([a (make attr id 0 #t)])
           (make pat:name (cons a (pattern-attrs pattern)) pattern (list id)))]))

;; id-pattern-attrs : (listof SAttr) id/#f IdPrefix -> (listof IAttr)
(define (id-pattern-attrs sattrs bind prefix)
  (let ([rest
         (if prefix
             (for/list ([a sattrs])
               (prefix-attr a prefix))
             null)])
    (if bind
        (cons (make attr bind 0 #t) rest)
        rest)))

;; prefix-attr : SAttr identifier -> IAttr
(define (prefix-attr a prefix)
  (make attr (prefix-attr-name prefix (attr-name a))
        (attr-depth a)
        (attr-syntax? a)))

;; prefix-attr-name : id symbol -> id
(define (prefix-attr-name prefix name)
  (datum->syntax prefix (format-symbol "~a~a" (syntax-e prefix) name)))

;; ----

(define (parse-pat:describe stx decls allow-head?)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options #'rest describe-option-table
                                          #:no-duplicates? #t
                                          #:context stx)])
       (define transparent? (and (assq '#:transparent chunks) #t))
       (syntax-case rest ()
         [(description pattern)
          (let ([p (parse-some-pattern #'pattern decls allow-head?)])
            (if (head-pattern? p)
                (make hpat:describe (pattern-attrs p)
                      #'description transparent? p)
                (make pat:describe (pattern-attrs p)
                      #'description transparent? p)))]))]))

(define (parse-pat:or stx decls allow-head?)
  (define patterns (parse-cdr-patterns stx decls allow-head? #f))
  (cond [(null? (cdr patterns))
         (car patterns)]
        [else
         (let ()
           (define attrs (union-iattrs (map pattern-attrs patterns)))
           (cond [(ormap head-pattern? patterns)
                  (make-hpat:or attrs patterns)]
                 [else
                  (make-pat:or attrs patterns)]))]))

(define (parse-pat:and stx decls)
  (define patterns (parse-cdr-patterns stx decls #f #t))
  (make pat:and (append-iattrs (map pattern-attrs patterns)) patterns))

(define (parse-hpat:seq stx list-stx decls)
  (define pattern (parse-single-pattern list-stx decls))
  (check-list-pattern pattern stx)
  (make hpat:seq (pattern-attrs pattern) pattern))

(define (parse-cdr-patterns stx decls allow-head? allow-cut?)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected sequence of patterns"))
  (let ([result
         (for/list ([sub (cdr (stx->list stx))])
           (if allow-cut?
               (or (parse-cut-in-and sub)
                   (parse-some-pattern sub decls allow-head?))
               (parse-some-pattern sub decls allow-head?)))])
    (when (null? result)
      (wrong-syntax stx "expected at least one pattern"))
    result))

(define (parse-cut-in-and stx)
  (syntax-case stx (~!)
    [~! (make pat:cut null (make pat:any null))]
    [_ #f]))

(define (parse-some-pattern stx decl allow-head?)
  (define p (parse-head-pattern stx decl))
  (when (head-pattern? p)
    (unless allow-head?
      (wrong-syntax stx "head pattern not allowed here")))
  p)

(define (parse-pat:dots stx head tail decls)
  (define headps
    (syntax-case head (~or)
      [(~or . _)
       (begin
         (unless (stx-list? head)
           (wrong-syntax head "expected sequence of patterns"))
         (unless (stx-pair? (stx-cdr head))
           (wrong-syntax head "expected at least one pattern"))
         (for/list ([sub (cdr (stx->list head))])
           (parse-ellipsis-head-pattern sub decls)))]
      [_
       (list (parse-ellipsis-head-pattern head decls))]))
  (define tailp (parse-single-pattern tail decls))
  (define attrs
    (append-iattrs (cons (pattern-attrs tailp)
                         (map pattern-attrs headps))))
  (make pat:dots attrs headps tailp))

(define (parse-pat:bind stx decls)
  (syntax-case stx ()
    [(_ clause ...)
     (let ([clauses (check-bind-clause-list #'(clause ...) stx)])
       (make pat:bind
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
            (make pat:fail null condition #'message)]
           [()
            (wrong-syntax stx "missing message expression")]
           [_
            (wrong-syntax stx "bad fail pattern")])))]))

(define (parse-pat:rest stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (parse-single-pattern #'pattern decls)]))

(define (parse-pat:pair stx head tail decls)
  (define headp (parse-head-pattern head decls))
  (define tailp (parse-single-pattern tail decls))
  (define attrs
    (append-iattrs
     (list (pattern-attrs headp) (pattern-attrs tailp))))
  ;; Only make pat:head if head is complicated; otherwise simple compound/pair
  ;; FIXME: Could also inline ~seq patterns from head...?
  (if (head-pattern? headp)
      (make pat:head attrs headp tailp)
      (make pat:compound attrs '#:pair (list headp tailp))))

(define (check-list-pattern pattern stx)
  (match pattern
    [(struct pat:datum (_base '()))
     #t]
    [(struct pat:head (_base _head tail))
     (check-list-pattern tail stx)]
    [(struct pat:dots (_base _head tail))
     (check-list-pattern tail stx)]
    [(struct pat:compound (_base '#:pair (list _head tail)))
     (check-list-pattern tail stx)]
    [(struct pat:name (_ pattern _))
     (check-list-pattern pattern stx)]
    [else
     (wrong-syntax stx "expected proper list pattern")]))

(define (parse-hpat:optional stx decls)
  (define-values (head all-iattrs _name _tmm defaults)
    (parse-optional-pattern stx decls h-optional-directive-table))
  (make hpat:optional all-iattrs head defaults))

(define (parse-ehpat/optional stx decls)
  (define-values (head all-iattrs name too-many-msg defaults)
    (parse-optional-pattern stx decls eh-optional-directive-table))
  (make ehpat all-iattrs head
        (make rep:optional name too-many-msg defaults)))

(define (parse-optional-pattern stx decls optional-directive-table)
  (syntax-case stx (~optional)
    [(~optional p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define chunks
         (parse-keyword-options/eol #'options optional-directive-table
                                    #:no-duplicates? #t
                                    #:context stx))
       (let ([too-many-msg
              (options-select-value chunks '#:too-many #:default #'#f)]
             [name
              (options-select-value chunks '#:name #:default #'#f)]
             [defaults
              (options-select-value chunks '#:defaults #:default '())])
         (define pattern-iattrs (pattern-attrs head))
         (define defaults-iattrs
           (append-iattrs (side-clauses-attrss defaults)))
         (define all-iattrs
           (union-iattrs (list pattern-iattrs defaults-iattrs)))
         (check-iattrs-subset defaults-iattrs pattern-iattrs stx)
         (values head all-iattrs name too-many-msg defaults)))]))

(define (parse-ehpat/once stx decls)
  (syntax-case stx (~once)
    [(~once p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define chunks
         (parse-keyword-options/eol #'options
                                    (list (list '#:too-few check-expression)
                                          (list '#:too-many check-expression)
                                          (list '#:name check-expression))
                                    #:context stx))
       (let ([too-few-msg
              (options-select-value chunks '#:too-few #:default #'#f)]
             [too-many-msg
              (options-select-value chunks '#:too-many #:default #'#f)]
             [name
              (options-select-value chunks '#:name #:default #'#f)])
         (make ehpat (pattern-attrs head)
               head
               (make rep:once name too-few-msg too-many-msg))))]))

(define (parse-ehpat/bounds stx decls)
  (syntax-case stx (~bounds)
    [(~bounds p min max . options)
     (let ([head (parse-head-pattern #'p decls)])
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
       (let ([chunks (parse-keyword-options #'options
                                            (list (list '#:too-few check-expression)
                                                  (list '#:too-many check-expression)
                                                  (list '#:name check-expression))
                                            #:context stx)])
         (let ([too-few-msg
                (options-select-value chunks '#:too-few #:default #'#f)]
               [too-many-msg
                (options-select-value chunks '#:too-many #:default #'#f)]
               [name
                (options-select-value chunks '#:name #:default #'#f)])
           (make ehpat (map increase-depth (pattern-attrs head))
                 head
                 (make rep:bounds #'min #'max
                       name too-few-msg too-many-msg)))))]))

;; -----

;; parse-pattern-directives : stxs(PatternDirective) <kw-args>
;;                         -> stx DeclEnv (listof stx) (listof SideClause)
(define (parse-pattern-directives stx
                                  #:allow-declare? allow-declare?
                                  #:decls decls
                                  #:context [ctx (current-syntax-context)])
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
    [(cons (list '#:fail-when fw-stx when-condition expr) rest)
     (cons (make clause:fail when-condition expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:fail-unless fu-stx unless-condition expr) rest)
     (cons (make clause:fail #`(not #,unless-condition) expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:when unless-condition) rest)
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
    ['()
     '()]))

;; grab-decls : (listof chunk) DeclEnv
;;           -> (values DeclEnv (listof chunk))
(define (grab-decls chunks decls)
  (define (add-decl stx decls)
    (syntax-case stx ()
      [(#:declare name sc)
       (identifier? #'sc)
       (add-decl* #'name #'sc null)]
      [(#:declare name (sc expr ...))
       (identifier? #'sc)
       (add-decl* #'name #'sc (syntax->list #'(expr ...)))]
      [(#:declare name bad-sc)
       (wrong-syntax #'bad-sc
                     "expected syntax class name (possibly with parameters)")]))
  (define (add-decl* id sc-name args)
    (declenv-put-stxclass decls id sc-name args))
  (define (loop chunks decls)
    (match chunks
      [(cons (cons '#:declare decl-stx) rest)
       (loop rest (add-decl decl-stx decls))]
      [else (values decls chunks)]))
  (loop chunks decls))


;; ----

;; Keyword Options & Checkers

;; check-attr-arity-list : stx stx -> (listof SAttr)
(define (check-attr-arity-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected list of attribute declarations" ctx stx))
  (let ([iattrs (for/list ([x (stx->list stx)]) (check-attr-arity x ctx))])
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

;; check-literals-list : stx stx -> (listof (list id id))
(define (check-literals-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literals list" ctx stx))
  (let ([lits (for/list ([x (stx->list stx)]) (check-literal-entry x ctx))])
    (let ([dup (check-duplicate-identifier (map car lits))])
      (when dup (raise-syntax-error #f "duplicate literal identifier" ctx dup)))
    lits))

;; check-literal-entry : stx stx -> (list id id)
(define (check-literal-entry stx ctx)
  (syntax-case stx ()
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (list #'internal #'external)]
    [id
     (identifier? #'id)
     (list #'id #'id)]
    [_
     (raise-syntax-error #f "expected literal (identifier or pair of identifiers)" ctx stx)]))

(define (check-literal-sets-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literal-set list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-literal-set-entry x ctx)))

(define (check-literal-set-entry stx ctx)
  (define (elaborate litset-id lctx)
    (let ([litset (syntax-local-value litset-id (lambda () #f))])
      (unless (literalset? litset)
        (raise-syntax-error #f "expected identifier defined as a literal-set" ctx litset-id))
      (elaborate-litset litset lctx stx)))
  (syntax-case stx ()
    [(litset #:at lctx)
     (and (identifier? #'litset) (identifier? #'lctx))
     (elaborate #'litset #'lctx)]
    [litset
     (identifier? #'litset)
     (elaborate #'litset #'litset)]
    [_
     (raise-syntax-error #f "expected literal-set entry" ctx stx)]))

(define (elaborate-litset litset lctx srcctx)
  (for/list ([entry (literalset-literals litset)])
    (list (datum->syntax lctx (car entry) srcctx)
          (cadr entry))))

(define (check-conventions-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected conventions list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-conventions x ctx)))

(define (check-conventions stx ctx)
  (define (elaborate conventions-id)
    (let ([cs (syntax-local-value conventions-id (lambda () #f))])
      (unless (conventions? cs)
        (raise-syntax-error #f "expected identifier defined as a conventions" ctx conventions-id))
      (conventions-rules cs)))
  (syntax-case stx ()
    [conventions
     (identifier? #'conventions)
     (elaborate #'conventions)]
    [_
     (raise-syntax-error "expected conventions entry" ctx stx)]))

(define (check-conventions-rules stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected convention rule list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-conventions-rule x ctx)))

(define (check-conventions-rule stx ctx)
  (define (check-conventions-pattern x blame)
    (cond [(symbol? x) (regexp (string-append "^" (regexp-quote (symbol->string x)) "$"))]
          [(regexp? x) x]
          [else (raise-syntax-error #f  "expected identifier convention pattern" ctx blame)]))
  (define (check-sc-expr x)
    (syntax-case x ()
      [sc (identifier? #'sc) (list #'sc null)]
      [(sc arg ...) (identifier? #'sc) (list #'sc #'(arg ...))]
      [_ (raise-syntax-error #f "expected syntax class use" ctx x)]))
  (syntax-case stx ()
    [(rx sc)
     (list (check-conventions-pattern (syntax-e #'rx) #'rx)
           (check-sc-expr #'sc))]))

;; bind clauses
(define (check-bind-clause-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected sequence of bind clauses" ctx stx))
  (for/list ([clause (stx->list stx)])
    (check-bind-clause clause ctx)))

(define (check-bind-clause clause ctx)
  (syntax-case clause ()
    [(attr-decl expr)
     (make clause:attr (check-attr-arity #'attr-decl ctx) #'expr)]
    [_ (raise-syntax-error #f "expected bind clause" ctx clause)]))

;; common-parse-directive-table
(define common-parse-directive-table
  (list (list '#:literals check-literals-list)
        (list '#:literal-sets check-literal-sets-list)
        (list '#:conventions check-conventions-list)))

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
         common-parse-directive-table))

;; pattern-directive-table
(define pattern-directive-table
  (list (list '#:declare check-identifier check-expression)
        (list '#:fail-when check-expression check-expression)
        (list '#:fail-unless check-expression check-expression)
        (list '#:when check-expression)
        (list '#:with check-expression check-expression)
        (list '#:attr check-attr-arity check-expression)))

;; fail-directive-table
(define fail-directive-table
  (list (list '#:when check-expression)
        (list '#:unless check-expression)))

;; describe-option-table
(define describe-option-table
  (list (list '#:transparent)))

;; eh-optional-directive-table
(define eh-optional-directive-table
  (list (list '#:too-many check-expression)
        (list '#:name check-expression)
        (list '#:defaults check-bind-clause-list)))

;; h-optional-directive-table
(define h-optional-directive-table
  (list (list '#:defaults check-bind-clause-list)))
