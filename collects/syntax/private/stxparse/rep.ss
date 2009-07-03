#lang scheme/base
(require (for-template scheme/base)
         (for-template "runtime.ss")
         scheme/contract
         scheme/match
         scheme/dict
         syntax/id-table
         syntax/stx
         "../util.ss"
         "rep-data.ss"
         "codegen-data.ss")

(provide/contract
 [parse-rhs
  (-> syntax? boolean? boolean? syntax?
      rhs?)]
 [parse-whole-pattern 
  (-> syntax? DeclEnv/c
      pattern?)]
 [parse-pattern-directives
  (->* [stx-list?]
       [#:decls DeclEnv/c #:allow-declare? boolean?]
       (values stx-list? DeclEnv/c (listof SideClause/c)))]
 [parse-directive-table any/c]
 [get-decls+defs
  (-> list?
      (values DeclEnv/c (listof syntax?)))]
 [check-literals-list
  (-> syntax?
      (listof (list/c identifier? identifier?)))]
 [check-literal-sets-list
  (-> syntax?
      (listof (listof (list/c identifier? identifier?))))]
 [append-lits+litsets
  (-> (listof (list/c identifier? identifier?))
      (listof (listof (list/c identifier? identifier?)))
      syntax?
      (listof (list/c identifier? identifier?)))]
 [check-conventions-rules any/c]
 [create-aux-def any/c])

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
;; If allow-unbound? is true, then all stxclasses act as if they have no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-rhs stx allow-unbound? splicing? ctx)
  (define-values (rest description transparent? attributes decls defs)
    (parse-rhs/part1 stx ctx))
  (define patterns (parse-variants rest decls allow-unbound? splicing? ctx))
  (when (null? patterns)
    (wrong-syntax ctx "expected at least one variant"))
  (let ([sattrs
         (or attributes
             (intersect-sattrss (map variant-attrs patterns)))])
    (make rhs stx sattrs transparent? description patterns defs)))

(define (parse-rhs/part1 stx ctx)
  (define-values (chunks rest)
    (chunk-kw-seq/no-dups stx rhs-directive-table #:context ctx))
  (define desc0 (assq '#:description chunks))
  (define trans0 (assq '#:transparent chunks))
  (define attrs0 (assq '#:attributes chunks))
  (define description (and desc0 (caddr desc0)))
  (define transparent? (and trans0 #t))
  (define attributes (and attrs0 (caddr attrs0)))
  (define-values (decls defs) (get-decls+defs chunks))
  (values rest description transparent? attributes decls defs))

(define (parse-variants rest decls allow-unbound? splicing? ctx)
  (define (gather-patterns stx)
    (syntax-case stx (pattern)
      [((pattern . _) . rest)
       (cons (parse-variant (stx-car stx) allow-unbound? splicing? decls)
             (gather-patterns #'rest))]
      [(bad-variant . rest)
       (raise-syntax-error #f "expected syntax-class variant" ctx #'bad-variant)]
      [()
       null]))
  (gather-patterns rest))

;; get-decls+defs : chunks -> (values DeclEnv (listof syntax))
(define (get-decls+defs chunks)
  (decls-create-defs (get-decls chunks)))

;; get-decls : chunks -> DeclEnv
(define (get-decls chunks #:context [ctx #f])
  (define lits0 (assq '#:literals chunks))
  (define litsets0 (assq '#:literal-sets chunks))
  (define convs0 (assq '#:conventions chunks))
  (define literals
    (append-lits+litsets
     (if lits0 (caddr lits0) null)
     (if litsets0 (caddr litsets0) null)
     ctx))
  (define convention-rules (if convs0 (apply append (caddr convs0)) null))
  (new-declenv literals #:conventions convention-rules))

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

(define (append-lits+litsets lits litsets ctx)
  (define seen (make-bound-id-table lits))
  (for ([litset litsets])
    (for ([lit litset])
      (when (bound-id-table-ref seen (car lit) #f)
        (raise-syntax-error #f "duplicate literal declaration" ctx (car lit)))
      (bound-id-table-set! seen (car lit) #t)))
  (apply append lits litsets))

;; parse-variant : stx boolean boolean boolean DeclEnv -> RHS
(define (parse-variant stx allow-unbound? splicing? decls0)
  (syntax-case stx (pattern)
    [(pattern p . rest)
     (parameterize ((use-dummy-stxclasses? allow-unbound?))
       (let-values ([(rest decls1 clauses)
                     (parse-pattern-directives #'rest
                                               #:decls decls0)])
         (define-values (decls defs) (decls-create-defs decls1))
         (unless (stx-null? rest)
           (wrong-syntax (if (pair? rest) (car rest) rest)
                         "unexpected terms after pattern directives"))
         (let* ([pattern (parse-whole-pattern #'p decls splicing?)]
                [attrs
                 (append-iattrs
                  (cons (pattern-attrs pattern)
                        (side-clauses-attrss clauses)))]
                [sattrs (iattrs->sattrs attrs)])
           (make variant stx sattrs pattern clauses defs))))]))

(define (side-clauses-attrss clauses)
  (for/list ([c clauses]
             #:when (or (clause:with? c) (clause:attr? c)))
    (if (clause:with? c)
        (pattern-attrs (clause:with-pattern c))
        (list (clause:attr-attr c)))))

;; parse-whole-pattern : stx DeclEnv boolean -> Pattern
(define (parse-whole-pattern stx decls [splicing? #f])
  (define pattern
    (if splicing?
        (parse-head-pattern stx decls)
        (parse-single-pattern stx decls)))
  (define pvars (map attr-name (pattern-attrs pattern)))
  (define excess-domain (declenv-domain-difference decls pvars))
  (when (pair? excess-domain)
    (wrong-syntax #f "declared pattern variables do not appear in pattern"
                  #:extra excess-domain))
  pattern)


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
  (syntax-case stx (~or ~seq ~describe)
    [id
     (and (identifier? #'id) (not (reserved? #'id)))
     (parse-pat:id stx decls #t)]
    [(~or . rest)
     (parse-pat:or stx decls #t)]
    [(~seq . rest)
     (parse-hpat:seq stx #'rest decls)]
    [(~describe . rest)
     (parse-pat:describe stx decls #t)]
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
     (error 'parse-pat:id "decls had leftover 'stxclass entry: ~s" entry)]
    [(list 'splicing-stxclass _ _ _)
     (error 'parse-pat:id "decls had leftover 'splicing-stxclass entry: ~s" entry)]
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
  (make attr (prefix-attr-name prefix (attr-name a)) (attr-depth a) (attr-syntax? a)))

;; prefix-attr-name : id symbol -> id
(define (prefix-attr-name prefix name)
  (datum->syntax prefix (format-symbol "~a~a" (syntax-e prefix) name)))

;; ----

(define (parse-pat:describe stx decls allow-head?)
  (syntax-case stx ()
    [(_ description pattern)
     (let ([p (parse-some-pattern #'pattern decls allow-head?)])
       (if (head-pattern? p)
           (make hpat:describe (pattern-attrs p) #'description p)
           (make pat:describe (pattern-attrs p) #'description p)))]))

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

;; FIXME: broken, first off, and second, must not reorder names, preserve original scopes
(define (simplify-and-pattern patterns0)
  (define (loop patterns names)
    (cond [(pair? patterns)
           (match (car patterns)
             [(struct pat:any ('()))
              (loop (cdr patterns) names)]
             [(struct pat:name (_ pattern ns))
              (loop (cons pattern (cdr patterns))
                    (append ns names))])]
          [else (values patterns names)]))
  (define-values (patterns names)
    (loop patterns0 null))
  (define base
    (if (pair? patterns)
        (make pat:and (append-iattrs (map pattern-attrs patterns)) patterns)
        (make pat:any '())))
  (if (pair? names)
      (let ([new-attrs (for/list ([name names]) (make attr name 0 #t))])
        (make pat:name (append new-attrs (pattern-attrs base)) base names))
      base))

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
               (or (parse-cut/and sub)
                   (parse-some-pattern sub decls allow-head?))
               (parse-some-pattern sub decls allow-head?)))])
    (when (null? result)
      (wrong-syntax stx "expected at least one pattern"))
    result))

(define (parse-cut/and stx)
  (syntax-case stx (~!)
    [~! (make pat:cut null (make pat:any null))]
    [_ #f]))

(define (parse-some-pattern stx decl allow-head?)
  (define p (parse-head-pattern stx decl))
  (when (head-pattern? p)
    (unless allow-head?
      (wrong-syntax stx "head pattern not allowed")))
  p)

(define (parse-pat:dots stx head tail decls)
  (define headps
    (syntax-case head (~or)
      [(~or . _)
       (begin
         (unless (stx-list? head)
           (wrong-syntax head "expected sequence of patterns"))
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
     (parameterize ((current-syntax-context stx))
       (let ([clauses (map parse-bind-clause (syntax->list #'(clause ...)))])
         (make pat:bind
           (append-iattrs (side-clauses-attrss clauses))
           clauses)))]))

(define (parse-bind-clause clause)
  (syntax-case clause ()
    [(attr-decl expr)
     (make clause:attr (check-attr-arity #'attr-decl) #'expr)]
    [_ (wrong-syntax clause "expected bind clause")]))

(define (parse-pat:fail stx decls)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (chunk-kw-seq/no-dups #'rest
                                         fail-directive-table
                                         #:context stx)])
       ;; chunks has 0 or 1 of each of #:when, #:unless
       ;; if has both, second one is bad; report it
       (when (> (length chunks) 1)
         (wrong-syntax (cadr (cadr chunks))
                       "cannot use both #:when and #:unless conditions"))
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

(define (parse-ehpat/optional stx decls)
  (syntax-case stx (~optional)
    [(~optional p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (with-syntax ([((too-many-msg) (name))
                      (parse-kw-options #'options
                                        (list (list '#:too-many values)
                                              (list '#:name values))
                                        (list (list '#:too-many #'#f)
                                              (list '#:name #'#f))
                                        #:context stx)])
         (make ehpat (map attr-make-uncertain (pattern-attrs head))
               head
               (make rep:optional #'name #'too-many-msg))))]))

(define (parse-ehpat/once stx decls)
  (syntax-case stx (~once)
    [(~once p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (with-syntax ([((too-few-msg) (too-many-msg) (name))
                      (parse-kw-options #'options
                                        (list (list '#:too-few values)
                                              (list '#:too-many values)
                                              (list '#:name values))
                                        (list (list '#:too-few #'#f)
                                              (list '#:too-many #'#f)
                                              (list '#:name #'#f))
                                        #:context stx)])
         (make ehpat (pattern-attrs head)
               head
               (make rep:once #'name #'too-few-msg #'too-many-msg))))]))

(define (parse-ehpat/bounds stx decls)
  (syntax-case stx (~bounds)
    [(~bounds p min max . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define minN (syntax-e #'min))
       (define maxN (syntax-e #'max))
       (unless (exact-nonnegative-integer? minN)
         (wrong-syntax #'min
                       "expected exact nonnegative integer"))
       (unless (or (exact-nonnegative-integer? maxN) (= +inf.0 maxN))
         (wrong-syntax #'max
                       "expected exact nonnegative integer or +inf.0"))
       (when (> minN maxN)
         (wrong-syntax stx "minumum larger than maximum repetition constraint"))
       (with-syntax ([((too-few-msg) (too-many-msg) (name))
                      (parse-kw-options #'options
                                        (list (list '#:too-few values)
                                              (list '#:too-many values)
                                              (list '#:name values))
                                        (list (list '#:too-few #'#f)
                                              (list '#:too-many #'#f)
                                              (list '#:name #'#f)))])
         (make ehpat (map increase-depth (pattern-attrs head))
               head
               (make rep:bounds #'min #'max #'name #'too-few #'too-many))))]))

;; -----

;; parse-pattern-directives : stxs(PatternDirective) <kw-args>
;;                         -> stx DeclEnv (listof SideClause)
(define (parse-pattern-directives stx
                                  #:decls [decls #f]
                                  #:allow-declare? [allow-declare? #t])
  (define-values (chunks rest)
    (chunk-kw-seq stx pattern-directive-table))
  (define-values (decls2 chunks2)
    (if allow-declare?
        (grab-decls chunks decls)
        (values decls chunks)))
  (define sides
    ;; NOTE: use *original* decls
    ;; because decls2 has #:declares for *above* pattern
    (parse-pattern-sides chunks2 decls))
  (values rest decls2 (parse-pattern-sides chunks2 decls)))

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


;; check-lit-string : stx -> string
(define (check-lit-string stx)
  (let ([x (syntax-e stx)])
    (unless (string? x)
      (wrong-syntax stx "expected string literal"))
    x))

;; check-attr-arity-list : stx -> (listof SAttr)
(define (check-attr-arity-list stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected list of attribute declarations"))
  (let ([iattrs (map check-attr-arity (stx->list stx))])
    (iattrs->sattrs (append-iattrs (map list iattrs)))))

;; check-attr-arity : stx -> IAttr
(define (check-attr-arity stx)
  (syntax-case stx ()
    [attr
     (identifier? #'attr)
     (make-attr #'attr 0 #f)]
    [(attr depth)
     (begin (unless (identifier? #'attr)
              (wrong-syntax #'attr "expected attribute name"))
            (unless (exact-nonnegative-integer? (syntax-e #'depth))
              (wrong-syntax #'depth "expected depth (nonnegative integer)"))
            (make-attr #'attr (syntax-e #'depth) #f))]
    [_
     (wrong-syntax stx "expected attribute name with optional depth declaration")]))

;; check-literals-list : syntax -> (listof id)
(define (check-literals-list stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected literals list"))
  (let ([lits (map check-literal-entry (stx->list stx))])
    (let ([dup (check-duplicate-identifier (map car lits))])
      (when dup (wrong-syntax dup "duplicate literal identifier")))
    lits))

(define (check-literal-entry stx)
  (syntax-case stx ()
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (list #'internal #'external)]
    [id
     (identifier? #'id)
     (list #'id #'id)]
    [_
     (wrong-syntax stx
                   "expected literal (identifier or pair of identifiers)")]))

(define (check-literal-sets-list stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected literal-set list"))
  (map check-literal-set-entry (stx->list stx)))

(define (check-literal-set-entry stx)
  (define (elaborate litset-id context)
    (let ([litset (syntax-local-value litset-id (lambda () #f))])
      (unless (literalset? litset)
        (wrong-syntax litset-id "expected identifier defined as a literal-set"))
      (elaborate-litset litset context stx)))
  (syntax-case stx ()
    [(litset #:at context)
     (and (identifier? #'litset) (identifier? #'context))
     (elaborate #'litset #'context)]
    [litset
     (identifier? #'litset)
     (elaborate #'litset #'litset)]
    [_
     (wrong-syntax stx "expected literal-set entry")]))

(define (elaborate-litset litset context ctx)
  (for/list ([entry (literalset-literals litset)])
    (list (datum->syntax context (car entry) ctx)
          (cadr entry))))

(define (check-conventions-list stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected conventions list"))
  (map check-conventions (stx->list stx)))

(define (check-conventions stx)
  (define (elaborate conventions-id)
    (let ([cs (syntax-local-value conventions-id (lambda () #f))])
      (unless (conventions? cs)
        (wrong-syntax conventions-id "expected identifier defined as a conventions"))
      (conventions-rules cs)))
  (syntax-case stx ()
    [conventions
     (identifier? #'conventions)
     (elaborate #'conventions)]
    [_
     (wrong-syntax stx "expected conventions entry")]))

(define (check-conventions-rules stx)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected convention rule list"))
  (map check-conventions-rule (stx->list stx)))

(define (check-conventions-rule stx)
  (define (check-conventions-pattern x blame)
    (cond [(symbol? x) (regexp (string-append "^" (regexp-quote (symbol->string x)) "$"))]
          [(regexp? x) x]
          [else (wrong-syntax blame "expected identifier convention pattern")]))
  (define (check-sc-expr x)
    (syntax-case x ()
      [sc (identifier? #'sc) (list #'sc null)]
      [(sc arg ...) (identifier? #'sc) (list #'sc #'(arg ...))]
      [_ (wrong-syntax x "expected syntax class use")]))
  (syntax-case stx ()
    [(rx sc)
     (list (check-conventions-pattern (syntax-e #'rx) #'rx)
           (check-sc-expr #'sc))]))

;; parse-directive-table
(define parse-directive-table
  (list (list '#:literals check-literals-list)
        (list '#:literal-sets check-literal-sets-list)
        (list '#:conventions check-conventions-list)))

;; rhs-directive-table
(define rhs-directive-table
  (list* (list '#:description values)
         (list '#:transparent)
         (list '#:attributes check-attr-arity-list)
         parse-directive-table))

;; pattern-directive-table
(define pattern-directive-table
  (list (list '#:declare check-id values)
        (list '#:fail-when values values)
        (list '#:fail-unless values values)
        (list '#:with values values)
        (list '#:attr check-attr-arity values)))

;; fail-directive-table
(define fail-directive-table
  (list (list '#:when values)
        (list '#:unless values)))
