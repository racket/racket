#lang scheme
(require (for-template "kws.ss")
         (for-template scheme/base)
         scheme/contract
         syntax/boundmap
         syntax/stx
         "../util.ss"
         "rep-data.ss")
(provide/contract
 [parse-pattern 
  (->* [any/c #|syntax?|# DeclEnv/c exact-nonnegative-integer?]
       [boolean?]
       pattern?)]
 [parse-pattern-directives
  (->* [stx-list?]
       [#:sc? boolean? #:literals (listof identifier?)]
       (values stx-list? DeclEnv/c RemapEnv/c (listof SideClause/c)))]
 [parse-rhs (syntax? boolean? syntax? . -> . rhs?)]
 [parse-splice-rhs (syntax? boolean? syntax? . -> . rhs?)])


(define (atomic-datum? stx)
  (let ([datum (syntax-e stx)])
    (or (null? datum)
        (boolean? datum)
        (string? datum)
        (number? datum)
        (keyword? datum))))

(define (wildcard? stx)
  (and (identifier? stx)
       (or (free-identifier=? stx (quote-syntax _)))))

(define (epsilon? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ||))))

(define (dots? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ...))))

(define (gdots? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ...*))))

;; ---

;; parse-rhs : stx(SyntaxClassRHS) boolean stx -> RHS
;; If allow-unbound? is true, then unbound stxclass acts as if it has no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-rhs stx allow-unbound? ctx)
  (parse-rhs* stx allow-unbound? #f ctx))

;; parse-splice-rhs : stx(SyntaxClassRHS) boolean stx -> RHS
;; If allow-unbound? is true, then unbound stxclass acts as if it has no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-splice-rhs stx allow-unbound? ctx)
  (parse-rhs* stx allow-unbound? #t ctx))

;; parse-rhs* : stx boolean boolean stx -> RHS
(define (parse-rhs* stx allow-unbound? splice? ctx)
  (define-values (chunks rest)
    (chunk-kw-seq stx rhs-directive-table #:context ctx))
  (define lits0 (assq '#:literals chunks))
  (define desc0 (assq '#:description chunks))
  (define trans0 (assq '#:transparent chunks))
  (define literals (if lits0 (caddr lits0) null))
  (define description (and desc0 (caddr desc0)))
  (define transparent? (and trans0 #t))

  (define (parse-rhs*-basic rest)
    (syntax-case rest (basic-syntax-class)
      [((basic-syntax-class (attr-decl ...) parser-expr))
       (make rhs:basic ctx 
             (for/list ([attr-stx (syntax->list #'(attr-decl ...))])
               (syntax-case attr-stx ()
                 [(attr depth)
                  (begin
                    (unless (and (identifier? #'attr)
                                 (exact-nonnegative-integer? (syntax-e #'depth)))
                      (wrong-syntax attr-stx "bad attribute declaration"))
                    (make-attr (syntax-e #'attr) (syntax-e #'depth) null))]
                 [_
                  (wrong-syntax attr-stx "bad attribute declaration")]))
             transparent?
             description
             #'parser-expr)]))

  (define (parse-rhs*-patterns rest)
    (define (gather-patterns stx)
      (syntax-case stx (pattern)
        [((pattern . _) . rest)
         (cons (parse-rhs-pattern (stx-car stx) allow-unbound? splice? literals)
               (gather-patterns #'rest))]
        [()
         null]))
    (define patterns (gather-patterns rest))
    (when (null? patterns)
      (wrong-syntax ctx "syntax class has no variants"))
    (let ([sattrs (intersect-attrss (map rhs:pattern-attrs patterns) ctx)])
      (make rhs:union stx sattrs 
            transparent?
            description
            patterns)))

  (syntax-case rest (pattern basic-syntax-class)
    [((basic-syntax-class . _))
     (parse-rhs*-basic rest)]
    [_
     (parse-rhs*-patterns rest)]))

;; parse-rhs-pattern : stx boolean boolean (listof identifier) -> RHS
(define (parse-rhs-pattern stx allow-unbound? splice? literals)
  (syntax-case stx (pattern)
    [(pattern p . rest)
     (parameterize ((allow-unbound-stxclasses allow-unbound?))
       (let-values ([(rest decls remap clauses)
                     (parse-pattern-directives #'rest
                                               #:literals literals
                                               #:sc? #t)])
         (unless (stx-null? rest)
           (wrong-syntax (if (pair? rest) (car rest) rest)
                         "unexpected terms after pattern directives"))
         (let* ([pattern (parse-pattern #'p decls 0)]
                [_ (when splice?
                     (check-proper-list-pattern pattern))]
                [with-patterns
                 (for/list ([c clauses] #:when (clause:with? c))
                   (clause:with-pattern c))]
                [attrs (append-attrs
                        (cons (pattern-attrs pattern)
                              (map pattern-attrs with-patterns))
                        stx)]
                [sattrs (iattrs->sattrs attrs remap)])
           (make rhs:pattern stx sattrs pattern decls remap clauses))))]))

;; rhs-directive-table
(define rhs-directive-table
  (list (list '#:literals check-idlist)
        (list '#:description values)
        (list '#:transparent)))

;; parse-pattern : stx(Pattern) env number -> Pattern
(define (parse-pattern stx decls depth [allow-splice? #f])
  (syntax-case stx ()
    [dots
     (or (dots? #'dots)
         (gdots? #'dots))
     (wrong-syntax stx "ellipses not allowed here")]
    [id
     (and (identifier? #'id) (eq? (decls #'id) #t))
     (make pat:literal stx null depth stx)]
    [id
     (identifier? #'id)
     (let-values ([(name sc args splice?) (split-id/get-stxclass #'id decls)])
       (when splice?
         (unless allow-splice?
           (wrong-syntax stx "splice-pattern not allowed here")))
       (let ([attrs
              (cond [(wildcard? name) null]
                    [(and (epsilon? name) sc)
                     (map (lambda (a)
                            (make attr (datum->syntax #'id (attr-name a))
                                       (+ depth (attr-depth a))
                                       (attr-inner a)))
                          (sc-attrs sc))]
                    [else
                     (list (make attr name depth (if sc (sc-attrs sc) null)))])]
             [name (if (epsilon? name) #f name)])
         (if splice?
             (make pat:splice-id stx attrs depth name sc args)
             (make pat:id stx attrs depth name sc args))))]
    [datum
     (atomic-datum? #'datum)
     (make pat:datum stx null depth (syntax->datum #'datum))]
    [(heads gdots . tail)
     (gdots? #'gdots)
     (let* ([heads (parse-heads #'heads decls depth)]
            [tail (parse-pattern #'tail decls depth)]
            [hattrs (append-attrs (for/list ([head heads]) (head-attrs head)) stx)]
            [tattrs (pattern-attrs tail)])
       (make pat:gseq stx (append-attrs (list hattrs tattrs) stx) depth heads tail))]
    [(head dots . tail)
     (dots? #'dots)
     (let* ([headp (parse-pattern #'head decls (add1 depth))]
            [tail (parse-pattern #'tail decls depth)]
            [head (pattern->head headp)]
            [attrs (append-attrs (list (head-attrs head) (pattern-attrs tail)) stx)])
       (make pat:gseq stx attrs depth (list head) tail))]
    [(a . b)
     (let ([pa (parse-pattern #'a decls depth #t)]
           [pb (parse-pattern #'b decls depth)])
       (let ([attrs (append-attrs (list (pattern-attrs pa) (pattern-attrs pb)) stx)])
         (if (splice-pattern? pa)
             (make pat:splice stx attrs depth pa pb)
             (make pat:pair stx attrs depth pa pb))))]))

(define (pattern->head p)
  (match p
    [(struct pattern (orig-stx iattrs depth))
     (make head orig-stx iattrs depth (list p) #f #f #t #f #f)]))

(define head-directive-table
  (list (list '#:min check-nat/f)
        (list '#:max check-nat/f)
        (list '#:occurs check-id)
        (list '#:default values)
        (list '#:opt)
        (list '#:mand)))

(define (parse-heads stx decls enclosing-depth)
  (syntax-case stx ()
    [({} . more)
     (wrong-syntax (stx-car stx)
                   "empty head sequence not allowed")]
    [({p ...} . more)
     (let-values ([(chunks rest) (chunk-kw-seq/no-dups #'more head-directive-table)])
       (reject-duplicate-chunks chunks) ;; FIXME: needed?
       (cons (parse-head/chunks (stx-car stx) decls enclosing-depth chunks)
             (parse-heads rest decls enclosing-depth)))]
    [()
     null]
    [_
     (wrong-syntax (cond [(pair? stx) (car stx)]
                         [(syntax? stx) stx]
                         [else #f])
                   "expected sequence of patterns or sequence directive")]))

(define (parse-head/chunks pstx decls enclosing-depth chunks)
  (let* ([min-row (assq '#:min chunks)]
         [max-row (assq '#:max chunks)]
         [occurs-row (assq '#:occurs chunks)]
         [default-row (assq '#:default chunks)]
         [opt-row (assq '#:opt chunks)]
         [mand-row (assq '#:mand chunks)]
         [min-stx (and min-row (caddr min-row))]
         [max-stx (and max-row (caddr max-row))]
         [min (if min-stx (syntax-e min-stx) #f)]
         [max (if max-stx (syntax-e max-stx) #f)])
    (unless (<= (or min 0) (or max +inf.0))
      (wrong-syntax (or min-stx max-stx)
                    "min-constraint must be less than max-constraint"))
    (when (and opt-row mand-row)
      (wrong-syntax (cadr opt-row)
                    "opt and mand directives are incompatible"))
    (when (and (or min-row max-row) (or opt-row mand-row))
      (wrong-syntax (or min-stx max-stx)
                    "min/max-constraints are incompatible with opt/mand directives"))
    (when default-row
      (unless opt-row
        (wrong-syntax (cadr default-row)
                      "default only allowed for optional patterns")))
    (parse-head/options pstx
                        decls
                        enclosing-depth
                        (cond [opt-row 0] [mand-row 1] [else min])
                        (cond [opt-row 1] [mand-row 1] [else max])
                        (not (or opt-row mand-row))
                        (and occurs-row (caddr occurs-row))
                        default-row)))

(define (parse-head/options pstx decls enclosing-depth 
                            min max as-list? occurs-pvar default-row)
  (let* ([depth (if as-list? (add1 enclosing-depth) enclosing-depth)]
         [heads
          (for/list ([p (syntax->list pstx)])
            (parse-pattern p decls depth))]
         [heads-attrs
          (append-attrs (map pattern-attrs heads) pstx)])
    (when default-row
      (unless (and (= (length heads-attrs) 1)
                   (= enclosing-depth (attr-depth (car heads-attrs)))
                   (null? (attr-inner (car heads-attrs))))
        (wrong-syntax (cadr default-row)
                      "default only allowed for patterns with single simple pattern variable")))
    (let ([occurs-attrs
           (if occurs-pvar
               (list (make-attr occurs-pvar depth null))
               null)])
      (make head pstx
            (append-attrs (list occurs-attrs heads-attrs) pstx)
            depth
            heads
            min max as-list?
            occurs-pvar
            (and default-row (caddr default-row))))))

;; parse-pattern-directives : stxs(PatternDirective) #:literals (listof id)
;;                         -> stx DeclEnv env (listof SideClause)
;;   if decls maps a name to #t, it indicates literal
(define (parse-pattern-directives stx
                                  #:sc? [sc? #f]
                                  #:literals [literals null])
  (let ([decl-table (make-bound-identifier-mapping)]
        [remap-table (make-bound-identifier-mapping)]
        [rclauses null])

    (define (decls id)
      (bound-identifier-mapping-get decl-table id (lambda () #f)))
    (define (remap id)
      (bound-identifier-mapping-get remap-table id (lambda () (syntax-e id))))
    (define (decls-add! id value)
      (bound-identifier-mapping-put! decl-table id value))

    (define (check-in-sc stx)
      (unless sc?
        (wrong-syntax (if (pair? stx) (car stx) stx)
                      "not within syntax-class definition")))
    (define directive-table
      (list (list '#:declare check-id values)
            (list '#:rename check-id check-id)
            (list '#:with values values)
            (list '#:when values)))
    (define-values (chunks rest) (chunk-kw-seq stx directive-table))
    (define directives (map cdr chunks))

    (define (for-decl stx)
      (syntax-case stx ()
        [[#:declare name sc]
         (identifier? #'sc)
         (for-decl #'[#:declare name (sc)])]
        [[#:declare name (sc expr ...)]
         (begin
           (let ([prev (decls #'name)])
             (when (pair? prev)
               (wrong-syntax #'name
                             "duplicate syntax-class declaration for name"))
             (when prev
               (wrong-syntax #'name
                             "name already declared as literal")))
           (decls-add! #'name
                       (list* #'name #'sc (syntax->list #'(expr ...)))))]
        [[#:declare . _]
         (wrong-syntax stx "bad #:declare form")]
        [[#:rename id s]
         (begin (check-in-sc stx)
                (bound-identifier-mapping-put! remap-table #'id
                                               (if (wildcard? #'s)
                                                   #f
                                                   (syntax-e #'s))))]
        [_ (void)]))
    (define (for-side stx)
      (syntax-case stx ()
        [[#:with p expr]
         (let* ([pattern (parse-pattern #'p decls 0)])
           (set! rclauses
                 (cons (make clause:with pattern #'expr) rclauses)))]
        [[#:when expr]
         (set! rclauses
               (cons (make clause:when #'expr) rclauses))]
        [_ (void)]))

    (for ([literal literals])
      (bound-identifier-mapping-put! decl-table literal #t))

    (for-each for-decl directives)
    (for-each for-side directives)

    (values rest
            decls
            remap
            (reverse rclauses))))

;; check-proper-list-pattern : Pattern -> void
(define (check-proper-list-pattern p)
  (define (err stx)
    (wrong-syntax stx "not a proper list pattern"))
  (match p
    [(struct pat:id (orig-stx _ _ _ _ _))
     (err orig-stx)]
    [(struct pat:datum (orig-stx _ _ datum))
     (unless (null? datum)
       (err orig-stx))]
    [(struct pat:pair (_ _ _ head tail))
     (check-proper-list-pattern tail)]
    [(struct pat:splice (_ _ _ head tail))
     (check-proper-list-pattern tail)]
    [(struct pat:gseq (_ _ _ heads tail))
     (check-proper-list-pattern tail)]))
