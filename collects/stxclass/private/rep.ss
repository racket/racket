#lang scheme/base
(require (for-template scheme/base)
         (for-template "runtime.ss")
         scheme/contract
         scheme/match
         syntax/boundmap
         syntax/stx
         "../util.ss"
         "rep-data.ss"
         "codegen-data.ss")

(provide/contract
 [parse-whole-pattern 
  (-> syntax? DeclEnv/c
      pattern?)]
 [parse-pattern-directives
  (->* [stx-list?]
       [#:sc? boolean? #:literals (listof (list/c identifier? identifier?))]
       (values stx-list? DeclEnv/c RemapEnv/c (listof SideClause/c)))]
 [parse-rhs
  (-> syntax? boolean? syntax?
      rhs?)]
 [check-literals-list
  (-> syntax?
      (listof (list/c identifier? identifier?)))]
 [pairK kind?]
 [vectorK kind?]
 [boxK kind?])

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

(define (and-kw? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ~and))))

(define (orseq-kw? stx)
  (and (identifier? stx)
       (free-identifier=? stx (quote-syntax ~or))))

(define (reserved? stx)
  (or (dots? stx)
      (gdots? stx)
      (and-kw? stx)
      (orseq-kw? stx)))

;; ---- Kinds ----

(define pairK
  (make-kind #'pair?
             (list (lambda (s d) #`(car #,d))
                   (lambda (s d) #`(datum->syntax #,s (cdr #,d) #,s)))
             (list (lambda (fc x) (frontier:add-car fc x))
                   (lambda (fc x) (frontier:add-cdr fc)))))

(define vectorK
  (make-kind #'vector?
             (list (lambda (s d)
                     #`(datum->syntax #,s (vector->list #,d) #,s)))
             (list (lambda (fc x) (frontier:add-unvector fc)))))

(define boxK
  (make-kind #'box?
             (list (lambda (s d) #`(unbox #,d)))
             (list (lambda (fc x) (frontier:add-unbox fc)))))

;; ---

;; parse-rhs : stx(SyntaxClassRHS) boolean stx -> RHS
;; If allow-unbound? is true, then unbound stxclass acts as if it has no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-rhs stx allow-unbound? ctx)
  (define-values (chunks rest)
    (chunk-kw-seq stx rhs-directive-table #:context ctx))
  (define lits0 (assq '#:literals chunks))
  (define desc0 (assq '#:description chunks))
  (define trans0 (assq '#:transparent chunks))
  (define attrs0 (assq '#:attributes chunks))
  (define literals (if lits0 (caddr lits0) null))
  (define description (and desc0 (caddr desc0)))
  (define transparent? (and trans0 #t))
  (define attributes (and attrs0 (caddr attrs0)))

  (define (parse-rhs*-patterns rest)
    (define (gather-patterns stx)
      (syntax-case stx (pattern)
        [((pattern . _) . rest)
         (cons (parse-rhs-pattern (stx-car stx) allow-unbound? literals)
               (gather-patterns #'rest))]
        [()
         null]))
    (define patterns (gather-patterns rest))
    (when (null? patterns)
      (wrong-syntax ctx "expected at least one variant"))
    (let ([sattrs
           (or attributes
               (intersect-attrss (map rhs:pattern-attrs patterns) ctx))])
      (make rhs:union stx sattrs 
            transparent?
            description
            patterns)))

  (parse-rhs*-patterns rest))

;; parse-rhs-pattern : stx boolean boolean (listof id+id) -> RHS
(define (parse-rhs-pattern stx allow-unbound? literals)
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
         (let* ([pattern (parse-whole-pattern #'p decls)]
                [with-patterns
                 (for/list ([c clauses] #:when (clause:with? c))
                   (clause:with-pattern c))]
                [attrs (append-attrs
                        (cons (pattern-attrs pattern)
                              (map pattern-attrs with-patterns)))]
                [sattrs (iattrs->sattrs attrs remap)])
           (make rhs:pattern stx sattrs pattern decls remap clauses))))]))

;; parse-whole-pattern : stx DeclEnv -> Pattern
(define (parse-whole-pattern stx decls)
  (define pattern (parse-pattern stx decls 0))
  (define pvars (map attr-name (pattern-attrs pattern)))
  (define excess-domain (declenv-domain-difference decls pvars))
  (when (pair? excess-domain)
    (wrong-syntax #f "declared pattern variables do not appear in pattern"
                  #:extra excess-domain))
  pattern)

;; parse-pattern : stx(Pattern) DeclEnv number -> Pattern
(define (parse-pattern stx decls depth
                       #:allow-orseq-pattern? [allow-orseq-pattern? #f])
  (syntax-case stx (~and ~or)
    [gdots
     (gdots? #'gdots)
     (wrong-syntax stx "obsolete (...*) sequence syntax")]
    [reserved
     (reserved? #'reserved)
     (wrong-syntax #'reserved "not allowed here")]
    [id
     (identifier? #'id)
     (match (declenv-lookup decls #'id)
       [(list 'literal internal-id literal-id)
        (make pat:literal stx null depth literal-id)]
       [(list 'stxclass declared-id scname args)
        (let* ([sc (get-stxclass/check-arg-count scname (length args))]
               [attrs (id-pattern-attrs #'id sc depth)])
          (make pat:id stx attrs depth #'id sc args))]
       [#f
        (let-values ([(name sc args) (split-id/get-stxclass #'id decls)])
          (let ([attrs (id-pattern-attrs name sc depth)]
                [name (if (epsilon? name) #f name)])
            (make pat:id stx attrs depth name sc args)))])]
    [datum
     (atomic-datum? #'datum)
     (make pat:datum stx null depth (syntax->datum #'datum))]
    [(~and . rest)
     (begin (unless (stx-list? #'rest)
              (wrong-syntax stx "expected list of patterns"))
            (parse-and-pattern stx decls depth))]
    [(~or . heads)
     (begin (unless (stx-list? #'heads)
              (wrong-syntax stx "expected list of pattern sequences"))
            (unless allow-orseq-pattern?
              (wrong-syntax stx "or/sequence pattern not allowed here"))
            (let* ([heads (parse-heads #'heads decls depth)]
                   [attrs
                    (append-attrs
                     (for/list ([head heads]) (head-attrs head)))])
              (make pat:orseq stx attrs depth heads)))]
    [(head dots . tail)
     (dots? #'dots)
     (let* ([headp (parse-pattern #'head decls (add1 depth)
                                  #:allow-orseq-pattern? #t)]
            [heads
             (if (pat:orseq? headp)
                 (pat:orseq-heads headp)
                 (list (pattern->head headp)))]
            [tail (parse-pattern #'tail decls depth)]
            [hattrs (pattern-attrs headp)]
            [tattrs (pattern-attrs tail)])
       (make pat:gseq stx (append-attrs (list hattrs tattrs))
             depth heads tail))]
    [(a . b)
     (let ([pa (parse-pattern #'a decls depth)]
           [pb (parse-pattern #'b decls depth)])
       (define attrs
         (append-attrs (list (pattern-attrs pa) (pattern-attrs pb))))
       (make pat:compound stx attrs depth pairK (list pa pb))
       #| (make pat:pair stx attrs depth pa pb) |#)]
    [#(a ...)
     (let ([lp (parse-pattern (syntax/loc stx (a ...)) decls depth)])
       (make pat:compound stx (pattern-attrs lp) depth vectorK (list lp)))]
    [#&x
     (let ([bp (parse-pattern #'x decls depth)])
       (make pat:compound stx (pattern-attrs bp) depth boxK (list bp)))]))

(define (id-pattern-attrs name sc depth)
  (cond [(wildcard? name) null]
        [(and (epsilon? name) sc)
         (for/list ([a (sc-attrs sc)])
           (make attr (datum->syntax name (attr-name a))
                 (+ depth (attr-depth a))
                 (attr-inner a)))]
        [sc
         (list (make attr name depth (sc-attrs sc)))]
        [else
         (list (make attr name depth null))]))

;; parse-and-patttern : stxlist DeclEnv nat -> Pattern
(define (parse-and-pattern stx decls depth)
  (define-values (chunks rest)
    (chunk-kw-seq/no-dups (stx-cdr stx) and-pattern-directive-table))
  (define description
    (cond [(assq '#:description chunks) => caddr]
          [else #f]))
  (define patterns
    (for/list ([x (stx->list rest)])
      (parse-pattern x decls depth)))
  (define attrs (append-attrs (map pattern-attrs patterns)))
  (make pat:and stx attrs depth description patterns))

(define (pattern->head p)
  (match p
    [(struct pattern (ostx iattrs depth))
     (make head ostx iattrs depth (list p) #f #f #t)]))

(define (parse-heads stx decls enclosing-depth)
  (syntax-case stx ()
    [({} . more)
     (wrong-syntax (stx-car stx)
                   "empty head sequence not allowed")]
    [({p ...} . more)
     (let()
       (define-values (chunks rest)
         (chunk-kw-seq/no-dups #'more head-directive-table))
       (define-values (chunks2 rest2)
         (chunk-kw-seq rest head-directive-table2))
       ;; FIXME FIXME: handle chunks2 !!!!
       (cons (parse-head/chunks (stx-car stx) decls enclosing-depth chunks)
             (parse-heads rest2 decls enclosing-depth)))]
    [()
     null]
    [_
     (wrong-syntax (cond [(pair? stx) (car stx)]
                         [(syntax? stx) stx]
                         [else #f])
                   "expected sequence of patterns or sequence directive")]))

(define (parse-head/chunks pstx decls depth chunks)
  (let* ([min-row (assq '#:min chunks)]
         [max-row (assq '#:max chunks)]
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
    (parse-head/options pstx
                        decls
                        depth
                        (cond [opt-row 0] [mand-row 1] [else min])
                        (cond [opt-row 1] [mand-row 1] [else max])
                        (not (or opt-row mand-row)))))

(define (parse-head/options pstx decls depth min max as-list?)
  (let* ([effective-depth (if as-list? depth (sub1 depth))]
         [heads
          (for/list ([p (stx->list pstx)])
            (parse-pattern p decls effective-depth))]
         [heads-attrs
          (append-attrs (map pattern-attrs heads))])
    (make head pstx
          heads-attrs
          depth
          heads
          min max as-list?)))

;; parse-pattern-directives : stxs(PatternDirective) #:literals (listof id+id)
;;                         -> stx DeclEnv RemapEnv (listof SideClause)
(define (parse-pattern-directives stx
                                  #:sc? [sc? #f]
                                  #:literals [literals null])
  (define remap (new-remapenv))
  (define-values (chunks rest)
    (chunk-kw-seq stx pattern-directive-table))
  (define (process-renames chunks)
    (match chunks
      [(cons (list '#:rename rename-stx internal-id sym-id) rest)
       (unless sc?
         (wrong-syntax rename-stx
                       "only allowed within syntax-class definition"))
       (remapenv-put remap internal-id (syntax-e sym-id))
       (process-renames rest)]
      [(cons decl rest)
       (cons decl (process-renames rest))]
      ['()
       '()]))
  (define chunks2 (process-renames chunks))
  (define-values (decls chunks3)
    (grab-decls chunks2 literals))
  (values rest decls remap
          (parse-pattern-sides chunks3 literals)))

;; grab-decls : (listof chunk) (listof id+id)
;;           -> (values DeclEnv/c (listof chunk))
(define (grab-decls chunks literals)
  (define decls (new-declenv literals))
  (define (loop chunks)
    (match chunks
      [(cons (cons '#:declare decl-stx) rest)
       (add-decl decl-stx)
       (loop rest)]
      [else chunks]))
  (define (add-decl stx)
    (syntax-case stx ()
      [(#:declare name sc)
       (identifier? #'sc)
       (add-decl #'(#:declare name (sc)))]
      [(#:declare name (sc expr ...))
       (declenv-put-stxclass decls #'name #'sc (syntax->list #'(expr ...)))]
      [(#:declare name bad-sc)
       (wrong-syntax #'bad-sc
                     "expected syntax class name (possibly with parameters)")]))
  (let ([rest (loop chunks)])
    (values decls rest)))

;; parse-pattern-sides : (listof chunk) (listof id+id)
;;                    -> (listof SideClause/c)
(define (parse-pattern-sides chunks literals)
  (match chunks
    [(cons (list '#:declare declare-stx _ _) rest)
     (wrong-syntax declare-stx
                   "#:declare can only follow pattern or #:with clause")]
    [(cons (list '#:when when-stx expr) rest)
     (cons (make clause:when expr)
           (parse-pattern-sides rest literals))]
    [(cons (list '#:with with-stx pattern expr) rest)
     (let-values ([(decls rest) (grab-decls rest literals)])
       (cons (make clause:with (parse-whole-pattern pattern decls) expr)
             (parse-pattern-sides rest literals)))]
    ['()
     '()]))


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
    (iattrs->sattrs (append-attrs (map list iattrs)) trivial-remap)))

;; check-attr-arity : stx -> IAttr
(define (check-attr-arity stx)
  (syntax-case stx ()
    [attr
     (identifier? #'attr)
     (make-attr #'attr 0 null)]
    [(attr depth)
     (check-attr-arity #'(attr depth ()))]
    [(attr depth inners)
     (begin (unless (identifier? #'attr)
              (wrong-syntax #'attr "expected attribute name"))
            (unless (exact-nonnegative-integer? (syntax-e #'depth))
              (wrong-syntax #'depth "expected depth (nonnegative integer)"))
            (make-attr #'attr (syntax-e #'depth) (check-attr-arity-list #'inners)))]
    [_
     (wrong-syntax stx "expected attribute arity declaration")]))


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

;; rhs-directive-table
(define rhs-directive-table
  (list (list '#:literals check-literals-list)
        (list '#:description values)
        (list '#:transparent)
        (list '#:attributes check-attr-arity-list)))

;; pattern-directive-table
(define pattern-directive-table
  (list (list '#:declare check-id values)
        (list '#:rename check-id check-id)
        (list '#:with values values)
        (list '#:when values)))

;; and-pattern-directive-table
(define and-pattern-directive-table
  (list (list '#:description check-lit-string)))

(define head-directive-table
  (list (list '#:min check-nat/f)
        (list '#:max check-nat/f)
        (list '#:opt)
        (list '#:mand)))

(define head-directive-table2
  (list (list '#:with values values)
        (list '#:declare check-id values)))
