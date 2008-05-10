
#lang scheme
(require (for-template "kws.ss")
         (for-template scheme/base)
         scheme/contract
         syntax/boundmap
         syntax/stx
         "util.ss")
(provide (struct-out sc)
         (struct-out attr)
         (struct-out rhs)
         (struct-out rhs:union)
         (struct-out rhs:pattern)
         (struct-out pattern)
         (struct-out pat:id)
         (struct-out pat:datum)
         (struct-out pat:literal)
         (struct-out pat:pair)
         (struct-out pat:splice)
         (struct-out pat:gseq)
         (struct-out splice-pattern)
         (struct-out pat:splice-id)
         (struct-out head)
         (struct-out clause:where)
         (struct-out clause:with)

         get-stxclass
         parse-pattern
         parse-pattern-directives
         flatten-attrs*
         format-symbol)

;; An SC is one of (make-sc symbol (listof symbol) (list-of SAttr) identifier)
(define-struct sc (name inputs attrs parser-name)
  #:property prop:procedure (lambda (self stx) (sc-parser-name self))
  #:transparent)

;; An SSC is one of (make-ssc symbol (listof symbol) (list-of SAttr) identifier)
(define-struct ssc (name inputs attrs parser-name)
  #:transparent)

;; An IAttr is (make-attr identifier number (listof SAttr))
;; An SAttr is (make-attr symbol number (listof SAttr))
(define-struct attr (name depth inner)
  #:transparent)

;; A RHS is one of
;;   (make-rhs:union <RHS> (listof RHS))
;;   (make-rhs:pattern <RHS> Pattern Env Env (listof SideClause))
;; where <RHS> is stx (listof SAttr)
(define-struct rhs (orig-stx attrs) #:transparent)
(define-struct (rhs:union rhs) (rhss) #:transparent)
(define-struct (rhs:pattern rhs) (pattern decls remap wheres) #:transparent)

;; A Pattern is one of
;;   (make-pat:id <Pattern> identifier SC/#f (listof stx))
;;   (make-pat:datum <Pattern> datum)
;;   (make-pat:pair <Pattern> Pattern Pattern)
;;   (make-pat:seq <Pattern> Pattern Pattern)
;;   (make-pat:gseq <Pattern> (listof Head) Pattern)
;; where <Pattern> = stx (listof IAttr) number
(define-struct pattern (orig-stx attrs depth) #:transparent)
(define-struct (pat:id pattern) (name stxclass args) #:transparent)
(define-struct (pat:datum pattern) (datum) #:transparent)
(define-struct (pat:literal pattern) (literal) #:transparent)
(define-struct (pat:pair pattern) (head tail) #:transparent)
(define-struct (pat:splice pattern) (head tail) #:transparent)
(define-struct (pat:gseq pattern) (heads tail) #:transparent)

;; A SplicePattern is one of
;;   (make-pat:splice-id <Pattern> identifier SSC (listof stx))
(define-struct (splice-pattern pattern) ()
  #:transparent)
(define-struct (pat:splice-id splice-pattern) (name stx-splice-class args)
  #:transparent)

;; A Head is
;;   (make-head stx (listof IAttr) nat (listof Pattern) nat/f nat/f boolean id/#f stx/#f)
(define-struct head (orig-stx attrs depth ps min max as-list? occurs default)
  #:transparent)

;; A SideClause is one of
;;   (make-clause:with pattern stx)
;;   (make-clause:where stx)
(define-struct clause:with (pattern expr) #:transparent)
(define-struct clause:where (expr) #:transparent)

;; make-empty-sc : identifier => SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-empty-sc name)
  (make sc (syntax-e name) null null #f))

(define (iattr? a)
  (and (attr? a) (identifier? (attr-name a))))

(define (sattr? a)
  (and (attr? a) (symbol? (attr-name a))))

(provide/contract
 [iattr? (any/c . -> . boolean?)]
 [sattr? (any/c . -> . boolean?)]
 [reorder-iattrs
  ((listof sattr?) (listof iattr?) (identifier? . -> . symbol?) . -> . (listof iattr?))]
 [parse-rhs (syntax? boolean? . -> . rhs?)]
 [parse-splice-rhs (syntax? boolean? . -> . rhs?)]
 [flatten-sattrs
  ([(listof sattr?)] [exact-integer? (or/c symbol? false/c)] . ->* . (listof sattr?))]

#|
 [iattr->sattr (iattr? . -> . sattr?)]
 [rename-attr (attr? symbol? . -> . sattr?)]
 [iattrs->sattrs ((listof iattr?) (identifier? . -> . symbol?) . -> . (listof sattr?))]
 [sattr->iattr/id (sattr? identifier? . -> . iattr?)]
 [atomic-datum? (syntax? . -> . boolean?)]
 [wildcard? (syntax? . -> . boolean?)]
 [dots? (syntax? . -> . boolean?)]
 [append-attrs ((listof (listof iattr?)) syntax? . -> . (listof iattr?))]
 [intersect-attrss ((listof (listof sattr?)) syntax? . -> . (listof sattr?))]
 [join-attrs (sattr? sattr? syntax? . -> . sattr?)]
 [restrict-iattrs
  ((listof sattr?) (listof iattr?) (identifier? . -> . symbol?) . -> . (listof iattr?))]
 [intersect-sattrs ((listof sattr?) (listof sattr?) . -> . (listof sattr?))]
 [lookup-sattr (symbol? (listof sattr?) . -> . (or/c sattr? false/c))]
 [lookup-iattr (identifier? (listof iattr?) . -> . (or/c iattr? false/c))]
|#
 )

(define (iattr->sattr a)
  (match a
    [(struct attr (name depth inner))
     (make attr (syntax-e name) depth inner)]))

(define (rename-attr a name)
  (make attr name (attr-depth a) (attr-inner a)))

(define (iattrs->sattrs as remap)
  (if (pair? as)
      (let ([name* (remap (attr-name (car as)))])
        (if name*
            (cons (rename-attr (car as) name*)
                  (iattrs->sattrs (cdr as) remap))
            (iattrs->sattrs (cdr as) remap)))
      null))

(define (sattr->iattr/id a id)
  (match a
    [(struct attr (name depth inner))
     (make attr (datum->syntax id name id) depth inner)]))

(define (get-stxclass id [blame 'syntax-class])
  (define (no-good)
    (if (allow-unbound-stxclasses)
        (make-empty-sc id)
        (raise-syntax-error blame "not defined as syntax class" id)))
  (let ([sc (syntax-local-value id no-good)])
    (unless (or (sc? sc) (ssc? sc))
      (no-good))
    sc))

(define (split-id/get-stxclass id [decls (lambda _ #f)])
  (cond [(regexp-match #rx"^([^:]*):(.+)$" (symbol->string (syntax-e id)))
         => (lambda (m)
              (when (decls id)
                (raise-syntax-error 'syntax-class
                                    "name already declared with syntax class"
                                    id))
              (let ([sc (get-stxclass (datum->syntax id (string->symbol (caddr m))))])
                (values (datum->syntax id (string->symbol (cadr m)) id id)
                        sc
                        null
                        (ssc? sc))))]
        [(decls id)
         => (lambda (p)
              (let ([stxclass (car p)]
                    [args (cdr p)])
                (unless (equal? (length (sc-inputs stxclass)) (length args))
                  (raise-syntax-error 'syntax-class
                                      "too few arguments for syntax class"
                                      id))
                (values id stxclass args (ssc? stxclass))))]
        [else (values id #f null #f)]))

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

(define allow-unbound-stxclasses (make-parameter #f))

;; parse-rhs : stx(SyntaxClassRHS) boolean -> RHS
;; If allow-unbound? is true, then unbound stxclass acts as if it has no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-rhs stx allow-unbound?)
  (parse-rhs* stx allow-unbound? #f))

;; parse-splice-rhs : stx(SyntaxClassRHS) boolean -> RHS
;; If allow-unbound? is true, then unbound stxclass acts as if it has no attrs.
;; Used for pass1 (attr collection); parser requires stxclasses to be bound.
(define (parse-splice-rhs stx allow-unbound?)
  (parse-rhs* stx allow-unbound? #t))

;; parse-rhs* : stx boolean boolean -> RHS
(define (parse-rhs* stx allow-unbound? splice?)
  (syntax-case stx (pattern union)
    [(pattern p . rest)
     (parameterize ((allow-unbound-stxclasses allow-unbound?))
       (let-values ([(rest decls remap clauses)
                     (parse-pattern-directives #'rest #:sc? #t)])
         (unless (stx-null? rest)
           (raise-syntax-error #f "unexpected terms after pattern directives"
                               (if (pair? rest) (car rest) rest)))
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
           (make rhs:pattern stx sattrs pattern decls remap clauses))))]
    [(union p ...)
     (let* ([rhss (for/list ([rhs (syntax->list #'(p ...))])
                    (parse-rhs* rhs allow-unbound? splice?))]
            [sattrs (intersect-attrss (map rhs-attrs rhss) stx)])
       (make rhs:union stx sattrs rhss))]
    [(id arg ...)
     (identifier? #'id)
     (parse-rhs* (syntax/loc stx (pattern || #:declare || (id arg ...)))
                 allow-unbound?
                 splice?)]
    [id
     (identifier? #'id)
     (parse-rhs* (syntax/loc stx (pattern || #:declare || id))
                 allow-unbound?
                 splice?)]))

;; parse-pattern : stx(Pattern) env number -> Pattern
(define (parse-pattern stx [decls (lambda _ #f)] [depth 0] [allow-splice? #f])
  (syntax-case stx ()
    [dots
     (or (dots? #'dots)
         (gdots? #'dots))
     (raise-syntax-error 'pattern "ellipses not allowed here" stx)]
    [id
     (and (identifier? #'id) (eq? (decls #'id) #t))
     (make pat:literal stx null depth stx)]
    [id
     (identifier? #'id)
     (let-values ([(name sc args splice?) (split-id/get-stxclass #'id decls)])
       (when splice?
         (unless allow-splice?
           (raise-syntax-error 'pattern
                               "splice-pattern not allowed here"
                               stx)))
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
     (let* ([heads (parse-heads #'heads decls (add1 depth))]
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

(define (parse-heads stx decls depth)
  (syntax-case stx ()
    [({} . more)
     (raise-syntax-error 'pattern "empty head sequence not allowed" (stx-car stx))]
    [({p ...} . more)
     (let* ([heads
             (for/list ([p (syntax->list #'(p ...))])
               (parse-pattern p decls depth))]
            [heads-attrs
             (append-attrs (map pattern-attrs heads) (stx-car stx))])
       (parse-heads-k #'more
                      heads
                      heads-attrs
                      depth
                      (lambda (more min max as-list? occurs-pvar default)
                        (let ([occurs-attrs
                               (if occurs-pvar
                                   (list (make-attr occurs-pvar depth null))
                                   null)])
                          (cons (make head (stx-car stx)
                                      (append-attrs (list occurs-attrs heads-attrs)
                                                    (stx-car stx))
                                      depth
                                      heads
                                      min max as-list?
                                      occurs-pvar
                                      default)
                                (parse-heads more decls depth))))))]
    [()
     null]
    [_
     (raise-syntax-error 'pattern "expected sequence of patterns or sequence directive"
                         (if (pair? stx) (car stx) stx))]))

(define (check-nat/f stx)
  (let ([d (syntax-e stx)])
    (unless (nat/f d)
      (raise-syntax-error #f "expected exact nonnegative integer or #f" stx))
    stx))

(define head-directive-table
  (list (list '#:min check-nat/f)
        (list '#:max check-nat/f)
        (list '#:occurs check-id)
        (list '#:default values)
        (list '#:opt)
        (list '#:mand)))

(define (parse-heads-k stx heads heads-attrs heads-depth k)
  (define-values (chunks rest) (chunk-kw-seq stx head-directive-table))
  (reject-duplicate-chunks chunks)
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
      (raise-syntax-error #f
                          "min-constraint must be less than max-constraint"
                          (or min-stx max-stx)))
    (when (and opt-row mand-row)
      (raise-syntax-error #f
                          "opt and mand directives incompatible"
                          (cadr opt-row)))
    (when (and (or min-row max-row) (or opt-row mand-row))
      (raise-syntax-error #f
                          "min/max-constraints incompatible with opt/mand directives"
                          (or min-stx max-stx)))
    (when default-row
      (unless opt-row
        (raise-syntax-error #f
                            "default only allowed for optional patterns"
                            (cadr default-row)))
      (unless (and (pair? head-attrs)
                   (null? (cdr head-attrs))
                   (= heads-depth (attr-depth (car head-attrs)))
                   (null? (attr-inner (car head-attrs))))
        (raise-syntax-error #f
                            "default only allowed for patterns with single simple pattern variable"
                            (cadr default-row))))
    (k rest
       (cond [opt-row 0] [mand-row 1] [else min])
       (cond [opt-row 1] [mand-row 1] [else max])
       (not (or opt-row mand-row))
       (and occurs-row (caddr occurs-row))
       (and default-row (caddr default-row)))))

;; nat/f : any -> boolean
(define (nat/f x)
  (or (not x) (exact-nonnegative-integer? x)))

;; append-attrs : (listof (listof IAttr)) stx -> (listof IAttr)
(define (append-attrs attrss stx)
  (let* ([all (apply append attrss)]
         [names (map attr-name all)]
         [dup (check-duplicate-identifier names)])
    (when dup
      (raise-syntax-error 'syntax-class "duplicate pattern variable" stx dup))
    all))

;; parse-pattern-directives : stxs(PatternDirective) #:literals (listof id)
;;                         -> stx DeclEnv env (listof SideClause)
;; DeclEnv = bound-id-mapping[id => (cons SC (listof stx)) or #t]
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

    (define (check-in-sc stx)
      (unless sc?
        (raise-syntax-error 'pattern
                            "not within syntax-class definition"
                            (if (pair? stx) (car stx) stx))))
    (define directive-table
      (list (list '#:declare check-id values)
            (list '#:rename check-id check-id)
            (list '#:with values values)
            (list '#:where values)))
    (define-values (chunks rest) (chunk-kw-seq stx directive-table))
    (define directives (map cdr chunks))

    (define (for-decl stx)
      (syntax-case stx ()
        [[#:declare name sc]
         (identifier? #'sc)
         (for-decl #'[#:declare name (sc)])]
        [[#:declare name (sc expr ...)]
         (begin (let ([prev (bound-identifier-mapping-get decl-table #'name (lambda () #f))])
                  (when (pair? prev)
                    (raise-syntax-error 'pattern
                                        "duplicate syntax-class declaration for name"
                                        #'name))
                  (when prev
                    (raise-syntax-error 'pattern
                                        "name already declared as literal"
                                        #'name)))
                (bound-identifier-mapping-put! decl-table #'name
                                               (cons (get-stxclass #'sc)
                                                     (syntax->list #'(expr ...)))))]
        [[#:declare . _]
         (raise-syntax-error 'pattern "bad #:declare form" stx)]
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
        [[#:where expr]
         (set! rclauses
               (cons (make clause:where #'expr) rclauses))]
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
    (raise-syntax-error 'define-syntax-splice-pattern
                        "not a proper list pattern"
                        stx))
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

;; intersect-attrss : (listof (listof SAttr)) stx -> (listof SAttr)
(define (intersect-attrss attrss blamestx)
  (cond [(null? attrss) null]
        [else
         (let* ([namess (map (lambda (attrs) (map attr-name attrs)) attrss)]
                [names (filter (lambda (s)
                                 (andmap (lambda (names) (memq s names))
                                         (cdr namess)))
                               (car namess))]
                [ht (make-hasheq)]
                [put (lambda (attr) (hash-set! ht (attr-name attr) attr))]
                [fetch-like (lambda (attr) (hash-ref ht (attr-name attr) #f))])
           (for* ([attrs attrss]
                  [attr attrs]
                  #:when (memq (attr-name attr) names))
             (put (join-attrs attr (fetch-like attr) blamestx)))
           (sort (hash-map ht (lambda (k v) v))
                 (lambda (a b)
                   (string<? (symbol->string (attr-name a))
                             (symbol->string (attr-name b))))))]))

;; join-attrs : SAttr SAttr stx -> SAttr
(define (join-attrs a b blamestx)
  (define (complain str . args)
    (raise-syntax-error 'syntax-class (apply format str args) blamestx))
  (if (not b)
      a
      (begin
        (unless (equal? (attr-depth a) (attr-depth b))
          (complain "attribute '~a'occurs with different nesting depth" (attr-name a)))
        (make attr (attr-name a)
                   (attr-depth a)
                   (intersect-attrss (list (attr-inner a) (attr-inner b)) blamestx)))))

;; reorder-iattrs : (listof SAttr) (listof IAttr) env -> (listof IAttr)
;; Reorders iattrs (and restricts) based on relsattrs
(define (reorder-iattrs relsattrs iattrs remap)
  (let ([ht (make-hasheq)])
    (for-each (lambda (iattr)
                (let ([remap-name (remap (attr-name iattr))])
                  (hash-set! ht remap-name iattr)))
              iattrs)
    (let loop ([relsattrs relsattrs])
      (match relsattrs
        ['() null]
        [(cons (struct attr (name depth inner)) rest)
         (let ([iattr (hash-ref ht name #f)])
           (if iattr
               (cons (make attr (attr-name iattr)
                                (attr-depth iattr)
                                (intersect-sattrs inner (attr-inner iattr)))
                     (loop rest))
               (loop rest)))]))))

;; restrict-iattrs : (listof SAttr) (listof IAttr) env -> (listof IAttr)
;; Preserves order of iattrs
(define (restrict-iattrs relsattrs iattrs remap)
  (match iattrs
    ['() null]
    [(cons (struct attr (name depth inner)) rest)
     (let ([sattr (lookup-sattr (remap name) relsattrs)])
       (if (and sattr (= depth (attr-depth sattr)))
           (cons (make attr name depth
                            (intersect-sattrs inner (attr-inner sattr)))
                 (restrict-iattrs relsattrs (cdr iattrs) remap))
           (restrict-iattrs relsattrs (cdr iattrs) remap)))]))

;; flatten-sattrs : (listof SAttr) num symbol -> (listof SAttr)
(define (flatten-sattrs sattrs [depth-delta 0] [prefix #f])
  (match sattrs
    ['()
     null]
    [(cons (struct attr (name depth nested)) rest)
     (let ([prefixed-name
            (if prefix
                (format-symbol "~a.~a" prefix name)
                name)])
       (append (list (make attr prefixed-name
                                (+ depth-delta depth)
                                null))
               (flatten-sattrs nested (+ depth depth-delta) prefixed-name)
               (flatten-sattrs rest depth-delta prefix)))]))

;; intersect-sattrs : (listof SAttr) (listof SAttr) -> (listof SAttr)
;; Preserves order of first list of attrs.
(define (intersect-sattrs as bs)
  (match as
    ['() null]
    [(cons (struct attr (name depth inner)) rest)
     (let ([b (lookup-sattr name bs)])
       (if (and b (= depth (attr-depth b)))
           (cons (make attr name depth (intersect-sattrs inner (attr-inner b)))
                 (intersect-sattrs (cdr as) bs))
           (intersect-sattrs (cdr as) bs)))]))

;; flatten-attrs* : (listof attr) num symbol stx -> (listof attr)
(define (flatten-attrs* attrs [depth-delta 0] [prefix #f] [ctx #f])
  (match attrs
    ['()
     null]
    [(cons (struct attr (name depth nested)) rest)
     (let ([prefixed-name
            (if prefix
                (format-symbol "~a.~a" prefix name)
                (syntax-e name))]
           [ctx (or ctx name)])
       (append (list (make attr (if ctx (datum->syntax ctx prefixed-name) name)
                                (+ depth-delta depth)
                                null))
               (flatten-attrs* nested (+ depth depth-delta) prefixed-name ctx)
               (flatten-attrs* rest depth-delta prefix ctx)))]))

(define (format-symbol fmt . args)
  (string->symbol (apply format fmt args)))

(define (lookup-sattr name sattrs)
  (cond [(null? sattrs) #f]
        [(eq? name (attr-name (car sattrs))) (car sattrs)]
        [else (lookup-sattr name (cdr sattrs))]))

(define (lookup-iattr name iattrs)
  (cond [(null? iattrs) #f]
        [(bound-identifier=? name (attr-name (car iattrs))) (car iattrs)]
        [else (lookup-iattr name (cdr iattrs))]))
