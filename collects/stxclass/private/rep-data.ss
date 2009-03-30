#lang scheme/base
(require scheme/contract
         scheme/match
         syntax/stx
         syntax/boundmap
         "../util.ss")
(provide (struct-out sc)
         (struct-out attr)
         (struct-out rhs)
         (struct-out rhs:union)
         (struct-out rhs:pattern)
         (struct-out pattern)
         (struct-out pat:id)
         (struct-out pat:datum)
         (struct-out pat:literal)
         (struct-out pat:compound)
         (struct-out pat:gseq)
         (struct-out pat:and)
         (struct-out pat:orseq)
         (struct-out kind)
         (struct-out head)
         (struct-out clause:when)
         (struct-out clause:with))

;; An SC is one of (make-sc symbol (listof symbol) (list-of SAttr) identifier)
(define-struct sc (name inputs attrs parser-name description)
  #:property prop:procedure (lambda (self stx) (sc-parser-name self))
  #:transparent)

;; An IAttr is (make-attr identifier number (listof SAttr))
;; An SAttr is (make-attr symbol number (listof SAttr))
(define-struct attr (name depth inner)
  #:transparent)

;; RHSBase is stx (listof SAttr) boolean stx/#f
(define-struct rhs (ostx attrs transparent? description)
  #:transparent)

;; A RHS is one of
;;   (make-rhs:union <RHSBase> (listof RHS))
(define-struct (rhs:union rhs) (patterns)
  #:transparent)

;; An RHSPattern is
;;   (make-rhs:pattern stx (listof SAttr) Pattern Env Env (listof SideClause))
(define-struct rhs:pattern (stx attrs pattern decls remap sides)
  #:transparent)

#|

NOT YET ...

;; A Pattern is
;;   (make-pattern (listof IAttr) PCtx (listof id) string/#f Descriminator)
(define-struct pattern (attrs ctx names description descrim) #:transparent)

;; A PatternContext (PCtx) is
;;   (make-pctx stx nat (listof IAttr) (listof IAttr))
(define-struct pctx (ostx depth env outer-env) #:transparent)

;; A Descriminator is one of
;;  (make-d:any)
;;  (make-d:stxclass SC (listof stx))
;;  (make-d:datum datum)
;;  (make-d:literal id)
;;  (make-d:gseq (listof Head) Pattern)
;;  (make-d:and (listof Pattern))
;;  (make-d:orseq (listof Head))
;;  (make-d:compound Kind (listof Pattern))
(define-struct d:any () #:transparent)
(define-struct d:stxclass (stxclass args) #:transparent)
(define-struct d:datum (datum) #:transparent)
(define-struct d:literal (literal) #:transparent)
(define-struct d:gseq (heads tail) #:transparent)
(define-struct d:and (subpatterns) #:transparent)
(define-struct d:orseq (heads) #:transparent)
(define-struct d:compound (kind patterns) #:transparent)
|#

;; A Pattern is one of
;;   (make-pat:id <Pattern> identifier SC/#f (listof stx))
;;   (make-pat:datum <Pattern> datum)
;;   (make-pat:pair <Pattern> Pattern Pattern)
;;   (make-pat:seq <Pattern> Pattern Pattern)
;;   (make-pat:gseq <Pattern> (listof Head) Pattern)
;;   (make-pat:and <Pattern> string/#f (listof Pattern))
;;   (make-pat:compound <Pattern> Kind (listof Pattern))
;; when <Pattern> = stx (listof IAttr) number
(define-struct pattern (ostx attrs depth) #:transparent)
(define-struct (pat:id pattern) (name stxclass args) #:transparent)
(define-struct (pat:datum pattern) (datum) #:transparent)
(define-struct (pat:literal pattern) (literal) #:transparent)
(define-struct (pat:gseq pattern) (heads tail) #:transparent)
(define-struct (pat:and pattern) (description subpatterns) #:transparent)
(define-struct (pat:orseq pattern) (heads) #:transparent)
(define-struct (pat:compound pattern) (kind patterns) #:transparent)

;; A Kind is (make-kind id (listof (id id -> stx)) (listof (FCE id -> FCE)))
(define-struct kind (predicate selectors frontier-procs) #:transparent)

;; A Head is
;;   (make-head stx (listof IAttr) nat (listof Pattern)
;;              nat/f nat/f boolean id/#f stx/#f)
(define-struct head (ostx attrs depth ps min max as-list?) #:transparent)

;; A SideClause is one of
;;   (make-clause:with pattern stx)
;;   (make-clause:when stx)
(define-struct clause:with (pattern expr) #:transparent)
(define-struct clause:when (expr) #:transparent)

;; make-empty-sc : identifier -> SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-empty-sc name)
  (make sc (syntax-e name) null null #f #f))

(define (iattr? a)
  (and (attr? a) (identifier? (attr-name a))))

(define (sattr? a)
  (and (attr? a) (symbol? (attr-name a))))


;; Environments

;; DeclEnv maps [id => DeclEntry]
;; DeclEntry =
;;   (list 'literal id id)
;;   (list 'stxclass id id (listof stx))
;;   #f

(define-struct declenv (bm))

(define (new-declenv literals)
  (let ([decls (make-declenv (make-bound-identifier-mapping))])
    (for ([literal literals])
      (declenv-put-literal decls (car literal) (cadr literal)))
    decls))

(define (declenv-lookup env id)
  (bound-identifier-mapping-get (declenv-bm env) id (lambda () #f)))

(define (declenv-check-unbound env id [stxclass-name #f]
                               #:blame-declare? [blame-declare? #f])
  ;; Order goes: literals, pattern, declares
  ;; So blame-declare? only applies to stxclass declares
  (let ([val (declenv-lookup env id)])
    (when val
      (cond [(eq? 'literal (car val))
             (wrong-syntax id "identifier previously declared as literal")]
            [(and blame-declare? stxclass-name)
             (wrong-syntax (cadr val)
                           "identifier previously declared with syntax class ~a"
                           stxclass-name)]
            [else
             (wrong-syntax (if blame-declare? (cadr val) id)
                           "identifier previously declared")]))))

(define (declenv-put-literal env internal-id lit-id)
  (declenv-check-unbound env internal-id)
  (bound-identifier-mapping-put! (declenv-bm env) internal-id
                                 (list 'literal internal-id lit-id)))

(define (declenv-put-stxclass env id stxclass-name args)
  (declenv-check-unbound env id)
  (bound-identifier-mapping-put! (declenv-bm env) id
                                 (list 'stxclass id stxclass-name args)))

;; returns ids in domain of env but not in given list
(define (declenv-domain-difference env ids)
  (define idbm (make-bound-identifier-mapping))
  (define excess null)
  (for ([id ids]) (bound-identifier-mapping-put! idbm id #t))
  (bound-identifier-mapping-for-each
   (declenv-bm env)
   (lambda (k v)
     (when (and (pair? v) (eq? (car v) 'stxclass))
       (unless (bound-identifier-mapping-get idbm k (lambda () #f))
         (set! excess (cons k excess))))))
  excess)

;; A RemapEnv is a bound-identifier-mapping

(define (new-remapenv)
  (make-bound-identifier-mapping))

(define (remapenv-lookup env id)
  (bound-identifier-mapping-get env id (lambda () (syntax-e id))))

(define (remapenv-put env id sym)
  (bound-identifier-mapping-put! env id sym))

(define (remapenv-domain env)
  (bound-identifier-mapping-map env (lambda (k v) k)))

(define trivial-remap
  (new-remapenv))

;; Contracts

(define DeclEnv/c
  (flat-named-contract "DeclEnv/c" declenv?))

(define RemapEnv/c
  (flat-named-contract "RemapEnv/c" bound-identifier-mapping?))

(define SideClause/c
  (or/c clause:with? clause:when?))

(provide/contract
 [DeclEnv/c contract?]
 [RemapEnv/c contract?]
 [SideClause/c contract?]

 [make-empty-sc (-> identifier? sc?)]
 [allow-unbound-stxclasses (parameter/c boolean?)]
 [iattr? (any/c . -> . boolean?)]
 [sattr? (any/c . -> . boolean?)]

 [new-declenv
  (-> (listof (list/c identifier? identifier?)) DeclEnv/c)]
 [declenv-lookup
  (-> declenv? identifier? any)]
 [declenv-put-literal
  (-> declenv? identifier? identifier? any)]
 [declenv-put-stxclass
  (-> declenv? identifier? identifier? (listof syntax?)
      any)]
 [declenv-domain-difference
  (-> declenv? (listof identifier?)
      (listof identifier?))]

 [new-remapenv
  (-> RemapEnv/c)]
 [remapenv-lookup
  (-> RemapEnv/c identifier? symbol?)]
 [remapenv-put
  (-> RemapEnv/c identifier? symbol? any)]
 [remapenv-domain
  (-> RemapEnv/c list?)]
 [trivial-remap
  RemapEnv/c]

 [iattr->sattr (iattr? . -> . sattr?)]
 [rename-attr
  (attr? symbol? . -> . sattr?)]
 [iattrs->sattrs
  (-> (listof iattr?) RemapEnv/c
      (listof sattr?))]
 [sattr->iattr/id (sattr? identifier? . -> . iattr?)]

 [get-stxclass
  (-> identifier? any)]
 [get-stxclass/check-arg-count
  (-> identifier? exact-nonnegative-integer? any)]
 [split-id/get-stxclass
  (-> identifier? DeclEnv/c any)]

 [intersect-attrss ((listof (listof sattr?)) syntax? . -> . (listof sattr?))]
 [join-attrs (sattr? sattr? syntax? . -> . sattr?)]
 [reorder-iattrs
  (-> (listof sattr?) (listof iattr?) RemapEnv/c
      (listof iattr?))]
 [restrict-iattrs
  (-> (listof sattr?) (listof iattr?) RemapEnv/c
      (listof iattr?))]
 [flatten-sattrs
  (->* [(listof sattr?)]
       [exact-integer? (or/c symbol? false/c)]
       (listof sattr?))]
 [intersect-sattrs ((listof sattr?) (listof sattr?) . -> . (listof sattr?))]
 [flatten-attrs*
  (->* [(listof iattr?)]
       [exact-nonnegative-integer? any/c any/c]
       (listof iattr?))]
 [append-attrs ((listof (listof iattr?)) . -> . (listof iattr?))]
 [lookup-sattr (symbol? (listof sattr?) . -> . (or/c sattr? false/c))]
 [lookup-iattr (identifier? (listof iattr?) . -> . (or/c iattr? false/c))]
 )


(define allow-unbound-stxclasses (make-parameter #f))

(define (iattr->sattr a)
  (match a
    [(struct attr (name depth inner))
     (make attr (syntax-e name) depth inner)]))

(define (rename-attr a name)
  (make attr name (attr-depth a) (attr-inner a)))

(define (iattrs->sattrs as remap)
  (if (pair? as)
      (let ([name* (remapenv-lookup remap (attr-name (car as)))])
        (if name*
            (cons (rename-attr (car as) name*)
                  (iattrs->sattrs (cdr as) remap))
            (iattrs->sattrs (cdr as) remap)))
      null))

(define (sattr->iattr/id a id)
  (match a
    [(struct attr (name depth inner))
     (make attr (datum->syntax id name id) depth inner)]))


(define (get-stxclass id)
  (define (no-good)
    (if (allow-unbound-stxclasses)
        (make-empty-sc id)
        (wrong-syntax id "not defined as syntax class")))
  (let ([sc (syntax-local-value/catch id sc?)])
    (if (sc? sc)
        sc
        (no-good))))

(define (get-stxclass/check-arg-count id arg-count)
  (let* ([sc (get-stxclass id)]
         [expected-arg-count (length (sc-inputs sc))])
    (unless (or (= expected-arg-count arg-count)
                (allow-unbound-stxclasses))
      ;; (above: don't check error if stxclass may not be defined yet)
      (wrong-syntax id
                    "too few arguments for syntax-class ~a (expected ~s)"
                    (syntax-e id)
                    expected-arg-count))
    sc))

(define (split-id/get-stxclass id0 decls)
  (cond [(regexp-match #rx"^([^:]*):(.+)$" (symbol->string (syntax-e id0)))
         => (lambda (m)
              (define id
                (datum->syntax id0 (string->symbol (cadr m)) id0 id0))
              (define scname
                (datum->syntax id0 (string->symbol (caddr m)) id0 id0))
              (declenv-check-unbound decls id (syntax-e scname)
                                     #:blame-declare? #t)
              (let ([sc (get-stxclass/check-arg-count scname 0)])
                (values id sc null)))]
        [else (values id0 #f null)]))

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
    (apply wrong-syntax blamestx str args))
  (if (not b)
      a
      (begin
        (unless (equal? (attr-depth a) (attr-depth b))
          (complain "attribute '~a'occurs with different nesting depth"
                    (attr-name a)))
        (make attr (attr-name a)
                   (attr-depth a)
                   (intersect-attrss (list (attr-inner a) (attr-inner b))
                                     blamestx)))))

;; reorder-iattrs : (listof SAttr) (listof IAttr) RemapEnv/c -> (listof IAttr)
;; Reorders iattrs (and restricts) based on relsattrs
;; If a relsattr is not found, or if depth or contents mismatches, raises error.
(define (reorder-iattrs relsattrs iattrs remap)
  (let ([ht (make-hasheq)])
    (for ([iattr iattrs])
      (let ([remap-name (remapenv-lookup remap (attr-name iattr))])
        (hash-set! ht remap-name iattr)))
    (let loop ([relsattrs relsattrs])
      (match relsattrs
        ['() null]
        [(cons (struct attr (name depth inner)) rest)
         (let ([iattr (hash-ref ht name #f)])
           (unless iattr
             (wrong-syntax #f "required attribute is not defined: ~s" name))
           (unless (= (attr-depth iattr) depth)
             (wrong-syntax (attr-name iattr)
                           "attribute has wrong depth (expected ~s, found ~s)"
                           depth (attr-depth iattr)))
           (cons (make attr (attr-name iattr)
                       (attr-depth iattr)
                       (intersect-sattrs inner (attr-inner iattr)))
                 (loop rest)))]))))

;; restrict-iattrs : (listof SAttr) (listof IAttr) RemapEnv/c -> (listof IAttr)
;; Preserves order of iattrs
(define (restrict-iattrs relsattrs iattrs remap)
  (match iattrs
    ['() null]
    [(cons (struct attr (name depth inner)) rest)
     (let ([sattr (lookup-sattr (remapenv-lookup remap name) relsattrs)])
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

;; append-attrs : (listof (listof IAttr)) -> (listof IAttr)
(define (append-attrs attrss)
  (let* ([all (apply append attrss)]
         [names (map attr-name all)]
         [dup (check-duplicate-identifier names)])
    (when dup
      (wrong-syntax dup "duplicate pattern variable"))
    all))

(define (lookup-sattr name sattrs)
  (cond [(null? sattrs) #f]
        [(eq? name (attr-name (car sattrs))) (car sattrs)]
        [else (lookup-sattr name (cdr sattrs))]))

(define (lookup-iattr name iattrs)
  (cond [(null? iattrs) #f]
        [(bound-identifier=? name (attr-name (car iattrs))) (car iattrs)]
        [else (lookup-iattr name (cdr iattrs))]))
