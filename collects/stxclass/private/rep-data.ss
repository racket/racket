
#lang scheme
(require (for-template "kws.ss")
         (for-template scheme/base)
         scheme/contract
         syntax/boundmap
         syntax/stx
         "../util.ss")
(provide (struct-out sc)
         (struct-out attr)
         (struct-out rhs)
         (struct-out rhs:union)
         (struct-out rhs:basic)
         (struct-out rhs:pattern)
         (struct-out pattern)
         (struct-out pat:id)
         (struct-out pat:datum)
         (struct-out pat:literal)
         (struct-out pat:pair)
         (struct-out pat:gseq)
         (struct-out head)
         (struct-out clause:when)
         (struct-out clause:with))

;; An SC is one of (make-sc symbol (listof symbol) (list-of SAttr) identifier)
(define-struct sc (name inputs attrs parser-name description)
  #:property prop:procedure (lambda (self stx) (sc-parser-name self))
  #:transparent)

;; An SSC is one of (make-ssc symbol (listof symbol) (list-of SAttr) identifier)
(define-struct ssc (name inputs attrs parser-name)
  #:transparent)

;; An IAttr is (make-attr identifier number (listof SAttr))
;; An SAttr is (make-attr symbol number (listof SAttr))
(define-struct attr (name depth inner)
  #:transparent)

;; RHSBase is stx (listof SAttr) boolean stx/#f
(define-struct rhs (orig-stx attrs transparent? description)
  #:transparent)

;; A RHS is one of
;;   (make-rhs:union <RHSBase> (listof RHS))
;;   (make-rhs:basic <RHSBase> stx)
(define-struct (rhs:union rhs) (patterns)
  #:transparent)
(define-struct (rhs:basic rhs) (parser)
  #:transparent)

;; An RHSPattern is
;;   (make-rhs:pattern stx (listof SAttr) Pattern Env Env (listof SideClause))
(define-struct rhs:pattern (stx attrs pattern decls remap whens)
  #:transparent)

;; A Pattern is one of
;;   (make-pat:id <Pattern> identifier SC/#f (listof stx))
;;   (make-pat:datum <Pattern> datum)
;;   (make-pat:pair <Pattern> Pattern Pattern)
;;   (make-pat:seq <Pattern> Pattern Pattern)
;;   (make-pat:gseq <Pattern> (listof Head) Pattern)
;; when <Pattern> = stx (listof IAttr) number
(define-struct pattern (orig-stx attrs depth) #:transparent)
(define-struct (pat:id pattern) (name stxclass args) #:transparent)
(define-struct (pat:datum pattern) (datum) #:transparent)
(define-struct (pat:literal pattern) (literal) #:transparent)
(define-struct (pat:pair pattern) (head tail) #:transparent)
(define-struct (pat:gseq pattern) (heads tail) #:transparent)

;; A Head is
;;   (make-head stx (listof IAttr) nat (listof Pattern) nat/f nat/f boolean id/#f stx/#f)
(define-struct head (orig-stx attrs depth ps min max as-list? occurs default)
  #:transparent)

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

;; Contracts

;; DeclEnv = [id -> (list* id id (listof stx)) or #t or #f
;;   #t means literal, #f means undeclared, list means stxclass (w/ args)
(define DeclEnv/c
  (-> identifier? 
      (or/c boolean? (cons/c identifier? (cons/c identifier? (listof syntax?))))))

(define RemapEnv/c
  (-> identifier? symbol?))

(define SideClause/c (or/c clause:with? clause:when?))


(provide/contract
 [DeclEnv/c contract?]
 [RemapEnv/c contract?]
 [SideClause/c contract?]

 [make-empty-sc (-> identifier? sc?)]
 [allow-unbound-stxclasses (parameter/c boolean?)]
 [iattr? (any/c . -> . boolean?)]
 [sattr? (any/c . -> . boolean?)]
 [iattr->sattr (iattr? . -> . sattr?)]
 [rename-attr (attr? symbol? . -> . sattr?)]
 [iattrs->sattrs ((listof iattr?) (identifier? . -> . symbol?) . -> . (listof sattr?))]
 [sattr->iattr/id (sattr? identifier? . -> . iattr?)]

 [get-stxclass (-> identifier? any)]
 [split-id/get-stxclass (-> identifier? any/c any)]

 [intersect-attrss ((listof (listof sattr?)) syntax? . -> . (listof sattr?))]
 [join-attrs (sattr? sattr? syntax? . -> . sattr?)]
 [reorder-iattrs
  ((listof sattr?) (listof iattr?) (identifier? . -> . symbol?) . -> . (listof iattr?))]
 [restrict-iattrs
  ((listof sattr?) (listof iattr?) (identifier? . -> . symbol?) . -> . (listof iattr?))]
 [flatten-sattrs
  ([(listof sattr?)] [exact-integer? (or/c symbol? false/c)] . ->* . (listof sattr?))]
 [intersect-sattrs ((listof sattr?) (listof sattr?) . -> . (listof sattr?))]
 [flatten-attrs* any/c]
 [append-attrs ((listof (listof iattr?)) syntax? . -> . (listof iattr?))]
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


(define (get-stxclass id)
  (define (no-good)
    (if (allow-unbound-stxclasses)
        (make-empty-sc id)
        (wrong-syntax id "not defined as syntax class")))
  (let ([sc (syntax-local-value id no-good)])
    (unless (or (sc? sc) (ssc? sc))
      (no-good))
    sc))

(define (split-id/get-stxclass id0 decls)
  (cond [(regexp-match #rx"^([^:]*):(.+)$" (symbol->string (syntax-e id0)))
         => (lambda (m)
              (define id (datum->syntax id0 (string->symbol (cadr m)) id0 id0))
              (define scname (datum->syntax id0 (string->symbol (caddr m)) id0 id0))
              (match (decls id)
                [#t
                 (wrong-syntax id "name already declared as literal")]
                [(list* id2 scname2 args)
                 (wrong-syntax id2
                               "name already declared with syntax-class ~s"
                               (syntax-e scname))]
                [_ (void)])
              (let ([sc (get-stxclass scname)])
                (values id sc null)))]
        [(decls id0)
         => (lambda (p)
              (define scname (cadr p))
              (define args (cddr p))
              (define stxclass (get-stxclass scname))
              (unless (equal? (length (sc-inputs stxclass)) (length args))
                (wrong-syntax id0
                              "too few arguments for syntax-class ~a (expected ~s)"
                              (sc-name stxclass)
                              (length (sc-inputs stxclass))))
              (values id0 stxclass args))]
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


;; append-attrs : (listof (listof IAttr)) stx -> (listof IAttr)
(define (append-attrs attrss stx)
  (let* ([all (apply append attrss)]
         [names (map attr-name all)]
         [dup (check-duplicate-identifier names)])
    (when dup
      (raise-syntax-error 'syntax-class "duplicate pattern variable" stx dup))
    all))

(define (lookup-sattr name sattrs)
  (cond [(null? sattrs) #f]
        [(eq? name (attr-name (car sattrs))) (car sattrs)]
        [else (lookup-sattr name (cdr sattrs))]))

(define (lookup-iattr name iattrs)
  (cond [(null? iattrs) #f]
        [(bound-identifier=? name (attr-name (car iattrs))) (car iattrs)]
        [else (lookup-iattr name (cdr iattrs))]))
