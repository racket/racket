#lang scheme/base
(require scheme/contract/base
         scheme/match
         scheme/dict
         syntax/stx
         syntax/id-table
         "../util.ss"
         "rep-attrs.ss"
         "rep-patterns.ss")
(provide (all-from-out "rep-attrs.ss")
         (all-from-out "rep-patterns.ss")
         (struct-out stxclass)
         stxclass/s?
         stxclass/h?
         (struct-out attr)
         (struct-out rhs)
         (struct-out variant)
         (struct-out clause:fail)
         (struct-out clause:with)
         (struct-out clause:attr)
         (struct-out conventions)
         (struct-out literalset))

#|
A stxclass is
  (make-sc symbol (listof symbol) (list-of SAttr) identifier identifier boolean)
|#
(define-struct stxclass (name params attrs parser-name description splicing?)
  #:prefab)

(define (stxclass/s? x)
  (and (stxclass? x) (not (stxclass-splicing? x))))
(define (stxclass/h? x)
  (and (stxclass? x) (stxclass-splicing? x)))

#|
An RHS is
  (make-rhs stx (listof SAttr) boolean stx/#f (listof Variant) (listof stx))
definitions: auxiliary definitions from #:declare
|#
(define-struct rhs (ostx attrs transparent? description variants definitions)
  #:prefab)

#|
A Variant is
  (make-variant stx (listof SAttr) Pattern (listof SideClause))
|#
(define-struct variant (ostx attrs pattern sides definitions) #:prefab)

#|
A SideClause is one of
  (make-clause:fail stx stx)
  (make-clause:with pattern stx (listof stx))
  (make-clause:attr IAttr stx)
|#
(define-struct clause:fail (condition message) #:prefab)
(define-struct clause:with (pattern expr definitions) #:prefab)
(define-struct clause:attr (attr expr) #:prefab)

#|
A Conventions is
  (make-conventions (listof ConventionRule))
A ConventionRule is (list regexp DeclEntry)
|#
(define-struct conventions (rules) #:transparent)

#|
A LiteralSet is
  (make-literalset (listof (list symbol id)))
|#
(define-struct literalset (literals) #:transparent)

;; make-dummy-stxclass : identifier -> SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-dummy-stxclass name)
  (make stxclass (syntax-e name) null null #f #f #f))


;; Environments

#|
DeclEnv =
  (make-declenv immutable-bound-id-mapping[id => DeclEntry]
                (listof ConventionRule))
DeclEntry =
  (list 'literal id id)
  (list 'stxclass id id (listof stx))
  (list 'parser id id (listof IAttr))
  #f
|#
(define-struct declenv (table conventions))

(define (new-declenv literals #:conventions [conventions null])
  (for/fold ([decls (make-declenv (make-immutable-bound-id-table) conventions)])
      ([literal literals])
    (declenv-put-literal decls (car literal) (cadr literal))))

(define (declenv-lookup env id #:use-conventions? [use-conventions? #t])
  (or (bound-id-table-ref (declenv-table env) id #f)
      (and use-conventions?
           (conventions-lookup (declenv-conventions env) id))))

(define (declenv-check-unbound env id [stxclass-name #f]
                               #:blame-declare? [blame-declare? #f])
  ;; Order goes: literals, pattern, declares
  ;; So blame-declare? only applies to stxclass declares
  (let ([val (declenv-lookup env id #:use-conventions? #f)])
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
  (make-declenv
   (bound-id-table-set (declenv-table env) internal-id
                       (list 'literal internal-id lit-id))
   (declenv-conventions env)))

(define (declenv-put-stxclass env id stxclass-name args)
  (declenv-check-unbound env id)
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (list 'stxclass id stxclass-name args))
   (declenv-conventions env)))

(define (declenv-put-parser env id parser get-description attrs splicing?)
  ;; no unbound check, since replacing 'stxclass entry
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (list (if splicing? 'splicing-parser 'parser)
                             parser get-description attrs))
   (declenv-conventions env)))

;; returns ids in domain of env but not in given list
(define (declenv-domain-difference env ids)
  (define idbm (make-bound-id-table))
  (for ([id ids]) (bound-id-table-set! idbm id #t))
  (for/list ([(k v) (in-dict (declenv-table env))]
             #:when (and (pair? v) (not (eq? (car v) 'literal)))
             #:when (not (bound-id-table-ref idbm k #f)))
    k))

;; Conventions = (listof (list regexp DeclEntry))

(define (conventions-lookup conventions id)
  (let ([sym (symbol->string (syntax-e id))])
    (for/or ([c conventions])
      (and (regexp-match? (car c) sym) (cadr c)))))

;; Contracts

(define DeclEnv/c
  (flat-named-contract 'DeclEnv declenv?))

(define SideClause/c
  (or/c clause:fail? clause:with? clause:attr?))

(provide/contract
 [DeclEnv/c contract?]
 [SideClause/c contract?]

 [make-dummy-stxclass (-> identifier? stxclass?)]
 [stxclass-lookup-config (parameter/c (symbols 'no 'try 'yes))]

 [new-declenv
  (->* [(listof (list/c identifier? identifier?))]
       [#:conventions list?]
       DeclEnv/c)]
 [declenv-lookup
  (-> DeclEnv/c identifier? any)]
 [declenv-put-stxclass
  (-> DeclEnv/c identifier? identifier? (listof syntax?)
      DeclEnv/c)]
 [declenv-put-parser
  (-> DeclEnv/c identifier? any/c any/c (listof sattr?) boolean?
      DeclEnv/c)]
 [declenv-domain-difference
  (-> DeclEnv/c (listof identifier?)
      (listof identifier?))]
 [declenv-table
  (-> DeclEnv/c any)]

 [get-stxclass
  (-> identifier? any)]
 [get-stxclass/check-arg-count
  (-> identifier? exact-nonnegative-integer? any)]
 [split-id/get-stxclass
  (-> identifier? DeclEnv/c any)])

;; stxclass-lookup-config : (parameterof (U 'no 'try 'yes))
;;  'no means don't lookup, always use dummy (no nested attrs)
;;  'try means lookup, but on failure use dummy (-> nested attrs only from prev.)
;;  'yes means lookup, raise error on failure
(define stxclass-lookup-config (make-parameter 'yes))

(define (get-stxclass id)
  (define config (stxclass-lookup-config))
  (if (eq? config 'no)
      (make-dummy-stxclass id)
      (cond [(syntax-local-value/catch id stxclass?) => values]
            [(eq? config 'try)
             (make-dummy-stxclass id)]
            [else (wrong-syntax id "not defined as syntax class")])))

(define (get-stxclass/check-arg-count id arg-count)
  (let* ([sc (get-stxclass id)]
         [expected-arg-count (length (stxclass-params sc))])
    (unless (or (= expected-arg-count arg-count)
                (memq (stxclass-lookup-config) '(try no)))
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
                (values id sc)))]
        [else (values id0 #f)]))
