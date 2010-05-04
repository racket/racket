#lang racket/base
(require racket/contract/base
         racket/dict
         racket/list
         syntax/stx
         syntax/id-table
         "../util.ss"
         "minimatch.ss"
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

NOTES

syntax-class protocol
---------------------

Two kinds of syntax class: commit? = #t, commit? = #f

let syntax-class SC have params (P ...)
  if commit? = #t
    parser : Stx P ... -> (U list expectation)
  if commit? = #f
    parser : Stx ((U list expect) FailFunction -> Answer) P ... -> Answer


conventions
-----------

let conventions C have params (P ...)
  get-procedures :
    (P ... -> (values (listof ParserFun) (listof DescriptionFun)))

|#

#|
A stxclass is
  (make-sc symbol (listof symbol) (list-of SAttr) identifier identifier boolean boolean)
|#
(define-struct stxclass (name params attrs parser-name description
                         splicing? commit?)
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
(define-struct rhs (ostx attrs transparent? description variants definitions commit?)
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
  (make-conventions id (-> (listof ConventionRule)))
A ConventionRule is (list regexp DeclEntry)
|#
(define-struct conventions (get-procedures get-rules) #:transparent)

#|
A LiteralSet is
  (make-literalset (listof (list symbol id)) stx)
|#
(define-struct literalset (literals phase) #:transparent)

;; make-dummy-stxclass : identifier -> SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-dummy-stxclass name)
  (make stxclass (syntax-e name) null null #f #f #f #t))


;; Environments

#|
DeclEnv =
  (make-declenv immutable-bound-id-mapping[id => DeclEntry]
                (listof ConventionRule))

DeclEntry =
  (make-den:lit id id ct-phase ct-phase)
  (make-den:class id id (listof syntax) bool)
  (make-den:parser id id (listof SAttr) bool bool)
  (make-den:delayed id id id)
|#
(define-struct declenv (table conventions))

(define-struct den:lit (internal external input-phase lit-phase))
(define-struct den:class (name class args))
(define-struct den:parser (parser description attrs splicing? commit?))
(define-struct den:delayed (parser description class))

(define (new-declenv literals #:conventions [conventions null])
  (make-declenv
   (for/fold ([table (make-immutable-bound-id-table)])
       ([literal literals])
     (bound-id-table-set table (car literal)
                         (make den:lit (first literal) (second literal)
                               (third literal) (fourth literal))))
   conventions))

(define (declenv-lookup env id #:use-conventions? [use-conventions? #t])
  (or (bound-id-table-ref (declenv-table env) id #f)
      (and use-conventions?
           (conventions-lookup (declenv-conventions env) id))))

(define (declenv-check-unbound env id [stxclass-name #f]
                               #:blame-declare? [blame-declare? #f])
  ;; Order goes: literals, pattern, declares
  ;; So blame-declare? only applies to stxclass declares
  (let ([val (declenv-lookup env id #:use-conventions? #f)])
    (match val
      [(struct den:lit (_i _e _ip _lp))
       (wrong-syntax id "identifier previously declared as literal")]
      [(struct den:class (name _c _a))
       (if (and blame-declare? stxclass-name)
           (wrong-syntax name
                         "identifier previously declared with syntax class ~a"
                         stxclass-name)
           (wrong-syntax (if blame-declare? name id)
                         "identifier previously declared"))]
      [(struct den:parser (_p _d _a _sp _c))
       (wrong-syntax id "(internal error) late unbound check")]
      ['#f (void)])))

(define (declenv-put-stxclass env id stxclass-name args)
  (declenv-check-unbound env id)
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (make den:class id stxclass-name args))
   (declenv-conventions env)))

;; declenv-update/fold : DeclEnv (Id/Regexp DeclEntry a -> DeclEntry a) a
;;                    -> (values DeclEnv a)
(define (declenv-update/fold env0 f acc0)
  (define-values (acc1 rules1)
    (for/fold ([acc acc0] [newrules null])
        ([rule (declenv-conventions env0)])
      (let-values ([(val acc) (f (car rule) (cadr rule) acc)])
        (values acc (cons (list (car rule) val) newrules)))))
  (define-values (acc2 table2)
    (for/fold ([acc acc1] [table (make-immutable-bound-id-table)])
        ([(k v) (in-dict (declenv-table env0))])
      (let-values ([(val acc) (f k v acc)])
        (values acc (bound-id-table-set table k val)))))
  (values (make-declenv table2 (reverse rules1))
          acc2))

;; returns ids in domain of env but not in given list
(define (declenv-domain-difference env ids)
  (define idbm (make-bound-id-table))
  (for ([id ids]) (bound-id-table-set! idbm id #t))
  (for/list ([(k v) (in-dict (declenv-table env))]
             #:when (or (den:class? v) (den:parser? v))
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

(define DeclEntry/c
  (flat-named-contract 'DeclEntry
                       (or/c den:lit? den:class? den:parser? den:delayed?)))

(define SideClause/c
  (or/c clause:fail? clause:with? clause:attr?))

;; ct-phase = syntax, expr that computes absolute phase
;;   usually = #'(syntax-local-phase-level)
(define ct-phase/c syntax?)

(provide (struct-out den:lit)
         (struct-out den:class)
         (struct-out den:parser)
         (struct-out den:delayed))

(provide/contract
 [DeclEnv/c contract?]
 [DeclEntry/c contract?]
 [SideClause/c contract?]
 [ct-phase/c contract?]

 [make-dummy-stxclass (-> identifier? stxclass?)]
 [stxclass-lookup-config (parameter/c (symbols 'no 'try 'yes))]

 [new-declenv
  (->* [(listof (list/c identifier? identifier? ct-phase/c ct-phase/c))]
       [#:conventions list?]
       DeclEnv/c)]
 [declenv-lookup
  (-> DeclEnv/c identifier? any)]
 [declenv-put-stxclass
  (-> DeclEnv/c identifier? identifier? (listof syntax?)
      DeclEnv/c)]
 [declenv-domain-difference
  (-> DeclEnv/c (listof identifier?)
      (listof identifier?))]
 [declenv-update/fold
  (-> DeclEnv/c
      (-> (or/c identifier? regexp?) DeclEntry/c any/c (values DeclEntry/c any/c))
      any/c
      (values DeclEnv/c any/c))]

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
