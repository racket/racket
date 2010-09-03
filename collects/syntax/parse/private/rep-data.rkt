#lang racket/base
(require racket/contract/base
         racket/dict
         racket/list
         syntax/id-table
         unstable/syntax
         "minimatch.rkt"
         "kws.rkt"
         "rep-attrs.rkt"
         "rep-patterns.rkt")
(provide (all-from-out "rep-attrs.rkt")
         (all-from-out "rep-patterns.rkt")
         (struct-out stxclass)
         (struct-out options)
         (struct-out integrate)
         stxclass/s?
         stxclass/h?
         stxclass-commit?
         stxclass-delimit-cut?
         (struct-out attr)
         (struct-out rhs)
         (struct-out variant)
         (struct-out clause:fail)
         (struct-out clause:with)
         (struct-out clause:attr)
         (struct-out clause:do)
         (struct-out conventions)
         (struct-out literalset)
         (struct-out eh-alternative-set)
         (struct-out eh-alternative))

#|
A stxclass is
  #s(stxclass symbol (listof symbol) (list-of SAttr) identifier bool Options Integrate/#f)
where Options = #s(options boolean boolean)
      Integrate = #s(integrate id string)
Arity is defined in kws.rkt
|#
(define-struct stxclass (name arity attrs parser splicing? options integrate)
  #:prefab)

(define-struct options (commit? delimit-cut?)
  #:prefab)
(define-struct integrate (predicate description)
  #:prefab)

(define (stxclass/s? x)
  (and (stxclass? x) (not (stxclass-splicing? x))))
(define (stxclass/h? x)
  (and (stxclass? x) (stxclass-splicing? x)))

(define (stxclass-commit? x)
  (options-commit? (stxclass-options x)))
(define (stxclass-delimit-cut? x)
  (options-delimit-cut? (stxclass-options x)))

#|
An RHS is
  #s(rhs stx (listof SAttr) bool stx/#f (listof Variant) (listof stx) Options Integrate/#f)
definitions: auxiliary definitions from #:declare
|#
(define-struct rhs (ostx attrs transparent? description variants definitions options integrate)
  #:prefab)

#|
A Variant is
  (make-variant stx (listof SAttr) Pattern (listof stx))
|#
(define-struct variant (ostx attrs pattern definitions) #:prefab)

#|
SideClause is defined in rep-patterns
|#

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
  (make stxclass (syntax-e name) #f null #f #f #s(options #f #t) #f))

#|
An EH-alternative-set is
  (eh-alternative-set (listof EH-alternative)
An EH-alternative is
  (eh-alternative RepetitionConstraint (listof SAttr) id)
|#
(define-struct eh-alternative-set (alts))
(define-struct eh-alternative (repc attrs parser))

;; Environments

#|
DeclEnv =
  (make-declenv immutable-bound-id-mapping[id => DeclEntry]
                (listof ConventionRule))

DeclEntry =
  (den:lit id id ct-phase ct-phase)
  (den:class id id Arguments)
  (den:parser id (listof SAttr) bool bool bool)
  (den:delayed id id)

Arguments is defined in rep-patterns.rkt
|#
(define-struct declenv (table conventions))

(define-struct den:lit (internal external input-phase lit-phase))
(define-struct den:class (name class argu))
(define-struct den:parser (parser attrs splicing? commit? delimit-cut?))
(define-struct den:delayed (parser class))

(define (new-declenv literals #:conventions [conventions null])
  (make-declenv
   (for/fold ([table (make-immutable-bound-id-table)])
       ([literal (in-list literals)])
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
      [(struct den:parser (_p _a _sp _c _dc?))
       (wrong-syntax id "(internal error) late unbound check")]
      ['#f (void)])))

(define (declenv-put-stxclass env id stxclass-name argu)
  (declenv-check-unbound env id)
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (make den:class id stxclass-name argu))
   (declenv-conventions env)))

;; declenv-update/fold : DeclEnv (Id/Regexp DeclEntry a -> DeclEntry a) a
;;                    -> (values DeclEnv a)
(define (declenv-update/fold env0 f acc0)
  (define-values (acc1 rules1)
    (for/fold ([acc acc0] [newrules null])
        ([rule (in-list (declenv-conventions env0))])
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
  (for ([id (in-list ids)]) (bound-id-table-set! idbm id #t))
  (for/list ([(k v) (in-dict (declenv-table env))]
             #:when (or (den:class? v) (den:parser? v))
             #:when (not (bound-id-table-ref idbm k #f)))
    k))

;; Conventions = (listof (list regexp DeclEntry))

(define (conventions-lookup conventions id)
  (let ([sym (symbol->string (syntax-e id))])
    (for/or ([c (in-list conventions)])
      (and (regexp-match? (car c) sym) (cadr c)))))

;; Contracts

(define DeclEnv/c
  (flat-named-contract 'DeclEnv declenv?))

(define DeclEntry/c
  (flat-named-contract 'DeclEntry
                       (or/c den:lit? den:class? den:parser? den:delayed?)))

(define SideClause/c
  (or/c clause:fail? clause:with? clause:attr? clause:do?))

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
  (-> DeclEnv/c identifier? identifier? arguments?
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
  (-> identifier? stxclass?)]
 [get-stxclass/check-arity
  (-> identifier? syntax? exact-nonnegative-integer? (listof keyword?)
      stxclass?)]
 [split-id/get-stxclass
  (-> identifier? DeclEnv/c
      (values identifier? (or/c stxclass? #f)))])

;; stxclass-lookup-config : (parameterof (U 'no 'try 'yes))
;;  'no means don't lookup, always use dummy (no nested attrs)
;;  'try means lookup, but on failure use dummy (-> nested attrs only from prev.)
;;  'yes means lookup, raise error on failure
(define stxclass-lookup-config (make-parameter 'yes))

(define (get-stxclass id)
  (define config (stxclass-lookup-config))
  (if (eq? config 'no)
      (make-dummy-stxclass id)
      (cond [(syntax-local-value/record id stxclass?) => values]
            [(eq? config 'try)
             (make-dummy-stxclass id)]
            [else (wrong-syntax id "not defined as syntax class")])))

(define (get-stxclass/check-arity id stx pos-count keywords)
  (let ([sc (get-stxclass id)])
    (unless (memq (stxclass-lookup-config) '(try no))
      (check-arity (stxclass-arity sc) pos-count keywords
                   (lambda (msg)
                     (raise-syntax-error #f msg stx))))
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
              (let ([sc (get-stxclass/check-arity scname id0 0 null)])
                (values id sc)))]
        [else (values id0 #f)]))

;; ----

(provide get-eh-alternative-set)

(define (get-eh-alternative-set id)
  (let ([v (syntax-local-value id (lambda () #f))])
    (unless (eh-alternative-set? v)
      (wrong-syntax id "not defined as an eh-alternative-set"))
    v))
