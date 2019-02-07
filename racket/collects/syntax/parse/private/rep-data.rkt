#lang racket/base
(require racket/contract/base
         racket/dict
         syntax/private/id-table
         racket/syntax
         syntax/parse/private/residual-ct ;; keep abs. path
         "minimatch.rkt"
         "kws.rkt")
;; from residual.rkt
(provide (struct-out stxclass)
         (struct-out conventions)
         (struct-out literalset)
         (struct-out eh-alternative-set)
         (struct-out eh-alternative))
;; from here
(provide stxclass/s?
         stxclass/h?
         (struct-out rhs)
         (struct-out variant))

(define (stxclass/s? x)
  (and (stxclass? x) (not (stxclass-splicing? x))))
(define (stxclass/h? x)
  (and (stxclass? x) (stxclass-splicing? x)))

;; An RHS is #s(rhs SAttrs Bool Stx/#f Variants Stxs Bool Bool)
(define-struct rhs
  (attrs        ;; (Listof Sattr)
   transparent? ;; Bool
   description  ;; Syntax/#f
   variants     ;; (Listof Variant)
   definitions  ;; (Listof Stx), aux definitions from txlifts, local conventions?, etc
   commit?      ;; Bool
   delimit-cut? ;; Bool
   ) #:prefab)

;; A Variant is (variant Stx SAttrs Pattern Stxs)
(define-struct variant
  (ostx         ;; Stx
   attrs        ;; (Listof SAttr)
   pattern      ;; Pattern
   definitions  ;; (Listof Stx)
   ) #:prefab)

;; make-dummy-stxclass : identifier -> SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-dummy-stxclass name)
  (stxclass (syntax-e name) #f null #f #f (scopts 0 #t #t #f) #f))

;; Environments

#|
DeclEnv =
  (make-declenv immutable-bound-id-mapping[id => DeclEntry]
                (listof ConventionRule))

DeclEntry =
- (den:lit Id Id Stx Stx)
- (den:datum-lit Id Symbol)
- (den:class Id Id Arguments)
- (den:magic-class Id Id Arguments Stx)
- (den:delayed Id Id)

Arguments is defined in rep-patterns.rkt

A DeclEnv is built up in stages:
  1) syntax-parse (or define-syntax-class) directives
     #:literals -> den:lit
     #:datum-literals -> den:datum-lit
     #:local-conventions -> den:class
     #:conventions -> den:delayed
     #:literal-sets -> den:lit
  2) pattern directives
     #:declare -> den:magic-class
  3) create-aux-def creates aux parser defs
     den:class -> den:delayed

== Scoping ==

A #:declare directive results in a den:magic-class entry, which
indicates that the pattern variable's syntax class arguments (if any)
have "magical scoping": they are evaluated in the scope where the
pattern variable occurs. If the variable occurs multiple times, the
expressions are duplicated, and may be evaluated in different scopes.
|#

(define-struct declenv (table conventions))

(define-struct den:class (name class argu))
(define-struct den:magic-class (name class argu role))
;; and from residual.rkt:
;;  (define-struct den:lit (internal external input-phase lit-phase))
;;  (define-struct den:datum-lit (internal external))
;;  (define-struct den:delayed (parser class))

(define (new-declenv literals #:conventions [conventions null])
  (let* ([table (make-immutable-bound-id-table)]
         [table (for/fold ([table table]) ([literal (in-list literals)])
                  (let ([id (cond [(den:lit? literal) (den:lit-internal literal)]
                                  [(den:datum-lit? literal) (den:datum-lit-internal literal)])])
                    ;;(eprintf ">> added ~e\n" id)
                    (bound-id-table-set table id literal)))])
    (make-declenv table conventions)))

(define (declenv-lookup env id)
  (bound-id-table-ref (declenv-table env) id #f))

(define (declenv-apply-conventions env id)
  (conventions-lookup (declenv-conventions env) id))

(define (declenv-check-unbound env id [stxclass-name #f]
                               #:blame-declare? [blame-declare? #f])
  ;; Order goes: literals, pattern, declares
  ;; So blame-declare? only applies to stxclass declares
  (let ([val (declenv-lookup env id)])
    (match val
      [(den:lit _i _e _ip _lp)
       (wrong-syntax id "identifier previously declared as literal")]
      [(den:datum-lit _i _e)
       (wrong-syntax id "identifier previously declared as literal")]
      [(den:magic-class name _c _a _r)
       (if (and blame-declare? stxclass-name)
           (wrong-syntax name
                         "identifier previously declared with syntax class ~a"
                         stxclass-name)
           (wrong-syntax (if blame-declare? name id)
                         "identifier previously declared"))]
      [(den:class name _c _a)
       (if (and blame-declare? stxclass-name)
           (wrong-syntax name
                         "identifier previously declared with syntax class ~a"
                         stxclass-name)
           (wrong-syntax (if blame-declare? name id)
                         "identifier previously declared"))]
      ['#f (void)])))

(define (declenv-put-stxclass env id stxclass-name argu [role #f])
  (declenv-check-unbound env id)
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (den:magic-class id stxclass-name argu role))
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
             #:when (or (den:class? v) (den:magic-class? v))
             #:unless (bound-id-table-ref idbm k #f))
    k))

;; Conventions = (listof (list regexp DeclEntry))

(define (conventions-lookup conventions id)
  (let ([sym (symbol->string (syntax-e id))])
    (for/or ([c (in-list conventions)])
      (and (regexp-match? (car c) sym) (cadr c)))))

;; Contracts

(define DeclEnv/c declenv?)

(define DeclEntry/c 
  (or/c den:lit? den:datum-lit? den:class? den:magic-class? den:delayed?))

(provide (struct-out den:class)
         (struct-out den:magic-class)
         ;; from residual.rkt:
         (struct-out den:lit)
         (struct-out den:datum-lit)
         (struct-out den:delayed))

(provide/contract
 [DeclEnv/c contract?]
 [DeclEntry/c contract?]

 [new-declenv
  (->* [(listof (or/c den:lit? den:datum-lit?))]
       [#:conventions list?]
       DeclEnv/c)]
 [declenv-lookup
  (-> DeclEnv/c identifier? any)]
 [declenv-apply-conventions
  (-> DeclEnv/c identifier? any)]
 [declenv-put-stxclass
  (-> DeclEnv/c identifier? identifier? arguments? (or/c syntax? #f)
      DeclEnv/c)]
 [declenv-domain-difference
  (-> DeclEnv/c (listof identifier?)
      (listof identifier?))]
 [declenv-update/fold
  (-> DeclEnv/c
      (-> (or/c identifier? regexp?) DeclEntry/c any/c (values DeclEntry/c any/c))
      any/c
      (values DeclEnv/c any/c))]
 [declenv-check-unbound
  (->* [DeclEnv/c identifier?] [symbol? #:blame-declare? boolean?] any)]

 [get-stxclass
  (->* [identifier?] [boolean?] (or/c stxclass? #f))]
 [check-stxclass-arity
  (-> stxclass? syntax? exact-nonnegative-integer? (listof keyword?) any)]
 [get-stxclass/check-arity
  (-> identifier? syntax? exact-nonnegative-integer? (listof keyword?)
      stxclass?)])

;; get-stxclass : Identifier [Boolean] -> stxclass/#f
;; Stxclasses are primarily bound by env / syntax-local-value, but a few
;; are attached to existing bindings via alt-stxclass-mapping.
(define (get-stxclass id [allow-undef? #f])
  (let loop ([id id]
             [prev-ids '()])
    (cond [(syntax-local-value/record id stxclass?) => values]
          [(syntax-local-value/record id has-stxclass-prop?)
           => (lambda (val)
                (define prop-val (stxclass-prop-ref val))
                (define prop-id (if (identifier? prop-val) prop-val (prop-val val)))
                (loop prop-id (cons id prev-ids)))]
          [(assoc id (unbox alt-stxclass-mapping) free-identifier=?) => cdr]
          [allow-undef? #f]
          [else (wrong-syntax id #:extra prev-ids "not defined as syntax class")])))

;; check-stxclass-arity : stxclass Syntax Nat (Listof Keyword) -> Void
(define (check-stxclass-arity sc stx pos-count keywords)
  (check-arity (stxclass-arity sc) pos-count keywords
               (lambda (msg) (raise-syntax-error #f msg stx))))

(define (get-stxclass/check-arity id stx pos-count keywords)
  (define sc (get-stxclass id))
  (check-stxclass-arity sc stx pos-count keywords)
  sc)

;; ----

(provide get-eh-alternative-set)

(define (get-eh-alternative-set id)
  (let ([v (syntax-local-value id (lambda () #f))])
    (unless (eh-alternative-set? v)
      (wrong-syntax id "not defined as an eh-alternative-set"))
    v))
