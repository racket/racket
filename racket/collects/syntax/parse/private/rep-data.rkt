#lang racket/base
(require racket/contract/base
         racket/dict
         racket/list
         syntax/private/id-table
         racket/syntax
         syntax/parse/private/residual-ct ;; keep abs. path
         "make.rkt"
         "minimatch.rkt"
         "kws.rkt"
         "rep-attrs.rkt"
         "rep-patterns.rkt")
;; from residual.rkt
(provide (struct-out stxclass)
         (struct-out options)
         (struct-out integrate)
         (struct-out conventions)
         (struct-out literalset)
         (struct-out eh-alternative-set)
         (struct-out eh-alternative))
;; from here
(provide stxclass/s?
         stxclass/h?
         stxclass-commit?
         stxclass-delimit-cut?
         (struct-out rhs)
         (struct-out variant)
         (struct-out clause:fail)
         (struct-out clause:with)
         (struct-out clause:attr)
         (struct-out clause:do))

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

;; make-dummy-stxclass : identifier -> SC
;; Dummy stxclass for calculating attributes of recursive stxclasses.
(define (make-dummy-stxclass name)
  (make stxclass (syntax-e name) #f null #f #f #s(options #f #t) #f))

;; Environments

#|
DeclEnv =
  (make-declenv immutable-bound-id-mapping[id => DeclEntry]
                (listof ConventionRule))

DeclEntry =
  (den:lit id id ct-phase ct-phase)
  (den:datum-lit id symbol)
  (den:class id id Arguments)
  (den:magic-class id id Arguments stx)
  (den:parser id (listof SAttr) bool bool bool)
  (den:delayed id id)

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
     den:class -> den:parser or den:delayed

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
(define-struct den:parser (parser attrs splicing? commit? delimit-cut?))
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
      [(den:parser _p _a _sp _c _dc?)
       (wrong-syntax id "(internal error) late unbound check")]
      ['#f (void)])))

(define (declenv-put-stxclass env id stxclass-name argu [role #f])
  (declenv-check-unbound env id)
  (make-declenv
   (bound-id-table-set (declenv-table env) id
                       (make den:magic-class id stxclass-name argu role))
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
             #:when (or (den:class? v) (den:magic-class? v) (den:parser? v))
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
  (or/c den:lit? den:datum-lit? den:class? den:magic-class? den:parser? den:delayed?))

(define SideClause/c
  (or/c clause:fail? clause:with? clause:attr? clause:do?))

;; ct-phase = syntax, expr that computes absolute phase
;;   usually = #'(syntax-local-phase-level)
(define ct-phase/c syntax?)

(provide (struct-out den:class)
         (struct-out den:magic-class)
         (struct-out den:parser)
         ;; from residual.rkt:
         (struct-out den:lit)
         (struct-out den:datum-lit)
         (struct-out den:delayed))

(provide/contract
 [DeclEnv/c contract?]
 [DeclEntry/c contract?]
 [SideClause/c contract?]
 [ct-phase/c contract?]

 [make-dummy-stxclass (-> identifier? stxclass?)]
 [stxclass-lookup-config (parameter/c (symbols 'no 'try 'yes))]
 [stxclass-colon-notation? (parameter/c boolean?)]

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

 [get-stxclass
  (-> identifier? stxclass?)]
 [get-stxclass/check-arity
  (-> identifier? syntax? exact-nonnegative-integer? (listof keyword?)
      stxclass?)]
 [split-id/get-stxclass
  (-> identifier? DeclEnv/c
      (values identifier? (or/c stxclass? den:lit? den:datum-lit? #f)))])

;; stxclass-lookup-config : (parameterof (U 'no 'try 'yes))
;;  'no means don't lookup, always use dummy (no nested attrs)
;;  'try means lookup, but on failure use dummy (-> nested attrs only from prev.)
;;  'yes means lookup, raise error on failure
(define stxclass-lookup-config (make-parameter 'yes))

;; stxclass-colon-notation? : (parameterof boolean)
;;   if #t, then x:sc notation means (~var x sc)
;;   otherwise, just a var
(define stxclass-colon-notation? (make-parameter #t))

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
  (cond [(and (stxclass-colon-notation?)
              (regexp-match #rx"^([^:]*):(.+)$" (symbol->string (syntax-e id0))))
         => (lambda (m)
              (define-values [src ln col pos span]
                (syntax-srcloc-values id0))
              (define id-str (cadr m))
              (define id-len (string-length id-str))
              (define suffix-str (caddr m))
              (define suffix-len (string-length suffix-str))
              (define id
                (datum->syntax id0 (string->symbol id-str)
                  (list src ln col pos id-len)
                  id0))
              (define suffix
                (datum->syntax id0 (string->symbol suffix-str)
                  (list src ln (and col (+ col id-len 1)) (and pos (+ pos id-len 1)) suffix-len)
                  id0))
              (declenv-check-unbound decls id (syntax-e suffix)
                                     #:blame-declare? #t)
              (let ([suffix-entry (declenv-lookup decls suffix)])
                (cond [(or (den:lit? suffix-entry) (den:datum-lit? suffix-entry))
                       (values id suffix-entry)]
                      [else
                       (let ([sc (get-stxclass/check-arity suffix id0 0 null)])
                         (values id sc))])))]
        [else (values id0 #f)]))

(define (syntax-srcloc-values stx)
  (values (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

;; ----

(provide get-eh-alternative-set)

(define (get-eh-alternative-set id)
  (let ([v (syntax-local-value id (lambda () #f))])
    (unless (eh-alternative-set? v)
      (wrong-syntax id "not defined as an eh-alternative-set"))
    v))
