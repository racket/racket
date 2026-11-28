#lang racket/base
(require (only-in racket/extflonum extflonum?)
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "../namespace/core.rkt"
         "../common/module-path.rkt"
         "../common/phase.rkt"
         "../common/prefab.rkt"
         "../boot/runtime-primitive.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt"
         "require+provide.rkt"
         "../common/performance.rkt")

;; Check whether a module fits the restricted grammar of a cross-phase
;; persistent module

(provide check-cross-phase-persistent-form
         make-module-phaseless!)

(define (check-cross-phase-persistent-form bodys self-mpi)

  (define (check-body bodys)
    (for ([body (in-list bodys)])
      (define p (if (expanded+parsed? body)
                    (expanded+parsed-parsed body)
                    body))
      (cond
        [(parsed-define-values? p)
         (check-expr (parsed-define-values-rhs p) (length (parsed-define-values-syms p)) p)]
        [(or (parsed-#%declare? p)
             (parsed-module? p)
             (syntax? p)) ;; remaining unparsed forms, such as `#%require` and `#%provide`, are ok
         (void)]
        [else
         (disallow p)])))

  (define (check-expr e num-results enclosing)
    (cond
      [(parsed-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-case-lambda? e)
       (check-count 1 num-results enclosing)
       (check-no-disallowed-expr e)]
      [(parsed-quote? e)
       (check-datum (parsed-quote-datum e) e)
       (check-count 1 num-results enclosing)]
      [(parsed-app? e)
       (define rands (parsed-app-rands e))
       (for ([rand (in-list rands)])
         (check-expr rand 1 e))
       (case (cross-phase-primitive-name (parsed-app-rator e))
         [(cons list hasheq)
          (check-count 1 num-results enclosing)]
         [(make-struct-type)
          (check-count 5 num-results enclosing)]
         [(make-struct-type-property)
          (check-count 3 num-results enclosing)]
         [(make-parameter)
          (check-count 1 num-results enclosing)]
         [(gensym)
          (unless (or (= 0 (length rands))
                      (and (= 1 (length rands))
                           (quoted-string? (car rands))))
            (disallow e))]
         [(string->uninterned-symbol)
          (unless (and (= 1 (length rands))
                       (quoted-string? (car rands)))
            (disallow e))]
         [else (disallow e)])]
      [else (check-no-disallowed-expr e)]))

  (define (check-no-disallowed-expr e)
    (cond
      [(parsed-lambda? e)
       (check-body-no-disallowed-expr (parsed-lambda-body e))]
      [(parsed-case-lambda? e)
       (for ([clause (in-list (parsed-case-lambda-clauses e))])
         (check-body-no-disallowed-expr (cadr clause)))]
      ;; explicitly allow (variable-reference-from-unsafe? (#%variable-reference))
      [(and (parsed-app? e)
            (eq? 'variable-reference-from-unsafe? (cross-phase-primitive-name (parsed-app-rator e)))
            (andmap parsed-#%variable-reference? (parsed-app-rands e)))
       (void)]
      [(parsed-app? e)
       (check-no-disallowed-expr (parsed-app-rator e))
       (for ([e (in-list (parsed-app-rands e))])
         (check-no-disallowed-expr e))]
      [(parsed-if? e)
       (check-no-disallowed-expr (parsed-if-tst e))
       (check-no-disallowed-expr (parsed-if-thn e))
       (check-no-disallowed-expr (parsed-if-els e))]
      [(parsed-set!? e)
       (define id (parsed-set!-id e))
       (define normal-b (parsed-id-binding id))
       (when (or (not normal-b)
                 (parsed-top-id? id)
                 (and (not (symbol? normal-b))
                      (eq? (module-binding-module normal-b) self-mpi)))
         (disallow e))
       (check-no-disallowed-expr (parsed-set!-rhs e))]
      [(parsed-with-continuation-mark? e)
       (check-no-disallowed-expr (parsed-with-continuation-mark-key e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-val e))
       (check-no-disallowed-expr (parsed-with-continuation-mark-body e))]
      [(parsed-begin? e)
       (check-body-no-disallowed-expr (parsed-begin-body e))]
      [(parsed-begin0? e)
       (check-body-no-disallowed-expr (parsed-begin0-body e))]
      [(parsed-let_-values? e)
       (for ([clause (in-list (parsed-let_-values-clauses e))])
         (check-no-disallowed-expr (cadr clause)))
       (check-body-no-disallowed-expr (parsed-let_-values-body e))]
      [(or (parsed-quote-syntax? e)
           (parsed-#%variable-reference? e))
       (disallow e)]
      ;; Other forms have no subexpressions
      [else (void)]))

  (define (check-body-no-disallowed-expr l)
    (for ([e (in-list l)])
      (check-no-disallowed-expr e)))

  (check-body bodys))

(define (check-count is-num expected-num enclosing)
  (unless (= is-num expected-num)
    (disallow enclosing)))

; (walk-for-quotable d recur) -> (or/c (not/c #f) #f)
;   d : any/c
;   recur : (any/c . -> . (or/c (not/c #f) #f))
; If `d`'s outermost structure is quotable, then returns `(and (recur u) ...)`
; where `u ...` are the pieces of immediate substructure of d. (If `recur`
; encounters an unquotable expression, it can either return #f or throw; the
; walk-for-quotable invocation will do the same.)
; If `d`'s outermost structure is not quotable, returns #f.
(define (walk-for-quotable d recur)
  (or (number? d)
      (boolean? d)
      (symbol? d)
      (and (string? d) (immutable? d))
      (and (bytes? d) (immutable? d))
      (char? d)
      (keyword? d)
      (null? d)
      (regexp? d)
      (extflonum? d)
      (and (pair? d)
           (recur (car d))
           (recur (cdr d)))
      (and (vector? d)
           (immutable? d)
           (for/and ([v (in-vector d)]) (recur v)))
      (and (hash? d)
           (hash-eq? d)
           (immutable? d)
           (for/and ([(k v) (in-hash d)])
             (and (recur k) (recur v))))
      (and (box? d)
           (immutable? d)
           (recur (unbox d)))
      (and (immutable-prefab-struct-key d)
           (for/and ([v (in-vector (struct->vector d) 1)])
             (recur v)))))

; (check-datum datum expr) -> (or/c (not/c #f) #f)
;   datum : any/c
;   expr : (or/c parsed? syntax?)
; If datum is quotable, returns an unspecified not-#f value. Else, raises an
; error with `e` as the context.
(define (check-datum d e)
  (or (walk-for-quotable d
                         (lambda (d2)
                           (check-datum d2 e)))
      (disallow e)))

(define (quoted-string? e)
  (and (parsed-quote? e)
       (string? (parsed-quote-datum e))))

(define (cross-phase-primitive-name id)
  (cond
    [(parsed-id? id)
     (define b (parsed-id-binding id))
     (and (module-binding? b)
          (eq? runtime-module-name (module-path-index-resolve (module-binding-module b)))
          (module-binding-sym b))]
    [else #f]))

(define (disallow body)
  (raise-syntax-error 'module
                      "not allowed in a cross-phase persistent module"
                      (if (parsed? body)
                          (datum->syntax #f body (parsed-s body))
                          body)))

;; ----------------------------------------

;; Convert a module that is declared `#:unsafe-cross-phase-persistent`
;; so that its inside-edge scope is phaseless. The module can have
;; definitions and imports at any phase except the label phase (and
;; all phase levels of the module are instantiated together).
;; Converting to phaseless merges bindings at all phases, so check
;; that every module-level binding is the same after conversion as
;; before; that is, no symbol can be imported or defined differently
;; at different phases.
(define (make-module-phaseless! inside-scope r+p defined-syms s)
  (define (check-phase phase id-or-sym what)
    (when (label-phase? phase)
      (raise-syntax-error #f
                          (format (string-append "~a at the label phase not allowed in an unsafe"
                                                 " cross-phase persistent module\n"
                                                 "  name: ~a")
                                  what
                                  (if (syntax? id-or-sym) (syntax-e id-or-sym) id-or-sym))
                          s
                          (and (syntax? id-or-sym) id-or-sym))))

  ;; Merging phases can only change the binding of a site if the same
  ;; symbol has a binding site at some other phase, and only if the
  ;; sites can have different bindings; all sites for a symbol that
  ;; are bulk imports from the same provides table have the same
  ;; binding. A first pass finds candidate symbols with minimal
  ;; allocation, recording just the first site's phase and source for
  ;; each symbol; that avoids even collecting sites for every name
  ;; imported from, say, `racket/base` at both phase 0 and phase 1.
  (define firsts (make-hasheq)) ; sym -> (vector phase provides-or-#f prefix-len multi-phase? multi-source?)
  (define candidates (make-hasheq)) ; sym -> #t
  (define (note-site! sym phase provides prefix-len)
    (define first (hash-ref firsts sym #f))
    (cond
      [(not first)
       (hash-set! firsts sym (vector phase provides prefix-len #f #f))]
      [else
       (unless (eqv? phase (vector-ref first 0))
         (vector-set! first 3 #t))
       (unless (and provides
                    (eq? provides (vector-ref first 1))
                    (eqv? prefix-len (vector-ref first 2)))
         (vector-set! first 4 #t))
       ;; Candidate if sites span multiple phases and are not all from
       ;; the same source
       (when (and (vector-ref first 3) (vector-ref first 4))
         (hash-set! candidates sym #t))]))
  (performance-region
   ['expand 'pl-collect]
   (requires+provides-for-each-required-site
    r+p
    (lambda (sym phase provides prefix-len mod-name shift site)
      (when (label-phase? phase)
        (check-phase phase (requires+provides-site-id sym mod-name shift site) "import"))
      (note-site! sym phase provides prefix-len)))
   (requires+provides-for-each-defined-sym
    r+p
    (lambda (sym phase kind)
      (check-phase phase sym "definition")))
   (for* ([(phase ht) (in-hash defined-syms)]
          [(sym id) (in-hash ht)])
     ;; a definition is never the same source as another site
     (note-site! (syntax-e id) phase #f #f)))

  ;; Collect sites for candidate symbols: sym -> list of (vector id phase)
  (define sym-sites (make-hasheq))
  (unless (zero? (hash-count candidates))
    (define (add-site! sym id phase)
      (hash-set! sym-sites sym (cons (vector id phase) (hash-ref sym-sites sym null))))
    (requires+provides-for-each-required-site
     r+p
     (lambda (sym phase provides prefix-len mod-name shift site)
       (when (hash-ref candidates sym #f)
         (add-site! sym (requires+provides-site-id sym mod-name shift site) phase))))
    (for* ([(phase ht) (in-hash defined-syms)]
           [(sym id) (in-hash ht)])
      (when (hash-ref candidates (syntax-e id) #f)
        (add-site! (syntax-e id) id phase))))

  ;; Record bindings before conversion: list of (vector id phase binding)
  (define sites
    (performance-region
     ['expand 'pl-resolve]
     (for*/list ([(sym l) (in-hash sym-sites)]
                 [site (in-list l)])
       (define id (vector-ref site 0))
       (vector id (vector-ref site 1) (resolve+shift id (vector-ref site 1))))))

  (make-shifted-multi-scope-phaseless! inside-scope)

  ;; After conversion, all sites for a symbol have the same binding, so
  ;; a binding can change only if the sites for a symbol had different
  ;; bindings before conversion; check after conversion only in that case
  (define (same-binding/maybe? a b)
    (or (eq? a b)
        (and a
             b
             (or (module-binding? a) (local-binding? a))
             (same-binding? a b))))
  (define disagreeing-syms
    (let ([by-sym (make-hasheq)])
      (for/fold ([syms #hasheq()]) ([site (in-list sites)])
        (define sym (syntax-e (vector-ref site 0)))
        (define b (vector-ref site 2))
        (define prev (hash-ref by-sym sym missing))
        (cond
          [(eq? prev missing)
           (hash-set! by-sym sym b)
           syms]
          [(same-binding/maybe? prev b) syms]
          [else (hash-set syms sym #t)]))))
  (define bad-sites
    (performance-region
     ['expand 'pl-resolve]
     (for/list ([site (in-list sites)]
                #:when (hash-ref disagreeing-syms (syntax-e (vector-ref site 0)) #f)
                #:unless (same-binding/maybe? (vector-ref site 2)
                                              (resolve+shift (vector-ref site 0) (vector-ref site 1))))
       site)))
  (unless (null? bad-sites)
    (define site (car bad-sites))
    (define id (vector-ref site 0))
    (define phase (vector-ref site 1))
    (define before-b (vector-ref site 2))
    (define after-b (resolve+shift id phase))
    (define all-names
      (let loop ([l bad-sites] [seen #hasheq()])
        (cond
          [(null? l) null]
          [else
           (define sym (syntax-e (vector-ref (car l) 0)))
           (if (hash-ref seen sym #f)
               (loop (cdr l) seen)
               (cons sym (loop (cdr l) (hash-set seen sym #t))))])))
    (raise-syntax-error #f
                        (string-append "binding differs across phases in an unsafe cross-phase persistent module;\n"
                                       " a phase-0 binding and a phase-1 binding with the same name must be the same")
                        s
                        id
                        null
                        (format "\n  phase of binding: ~a\n  binding at that phase: ~a\n  merged binding: ~a\n  all conflicting names:~a\n  binding sites for `~a`:~a"
                                phase
                                (describe-binding before-b)
                                (describe-binding after-b)
                                (apply string-append
                                       (for/list ([sym (in-list all-names)])
                                         (format " ~a" sym)))
                                (syntax-e id)
                                (apply string-append
                                       (for/list ([site (in-list sites)]
                                                  #:when (eq? (syntax-e (vector-ref site 0)) (syntax-e id)))
                                         (define site-id (vector-ref site 0))
                                         (format "\n   phase ~a at ~a: ~a"
                                                 (vector-ref site 1)
                                                 (let ([loc (syntax-srcloc site-id)])
                                                   (if (srcloc? loc) (srcloc->string loc) "unknown location"))
                                                 (describe-binding (vector-ref site 2)))))))))

(define missing (gensym))

(define (describe-binding b)
  (cond
    [(not b) "none"]
    [(module-binding? b)
     (format "~a from ~a at phase level ~a"
             (module-binding-sym b)
             (let ([mpi (module-binding-module b)])
               (if (module-path-index? mpi)
                   (resolved-module-path-name (module-path-index-resolve mpi))
                   mpi))
             (module-binding-phase b))]
    [(local-binding? b) "local binding"]
    [else "ambiguous"]))
