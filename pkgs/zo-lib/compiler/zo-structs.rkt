#lang racket/base
(require racket/match
         racket/contract
         racket/list
         racket/set)

#| Unresolved issues

  what are the booleans in lexical-rename?

  contracts that are probably too generous:
  prefix-stxs
  provided-nom-src
  lam-num-params
  lexical-rename-alist
  all-from-module

|#

;; ----------------------------------------
;;  Structures to represent bytecode

(define-syntax-rule (define-form-struct* id id+par ([field-id field-contract . options] ...))
  (begin
    (define-struct id+par ([field-id . options] ...) #:prefab)
    (provide
     (contract-out
      [struct id ([field-id field-contract] ...)]))))

(define-struct zo () #:prefab)
(provide (struct-out zo))

(define-syntax define-form-struct
  (syntax-rules ()
    [(_ (id sup) . rest)
     (define-form-struct* id (id sup) . rest)]
    [(_ id . rest)
     (define-form-struct* id (id zo) . rest)]))

(define-form-struct function-shape ([arity procedure-arity?]
                                    [preserves-marks? boolean?]))

(define-form-struct struct-shape ())
(define-form-struct (constructor-shape struct-shape) ([arity exact-nonnegative-integer?]))
(define-form-struct (predicate-shape struct-shape) ())
(define-form-struct (accessor-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (mutator-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (struct-type-shape struct-shape) ([field-count exact-nonnegative-integer?]))
(define-form-struct (struct-other-shape struct-shape) ())

;; In toplevels of resove prefix:
(define-form-struct global-bucket ([name symbol?])) ; top-level binding
(define-form-struct module-variable ([modidx module-path-index?] 
                                     [sym symbol?] 
                                     [pos exact-integer?] 
                                     [phase exact-nonnegative-integer?]
                                     [constantness (or/c #f 'constant 'fixed 
                                                         function-shape? 
                                                         struct-shape?)]))

(define-form-struct prefix ([num-lifts exact-nonnegative-integer?] 
                            [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))] 
                            [stxs (listof (or/c #f stx?))] ; #f is unusual, but it can happen when one is optimized away at the last moment
                            [src-inspector-desc symbol?]))

(define-form-struct form ())
(define-form-struct (expr form) ())

(define-form-struct compilation-top ([max-let-depth exact-nonnegative-integer?]
                                     [binding-namess (hash/c exact-nonnegative-integer?
                                                             (hash/c symbol? stx?))]
                                     [prefix prefix?]
                                     [code (or/c form? any/c)])) ; compiled code always wrapped with this

;; A provided identifier
(define-form-struct provided ([name symbol?] 
                              [src (or/c module-path-index? #f)] 
                              [src-name symbol?] 
                              [nom-src any/c] ; should be (or/c module-path-index? #f)
                              [src-phase exact-nonnegative-integer?] 
                              [protected? boolean?]))

(define-form-struct (toplevel expr) ([depth exact-nonnegative-integer?] 
                                     [pos exact-nonnegative-integer?] 
                                     [const? boolean?] 
                                     [ready? boolean?]))  ; access binding via prefix array (which is on stack)

(define-form-struct (seq form) ([forms (listof (or/c form? any/c))])) ; `begin'
(define-form-struct (seq-for-syntax form) ([forms (listof (or/c form? any/c))] ; `begin-for-syntax'
                                           [prefix prefix?] 
                                           [max-let-depth exact-nonnegative-integer?]
                                           [dummy (or/c toplevel? #f)]))

(define-form-struct (inline-variant form) ([direct expr?]
                                           [inline expr?]))

;; Definitions (top level or within module):
(define-form-struct (def-values form) ([ids (listof (or/c toplevel? symbol?))]
                                       [rhs (or/c expr? seq? inline-variant? any/c)])) 
(define-form-struct (def-syntaxes form) ([ids (listof (or/c toplevel? symbol?))]
                                         [rhs (or/c expr? seq? any/c)] 
                                         [prefix prefix?] 
                                         [max-let-depth exact-nonnegative-integer?]
                                         [dummy (or/c toplevel? #f)]))

(define-form-struct (mod form) ([name (or/c symbol? (listof symbol?))]
                                [srcname symbol?]
                                [self-modidx module-path-index?]
                                [prefix prefix?]
                                [provides (listof (list/c (or/c exact-integer? #f)
                                                          (listof provided?)
                                                          (listof provided?)))]
                                [requires (listof (cons/c (or/c exact-integer? #f)
                                                          (listof module-path-index?)))]
                                [body (listof (or/c form? any/c))]
                                [syntax-bodies (listof (cons/c exact-positive-integer?
                                                               (listof (or/c def-syntaxes? seq-for-syntax?))))]
                                [unexported (listof (list/c exact-nonnegative-integer?
                                                            (listof symbol?)
                                                            (listof symbol?)))]
                                [max-let-depth exact-nonnegative-integer?]
                                [dummy toplevel?]
                                [lang-info (or/c #f (vector/c module-path? symbol? any/c))]
                                [internal-context (or/c #f #t stx? (vectorof stx?))]
                                [binding-names (hash/c exact-integer?
                                                       (hash/c symbol? (or/c #t stx?)))]
                                [flags (listof (or/c 'cross-phase))]
                                [pre-submodules (listof mod?)]
                                [post-submodules (listof mod?)]))

(define-form-struct (lam expr) ([name (or/c symbol? vector? empty?)]
                                [flags (listof (or/c 'preserves-marks 'is-method 'single-result
                                                     'only-rest-arg-not-used 'sfs-clear-rest-args))]
                                [num-params exact-nonnegative-integer?]
                                [param-types (listof (or/c 'val 'ref 'flonum 'fixnum 'extflonum))]
                                [rest? boolean?]
                                [closure-map (vectorof exact-nonnegative-integer?)]
                                [closure-types (listof (or/c 'val/ref 'flonum 'fixnum 'extflonum))]
                                [toplevel-map (or/c #f (set/c exact-nonnegative-integer?))]
                                [max-let-depth exact-nonnegative-integer?]
                                [body (or/c expr? seq? any/c)])) ; `lambda'
(define-form-struct (closure expr) ([code lam?] [gen-id symbol?])) ; a static closure (nothing to close over)
(define-form-struct (case-lam expr) ([name (or/c symbol? vector? empty?)] [clauses (listof (or/c lam? closure?))]))

(define-form-struct (let-one expr) ([rhs (or/c expr? seq? any/c)]  ; pushes one value onto stack
                                    [body (or/c expr? seq? any/c)] 
                                    [type (or/c #f 'flonum 'fixnum 'extflonum)]
                                    [unused? boolean?]))
(define-form-struct (let-void expr) ([count exact-nonnegative-integer?] [boxes? boolean?] [body (or/c expr? seq? any/c)])) ; create new stack slots
(define-form-struct (install-value expr) ([count exact-nonnegative-integer?] 
                                          [pos exact-nonnegative-integer?] 
                                          [boxes? boolean?] 
                                          [rhs (or/c expr? seq? any/c)] 
                                          [body (or/c expr? seq? any/c)])) ; set existing stack slot(s)
(define-form-struct (let-rec expr) ([procs (listof lam?)] [body (or/c expr? seq? any/c)])) ; put `letrec'-bound closures into existing stack slots
(define-form-struct (boxenv expr) ([pos exact-nonnegative-integer?] [body (or/c expr? seq? any/c)])) ; box existing stack element

(define-form-struct (localref expr) ([unbox? boolean?] 
                                     [pos exact-nonnegative-integer?] 
                                     [clear? boolean?] 
                                     [other-clears? boolean?] 
                                     [type (or/c #f 'flonum 'fixnum 'extflonum)])) ; access local via stack


(define-form-struct (topsyntax expr) ([depth exact-nonnegative-integer?] [pos exact-nonnegative-integer?] [midpt exact-nonnegative-integer?])) ; access syntax object via prefix array (which is on stack)

(define-form-struct (application expr) ([rator (or/c expr? seq? any/c)] [rands (listof (or/c expr? seq? any/c))])) ; function call
(define-form-struct (branch expr) ([test (or/c expr? seq? any/c)] [then (or/c expr? seq? any/c)] [else (or/c expr? seq? any/c)])) ; `if'
(define-form-struct (with-cont-mark expr) ([key (or/c expr? seq? any/c)] 
                                           [val (or/c expr? seq? any/c)] 
                                           [body (or/c expr? seq? any/c)])) ; `with-continuation-mark'
(define-form-struct (beg0 expr) ([seq (listof (or/c expr? seq? any/c))])) ; `begin0'
(define-form-struct (splice form) ([forms (listof (or/c form? any/c))])) ; top-level `begin'
(define-form-struct (varref expr) ([toplevel (or/c toplevel? #t)] [dummy (or/c toplevel? #f)])) ; `#%variable-reference'
(define-form-struct (assign expr) ([id toplevel?] [rhs (or/c expr? seq? any/c)] [undef-ok? boolean?])) ; top-level or module-level set!
(define-form-struct (apply-values expr) ([proc (or/c expr? seq? any/c)] [args-expr (or/c expr? seq? any/c)])) ; `(call-with-values (lambda () ,args-expr) ,proc)
(define-form-struct (with-immed-mark expr) ([key (or/c expr? seq? any/c)] 
                                            [def-val (or/c expr? seq? any/c)] 
                                            [body (or/c expr? seq? any/c)]))
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive

;; Top-level `require'
(define-form-struct (req form) ([reqs stx?] [dummy toplevel?]))


;; Syntax objects

(define-form-struct stx ([content stx-obj?]))

(define-form-struct stx-obj ([datum any/c] ; S-expression with `wrapped` components
                             [wrap any/c] ; should be `wrap?`, but encoded form appears initially
                             [srcloc any/c] ; should be `(or/c #f srcloc?)`, but encoded form appears initially
                             [props (hash/c symbol? any/c)]
                             [tamper-status (or/c 'clean 'armed 'tainted)]))

(define-form-struct wrap ([shifts (listof module-shift?)]
                          [simple-scopes (listof scope?)]
                          [multi-scopes (listof (list/c multi-scope? (or/c #f exact-integer? (box/c exact-integer?))))]))

(define-form-struct module-shift ([from (or/c #f module-path-index?)]
                                  [to (or/c #f module-path-index?)]
                                  [from-inspector-desc (or/c #f symbol?)]
                                  [to-inspector-desc (or/c #f symbol?)]))

(define-form-struct scope ([name (or/c 'root exact-nonnegative-integer?)] ; 'root is special; otherwise, just for printing
                           [kind symbol?]
                           [bindings (listof (list/c symbol? (listof scope?) binding?)) #:mutable]
                           [bulk-bindings (listof (list/c (listof scope?) all-from-module?)) #:mutable]
                           [multi-owner (or/c #f multi-scope?) #:mutable]))
(define-form-struct multi-scope ([name exact-nonnegative-integer?]
                                 [src-name any/c] ; debugging info, such as module name
                                 [scopes (listof (list/c (or/c #f exact-integer?) scope?)) #:mutable]))

(define-form-struct binding ())
(define-form-struct (free-id=?-binding binding) ([base (and/c binding?
                                                              (not/c free-id=?-binding?))]
                                                 [id stx-obj?]
                                                 [phase (or/c #f exact-integer?)]))
(define-form-struct (local-binding binding) ([name symbol?]))
(define-form-struct (module-binding binding) ([encoded any/c]))
;; Convert `module-binding` to `decoded-module-binding` with `decode-module-binding`:
(define-form-struct (decoded-module-binding binding) ([path (or/c #f module-path-index?)]
                                                      [name symbol?]
                                                      [phase exact-integer?]
                                                      [nominal-path (or/c #f module-path-index?)]
                                                      [nominal-export-name symbol?]
                                                      [nominal-phase (or/c #f exact-integer?)]
                                                      [import-phase (or/c #f exact-integer?)]
                                                      [inspector-desc (or/c #f symbol?)]))

(define-form-struct all-from-module ([path module-path-index?] 
                                     [phase (or/c exact-integer? #f)] 
                                     [src-phase (or/c exact-integer? #f)]
                                     [inspector-desc symbol?]
                                     [exceptions (listof symbol?)]
                                     [prefix (or/c symbol? #f)]))
