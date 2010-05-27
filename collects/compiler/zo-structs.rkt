#lang scheme/base
(require mzlib/etc 
         scheme/match
         scheme/contract
         scheme/list)

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

(define-syntax-rule (define-form-struct* id id+par ([field-id field-contract] ...))
  (begin
    (define-struct id+par (field-id ...) #:prefab)
    (provide/contract
     [struct id ([field-id field-contract] ...)])))

(define-syntax define-form-struct
  (syntax-rules ()
    [(_ (id sup) . rest)
     (define-form-struct* id (id sup) . rest)]
    [(_ id . rest)
     (define-form-struct* id id . rest)]))

;; In toplevels of resove prefix:
(define-form-struct global-bucket ([name symbol?])) ; top-level binding
(define-form-struct module-variable ([modidx module-path-index?] 
                                     [sym symbol?] 
                                     [pos exact-integer?] 
                                     [phase (or/c 0 1)])) ; direct access to exported id

;; Syntax object
(define-form-struct wrap ())
(define-form-struct wrapped ([datum any/c] 
                             [wraps (listof wrap?)] 
                             [certs (or/c list? #f)]))

;; In stxs of prefix:
(define-form-struct stx ([encoded wrapped?]))

(define-form-struct prefix ([num-lifts exact-nonnegative-integer?] 
                            [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))] 
                            [stxs list?]))   ; should be (listof stx?) sets up top-level and syntax-object array

(define-form-struct form ())
(define-form-struct (expr form) ())

;; A static closure can refer directly to itself, creating a cycle
(define-struct indirect ([v #:mutable]) #:prefab)

(define-form-struct compilation-top ([max-let-depth exact-nonnegative-integer?] [prefix prefix?] [code (or/c form? indirect? any/c)])) ; compiled code always wrapped with this

;; A provided identifier
(define-form-struct provided ([name symbol?] 
                              [src (or/c module-path-index? #f)] 
                              [src-name symbol?] 
                              [nom-src any/c] ; should be (or/c module-path-index? #f)
                              [src-phase (or/c 0 1)] 
                              [protected? boolean?] 
                              [insp (or/c boolean? void?)]))

(define-form-struct (toplevel expr) ([depth exact-nonnegative-integer?] 
                                     [pos exact-nonnegative-integer?] 
                                     [const? boolean?] 
                                     [ready? boolean?]))  ; access binding via prefix array (which is on stack)

(define-form-struct (seq form) ([forms (listof (or/c form? indirect? any/c))])) ; `begin'

;; Definitions (top level or within module):
(define-form-struct (def-values form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?
                                       [rhs (or/c expr? seq? indirect? any/c)])) 
(define-form-struct (def-syntaxes form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?                                        
                                         [rhs (or/c expr? seq? indirect? any/c)] 
                                         [prefix prefix?] 
                                         [max-let-depth exact-nonnegative-integer?])) 
(define-form-struct (def-for-syntax form) ([ids (listof (or/c toplevel? symbol?))] ; added symbol?
                                           [rhs (or/c expr? seq? indirect? any/c)] 
                                           [prefix prefix?] 
                                           [max-let-depth exact-nonnegative-integer?])) 

(define-form-struct (mod form) ([name symbol?] 
                                [srcname symbol?]
                                [self-modidx module-path-index?] 
                                [prefix prefix?] 
                                [provides (listof (list/c (or/c exact-integer? #f)
                                                          (listof provided?)
                                                          (listof provided?)))] 
                                [requires (listof (cons/c (or/c exact-integer? #f)
                                                          (listof module-path-index?)))]
                                [body (listof (or/c form? indirect? any/c))] 
                                [syntax-body (listof (or/c def-syntaxes? def-for-syntax?))] 
                                [unexported (list/c (listof symbol?) (listof symbol?)
                                                    (listof symbol?))] 
                                [max-let-depth exact-nonnegative-integer?]
                                [dummy toplevel?]
                                [lang-info (or/c #f (vector/c module-path? symbol? any/c))]
                                [internal-context (or/c #f #t stx?)]))

(define-form-struct (lam expr) ([name (or/c symbol? vector? empty?)]
                                [flags (listof (or/c 'preserves-marks 'is-method 'single-result))]
                                [num-params integer?] ; should be exact-nonnegative-integer?
                                [param-types (listof (or/c 'val 'ref 'flonum))]
                                [rest? boolean?]
                                [closure-map (vectorof exact-nonnegative-integer?)]
                                [closure-types (listof (or/c 'val/ref 'flonum))]
                                [max-let-depth exact-nonnegative-integer?]
                                [body (or/c expr? seq? indirect? any/c)])) ; `lambda'
(define-form-struct (closure expr) ([code lam?] [gen-id symbol?])) ; a static closure (nothing to close over)
(define-form-struct (case-lam expr) ([name (or/c symbol? vector? empty?)] [clauses (listof (or/c lam? indirect?))])) ; each clause is a lam (added indirect)

(define-form-struct (let-one expr) ([rhs (or/c expr? seq? indirect? any/c)] [body (or/c expr? seq? indirect? any/c)] [flonum? boolean?] [unused? boolean?])) ; pushes one value onto stack
(define-form-struct (let-void expr) ([count exact-nonnegative-integer?] [boxes? boolean?] [body (or/c expr? seq? indirect? any/c)])) ; create new stack slots
(define-form-struct (install-value expr) ([count exact-nonnegative-integer?] 
                                          [pos exact-nonnegative-integer?] 
                                          [boxes? boolean?] 
                                          [rhs (or/c expr? seq? indirect? any/c)] 
                                          [body (or/c expr? seq? indirect? any/c)])) ; set existing stack slot(s)
(define-form-struct (let-rec expr) ([procs (listof lam?)] [body (or/c expr? seq? indirect? any/c)])) ; put `letrec'-bound closures into existing stack slots
(define-form-struct (boxenv expr) ([pos exact-nonnegative-integer?] [body (or/c expr? seq? indirect? any/c)])) ; box existing stack element

(define-form-struct (localref expr) ([unbox? boolean?] [pos exact-nonnegative-integer?] [clear? boolean?] [other-clears? boolean?] [flonum? boolean?])) ; access local via stack


(define-form-struct (topsyntax expr) ([depth exact-nonnegative-integer?] [pos exact-nonnegative-integer?] [midpt exact-nonnegative-integer?])) ; access syntax object via prefix array (which is on stack)

(define-form-struct (application expr) ([rator (or/c expr? seq? indirect? any/c)] [rands (listof (or/c expr? seq? indirect? any/c))])) ; function call
(define-form-struct (branch expr) ([test (or/c expr? seq? indirect? any/c)] [then (or/c expr? seq? indirect? any/c)] [else (or/c expr? seq? indirect? any/c)])) ; `if'
(define-form-struct (with-cont-mark expr) ([key (or/c expr? seq? indirect? any/c)] 
                                           [val (or/c expr? seq? indirect? any/c)] 
                                           [body (or/c expr? seq? indirect? any/c)])) ; `with-continuation-mark'
(define-form-struct (beg0 expr) ([seq (listof (or/c expr? seq? indirect? any/c))])) ; `begin0'
(define-form-struct (splice form) ([forms (listof (or/c form? indirect? any/c))])) ; top-level `begin'
(define-form-struct (varref expr) ([toplevel toplevel?])) ; `#%variable-reference'
(define-form-struct (assign expr) ([id toplevel?] [rhs (or/c expr? seq? indirect? any/c)] [undef-ok? boolean?])) ; top-level or module-level set!
(define-form-struct (apply-values expr) ([proc (or/c expr? seq? indirect? any/c)] [args-expr (or/c expr? seq? indirect? any/c)])) ; `(call-with-values (lambda () ,args-expr) ,proc)
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive

;; Top-level `require'
(define-form-struct (req form) ([reqs syntax?] [dummy toplevel?]))

(define-form-struct (lexical-rename wrap) ([bool1 boolean?] ; this needs a name
                                           [bool2 boolean?] ; this needs a name
                                           [alist any/c])) ; should be (listof (cons/c symbol? symbol?))
(define-form-struct (phase-shift wrap) ([amt exact-integer?] [src (or/c module-path-index? #f)] [dest (or/c module-path-index? #f)]))
(define-form-struct (wrap-mark wrap) ([val exact-integer?]))
(define-form-struct (prune wrap) ([sym any/c]))

(define-form-struct all-from-module ([path module-path-index?] 
                                     [phase (or/c exact-integer? #f)] 
                                     [src-phase any/c] ; should be (or/c exact-integer? #f)
                                     [exceptions list?] ; should be (listof symbol?)
                                     [prefix any/c])) ; should be (or/c symbol? #f)

(define-form-struct nominal-path ())
(define-form-struct (simple-nominal-path nominal-path) ([value module-path-index?]))
(define-form-struct (imported-nominal-path nominal-path) ([value module-path-index?] 
                                                          [import-phase exact-integer?]))
(define-form-struct (phased-nominal-path nominal-path) ([value module-path-index?]
                                                        [import-phase (or/c false/c exact-integer?)]
                                                        [phase exact-integer?]))

(define-form-struct module-binding ())
(define-form-struct (phased-module-binding module-binding) ([path module-path-index?]
                                                            [phase exact-integer?]
                                                            [export-name any/c]
                                                            [nominal-path nominal-path?]
                                                            [nominal-export-name any/c]))
(define-form-struct (exported-nominal-module-binding module-binding) ([path module-path-index?]
                                                                      [export-name any/c]
                                                                      [nominal-path nominal-path?]
                                                                      [nominal-export-name any/c]))
(define-form-struct (nominal-module-binding module-binding) ([path module-path-index?]
                                                             [nominal-path nominal-path?]))
(define-form-struct (exported-module-binding module-binding) ([path module-path-index?]
                                                              [export-name any/c]))
(define-form-struct (simple-module-binding module-binding) ([path module-path-index?]))

(define-form-struct (module-rename wrap) ([phase (or/c exact-integer? #f)] 
                                          [kind (or/c 'marked 'normal)] 
                                          [set-id any/c] 
                                          [unmarshals (listof all-from-module?)]
                                          [renames (listof (cons/c symbol? module-binding?))] 
                                          [mark-renames any/c] 
                                          [plus-kern? boolean?]))

(provide/contract (struct indirect ([v (or/c closure? #f)])))





