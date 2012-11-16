#lang scheme/base
(require mzlib/etc 
         scheme/match
         scheme/contract
         scheme/list
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

(define-syntax-rule (define-form-struct* id id+par ([field-id field-contract] ...))
  (begin
    (define-struct id+par (field-id ...) #:prefab)
    #;(provide (struct-out id))
    (provide/contract
     [struct id ([field-id field-contract] ...)])))

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

;; Syntax object
(define ((alist/c k? v?) l)
  (let loop ([l l])
    (match l
      [(list) #t]
      [(list* (? k?) (? v?) l)
       (loop l)]
      [_ #f])))

(define mark-map? 
  (alist/c number? module-path-index?)
  #;(hash/c number? module-path-index?))

(define-form-struct wrap ())
(define-form-struct wrapped ([datum any/c] 
                             [wraps (listof wrap?)] 
                             [tamper-status (or/c 'clean 'armed 'tainted)]))

;; In stxs of prefix:
(define-form-struct stx ([encoded wrapped?]))

(define-form-struct prefix ([num-lifts exact-nonnegative-integer?] 
                            [toplevels (listof (or/c #f symbol? global-bucket? module-variable?))] 
                            [stxs list?]))   ; should be (listof stx?) sets up top-level and syntax-object array

(define-form-struct form ())
(define-form-struct (expr form) ())

(define-form-struct compilation-top ([max-let-depth exact-nonnegative-integer?] [prefix prefix?] [code (or/c form? any/c)])) ; compiled code always wrapped with this

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
                                [pre-submodules (listof mod?)]
                                [post-submodules (listof mod?)]))

(define-form-struct (lam expr) ([name (or/c symbol? vector? empty?)]
                                [flags (listof (or/c 'preserves-marks 'is-method 'single-result
                                                     'only-rest-arg-not-used 'sfs-clear-rest-args))]
                                [num-params exact-nonnegative-integer?]
                                [param-types (listof (or/c 'val 'ref 'flonum 'fixnum))]
                                [rest? boolean?]
                                [closure-map (vectorof exact-nonnegative-integer?)]
                                [closure-types (listof (or/c 'val/ref 'flonum 'fixnum))]
                                [toplevel-map (or/c #f (set/c exact-nonnegative-integer?))]
                                [max-let-depth exact-nonnegative-integer?]
                                [body (or/c expr? seq? any/c)])) ; `lambda'
(define-form-struct (closure expr) ([code lam?] [gen-id symbol?])) ; a static closure (nothing to close over)
(define-form-struct (case-lam expr) ([name (or/c symbol? vector? empty?)] [clauses (listof (or/c lam? closure?))]))

(define-form-struct (let-one expr) ([rhs (or/c expr? seq? any/c)]  ; pushes one value onto stack
                                    [body (or/c expr? seq? any/c)] 
                                    [type (or/c #f 'flonum 'fixnum)]
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
                                     [type (or/c #f 'flonum 'fixnum)])) ; access local via stack


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
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive

;; Top-level `require'
(define-form-struct (req form) ([reqs stx?] [dummy toplevel?]))


(define-form-struct free-id-info ([path0 module-path-index?]
                                  [symbol0 symbol?]
                                  [path1 module-path-index?]
                                  [symbol1 symbol?]
                                  [phase0 (or/c exact-integer? #f)]
                                  [phase1 (or/c exact-integer? #f)]
                                  [phase2 (or/c exact-integer? #f)]
                                  [use-current-inspector? boolean?]))

(define-form-struct (lexical-rename wrap) ([has-free-id-renames? boolean?]
                                           [bool2 boolean?] ; this needs a name
                                           [alist (listof 
                                                   (cons/c symbol?
                                                           (or/c
                                                            symbol?
                                                            (cons/c
                                                             symbol?
                                                             (or/c
                                                              (cons/c symbol? (or/c symbol? #f))
                                                              free-id-info?)))))])) 
(define-form-struct (phase-shift wrap) ([amt (or/c exact-integer? #f)] 
                                        [src (or/c module-path-index? #f)] 
                                        [dest (or/c module-path-index? #f)]
                                        [cancel-id (or/c exact-integer? #f)]))
(define-form-struct (wrap-mark wrap) ([val exact-integer?]))
(define-form-struct (prune wrap) ([sym any/c]))

(define-form-struct all-from-module ([path module-path-index?] 
                                     [phase (or/c exact-integer? #f)] 
                                     [src-phase (or/c exact-integer? #f)]
                                     [exceptions (listof symbol?)]
                                     [prefix (or/c symbol? #f)]
                                     [context (or/c (listof exact-integer?) 
                                                    (vector/c (listof exact-integer?) any/c))]))

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

; XXX better name for 'flag'
(define-form-struct (top-level-rename wrap) ([flag boolean?]))

; XXX better name for 'value'
(define-form-struct (mark-barrier wrap) ([value symbol?]))







