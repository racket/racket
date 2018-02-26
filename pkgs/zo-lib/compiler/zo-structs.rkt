#lang racket/base
(require racket/match
         racket/contract
         racket/list
         racket/set)

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
(define-form-struct (predicate-shape struct-shape) ([authentic? boolean?]))
(define-form-struct (accessor-shape struct-shape) ([field-count exact-nonnegative-integer?]
                                                   [authentic? boolean?]))
(define-form-struct (mutator-shape struct-shape) ([field-count exact-nonnegative-integer?]
                                                  [authentic? boolean?]))
(define-form-struct (struct-type-shape struct-shape) ([field-count exact-nonnegative-integer?]
                                                      [authentic? boolean?]))
(define-form-struct (struct-type-property-shape struct-shape) ([has-guard? boolean?]))
(define-form-struct (property-predicate-shape struct-shape) ())
(define-form-struct (property-accessor-shape struct-shape) ())
(define-form-struct (struct-other-shape struct-shape) ())

(define-form-struct form ())
(define-form-struct (expr form) ())

(define-form-struct (toplevel expr) ([depth exact-nonnegative-integer?] 
                                     [pos exact-nonnegative-integer?] 
                                     [const? boolean?] 
                                     [ready? boolean?]))  ; access binding via prefix array (which is on stack)

(define-form-struct (seq expr) ([forms (listof (or/c expr? any/c))])) ; `begin'

(define-form-struct (inline-variant zo) ([direct expr?]
                                         [inline expr?]))

;; Definitions (top level or within module):
(define-form-struct (def-values form) ([ids (listof (or/c toplevel? symbol?))]
                                       [rhs (or/c expr? seq? inline-variant? any/c)]))

(define-form-struct (linkl zo) ([name symbol?]
                                [importss (listof (listof symbol?))]
                                [import-shapess (listof (listof  (or/c #f 'constant 'fixed 
                                                                       function-shape? 
                                                                       struct-shape?)))]
                                [exports (listof symbol?)]
                                [internals (listof (or/c symbol? #f))]
                                [lifts (listof symbol?)]
                                [source-names (hash/c symbol? symbol?)]
                                [body (listof (or/c form? any/c))]
                                [max-let-depth exact-nonnegative-integer?]
                                [need-instance-access? boolean?]))

(define-form-struct (linkl-directory zo) ([table (hash/c (listof symbol?) linkl-bundle?)]))
(define-form-struct (linkl-bundle zo)    ([table (hash/c (or/c symbol? fixnum?)
                                                         any/c)])) ; can be anythingv, but especially a linklet

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


(define-form-struct (application expr) ([rator (or/c expr? seq? any/c)] [rands (listof (or/c expr? seq? any/c))])) ; function call
(define-form-struct (branch expr) ([test (or/c expr? seq? any/c)] [then (or/c expr? seq? any/c)] [else (or/c expr? seq? any/c)])) ; `if'
(define-form-struct (with-cont-mark expr) ([key (or/c expr? seq? any/c)] 
                                           [val (or/c expr? seq? any/c)] 
                                           [body (or/c expr? seq? any/c)])) ; `with-continuation-mark'
(define-form-struct (beg0 expr) ([seq (listof (or/c expr? seq? any/c))])) ; `begin0'
(define-form-struct (varref expr) ([toplevel (or/c toplevel? #f #t symbol?)]
                                   [dummy (or/c toplevel? #f)]
                                   [constant? boolean?]
                                   [from-unsafe? boolean?]))
(define-form-struct (assign expr) ([id toplevel?] [rhs (or/c expr? seq? any/c)] [undef-ok? boolean?])) ; top-level or module-level set!
(define-form-struct (apply-values expr) ([proc (or/c expr? seq? any/c)] [args-expr (or/c expr? seq? any/c)])) ; `(call-with-values (lambda () ,args-expr) ,proc)
(define-form-struct (with-immed-mark expr) ([key (or/c expr? seq? any/c)] 
                                            [def-val (or/c expr? seq? any/c)] 
                                            [body (or/c expr? seq? any/c)]))
(define-form-struct (primval expr) ([id exact-nonnegative-integer?])) ; direct preference to a kernel primitive

;; For backward compatibility, provide limited matching support as `compilation-top`:
(provide compilation-top)
(require (for-syntax racket/base))
(define-match-expander compilation-top
  (lambda (stx)
    (syntax-case stx ()
      [(_ max-let-depth binding-namess prefix code)
       #'(linkl-directory (hash-table ('() (linkl-bundle
                                            (hash-table (0 (linkl _ ; name
                                                                  _ ; imports
                                                                  _ ; import shapes
                                                                  _ ; exports
                                                                  _ ; internals
                                                                  _ ; lifts
                                                                  _ ; source-names
                                                                  (list code) ; body
                                                                  max-let-depth
                                                                  _))
                                                        _ (... ...))))
                                      _ (... ...)))]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ max-let-depth binding-namess prefix code)
       #'(linkl-directory (hash '() (linkl-bundle
                                     (hasheq 0 (linkl 'top
                                                      '()
                                                      '()
                                                      '()
                                                      '()
                                                      '()
                                                      #hasheq()
                                                      (list code)
                                                      (add1 max-let-depth)
                                                      #f)))))])))
