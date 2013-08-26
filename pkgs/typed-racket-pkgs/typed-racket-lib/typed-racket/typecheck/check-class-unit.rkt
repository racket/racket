#lang racket/unit

;; This module provides a unit for type-checking classes

(require "../utils/utils.rkt"
         racket/dict
         racket/format
         racket/match
         racket/pretty ;; DEBUG ONLY
         racket/set
         syntax/parse
         "signatures.rkt"
         "tc-metafunctions.rkt"
         "tc-funapp.rkt"
         "tc-subst.rkt"
         (prefix-in c: racket/class)
         (private parse-type syntax-properties type-annotation)
         (base-env class-prims)
         (env lexical-env tvar-env)
         (types utils abbrev union subtype resolve)
         (typecheck check-below internal-forms)
         (utils tc-utils)
         (rep type-rep)
         (for-syntax racket/base)
         (for-template racket/base
                       (prefix-in c: racket/class)
                       (base-env class-prims)
                       (typecheck internal-forms)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-class^)

;; time debugging
(define-syntax do-timing #f)
(define start-time (make-parameter 0))
(define-syntax (with-timing stx)
  (syntax-case stx ()
    [(with-timing e ...)
     (if (syntax-local-value #'do-timing)
         #'(begin
             (log-info "TR class start timing")
             (parameterize ([start-time (current-inexact-milliseconds)])
               e ...))
         #'(begin e ...))]))

(define (do-timestamp [str ""])
  (log-info (format "TR class time @ ~a: ~a"
                    str (- (current-inexact-milliseconds) (start-time)))))

;; Syntax classes for use in functions below
(define-syntax-class name-pair
  (pattern (internal:id external:id)))

(define-syntax-class internal-class-data
  #:literals (#%plain-app quote-syntax class-internal begin
              values c:init c:init-field optional-init c:field
              c:public c:override c:private c:inherit c:inherit-field
              private-field c:augment c:pubment)
  (pattern (begin (quote-syntax
                   (class-internal
                    (#:forall type-parameter:id ...)
                    (c:init init-names:name-pair ...)
                    (c:init-field init-field-names:name-pair ...)
                    (optional-init optional-names:id ...)
                    (c:field field-names:name-pair ...)
                    (c:public public-names:name-pair ...)
                    (c:override override-names:name-pair ...)
                    (c:private privates:id ...)
                    (private-field private-fields:id ...)
                    (c:inherit inherit-names:name-pair ...)
                    (c:inherit-field inherit-field-names:name-pair ...)
                    (c:augment augment-names:name-pair ...)
                    (c:pubment pubment-names:name-pair ...)))
                  (#%plain-app values))
           #:with type-parameters #'(type-parameter ...)
           #:with init-internals #'(init-names.internal ...)
           #:with init-externals #'(init-names.external ...)
           #:with init-field-internals #'(init-field-names.internal ...)
           #:with init-field-externals #'(init-field-names.external ...)
           #:with optional-inits #'(optional-names ...)
           #:with field-internals #'(field-names.internal ...)
           #:with field-externals #'(field-names.external ...)
           #:with public-internals #'(public-names.internal ...)
           #:with public-externals #'(public-names.external ...)
           #:with override-internals #'(override-names.internal ...)
           #:with override-externals #'(override-names.external ...)
           #:with inherit-externals #'(inherit-names.external ...)
           #:with inherit-internals #'(inherit-names.internal ...)
           #:with inherit-field-externals #'(inherit-field-names.external ...)
           #:with inherit-field-internals #'(inherit-field-names.internal ...)
           #:with augment-externals #'(augment-names.external ...)
           #:with augment-internals #'(augment-names.internal ...)
           #:with pubment-externals #'(pubment-names.external ...)
           #:with pubment-internals #'(pubment-names.internal ...)
           #:with private-names #'(privates ...)
           #:with private-field-names #'(private-fields ...)))

(define-syntax-class initializer-body
  #:literals (letrec-syntaxes+values)
  #:attributes (val)
  (pattern (letrec-syntaxes+values _ _ body:initializer-body)
           #:with val #'body.val)
  (pattern (letrec-syntaxes+values _ _
             (~and e0 (~not letrec-syntaxes+values))
             e:expr ...)
           #:with val #'(e0 e ...)))

(define-syntax-class initializer-class
  #:literals (#%plain-lambda)
  #:attributes (initializer-body initializer-self-id
                initializer-args-id)
  (pattern (#%plain-lambda
            (self:id super-go:id si_c:id si_inited?:id
                     si_leftovers:id init-args:id)
            body:initializer-body)
           #:with initializer-body #'body.val
           #:with initializer-self-id #'self
           #:with initializer-args-id #'init-args))

(define-syntax-class make-methods-body
  #:literals (let-values letrec-syntaxes+values #%plain-app values)
  #:attributes (initializer-body initializer-self-id
                initializer-args-id)
  (pattern (letrec-values _
            (#%plain-app
             values
             public:expr
             override:expr
             augride:expr
             :initializer-class)))
  (pattern (let-values () :make-methods-body))
  (pattern (letrec-syntaxes+values _ _ :make-methods-body)))

(define-syntax-class make-methods-class
  #:literals (let-values #%plain-lambda)
  #:attributes (initializer-body initializer-self-id
                initializer-args-id)
  (pattern (#%plain-lambda
            (local-accessor:id local-mutator:id local-method-or-field:id ...)
            (let-values ([(field-name:id) accessor-or-mutator] ...)
              :make-methods-body))))

(define-syntax-class class-expansion
  #:literals (let-values letrec-syntaxes+values #%plain-app)
  #:attributes (superclass-expr
                type-parameters
                init-internals init-externals
                init-field-internals init-field-externals
                optional-inits
                field-internals field-externals
                public-internals public-externals
                override-internals override-externals
                inherit-internals inherit-externals
                inherit-field-internals inherit-field-externals
                augment-internals augment-externals
                pubment-internals pubment-externals
                private-names private-field-names
                make-methods
                initializer-body
                initializer-self-id
                initializer-args-id)
  (pattern (let-values ()
             (letrec-syntaxes+values
              ()
              ((() ;; residual class: data
                   :internal-class-data))
              (let-values (((superclass:id) superclass-expr)
                           ((interfaces:id) interface-expr))
                (#%plain-app
                 compose-class:id
                 internal:expr ...
                 (~and make-methods :make-methods-class)
                 (quote #f)))))))

;; Syntax TCResults -> Type
;; Type-check a class form by trawling its innards
;;
;; Assumptions:
;;  by the time this is called, we can be sure that
;;  method clauses match up to a corresponding definition
;;  by the local-expansion done by `class`
;;
;;  we know by this point that #'form is an actual typed
;;  class produced by `class` due to the syntax property
(define (check-class form [expected #f])
  (match (and expected (resolve expected))
    [(tc-result1: (and self-class-type (Class: _ _ _ _ _)))
     (parse-and-check form self-class-type)]
    [(tc-result1: (Poly-names: ns body-type))
     ;; FIXME: this case probably isn't quite right
     (check-class form (ret body-type))]
    [#f (parse-and-check form #f)]
    [_ (check-below (parse-and-check form #f) expected)]))

;; Syntax Option<Type> -> Type
;; Parse the syntax and extract useful information to pass to the
;; main type-checking helper function
(define (parse-and-check form expected)
  (syntax-parse form
    ;; Inspect the expansion of the class macro for the pieces that
    ;; we need to type-check like superclass, methods, top-level
    ;; expressions and so on
    [cls:class-expansion
     ;; Make sure the superclass is a class
     ;; FIXME: maybe should check the property on this expression
     ;;        as a sanity check too
     (define super-type (tc-expr #'cls.superclass-expr))
     ;; Save parse attributes to pass through to helper functions
     (define type-parameters (syntax->datum #'cls.type-parameters))
     (define fresh-parameters (map gensym type-parameters))
     (define parse-info
       (hash 'type-parameters     type-parameters
             'fresh-parameters    fresh-parameters
             'superclass-expr     #'cls.superclass-expr
             'make-methods        #'cls.make-methods
             'initializer-self-id #'cls.initializer-self-id
             'initializer-args-id #'cls.initializer-args-id
             'initializer-body    #'cls.initializer-body
             'optional-inits      (syntax->datum #'cls.optional-inits)
             'only-init-internals (syntax->datum #'cls.init-internals)
             'only-init-names     (syntax->datum #'cls.init-externals)
             'init-internals
             (set-union (syntax->datum #'cls.init-internals)
                        (syntax->datum #'cls.init-field-internals))
             'public-internals   (syntax->datum #'cls.public-internals)
             'override-internals (syntax->datum #'cls.override-internals)
             'pubment-internals  (syntax->datum #'cls.pubment-internals)
             'augment-internals  (syntax->datum #'cls.augment-internals)
             'method-internals
             (set-union (syntax->datum #'cls.public-internals)
                        (syntax->datum #'cls.override-internals))
             'field-internals
             (set-union (syntax->datum #'cls.field-internals)
                        (syntax->datum #'cls.init-field-internals))
             'inherit-internals
             (syntax->datum #'cls.inherit-internals)
             'inherit-field-internals
             (syntax->datum #'cls.inherit-field-internals)
             'init-names
             (set-union (syntax->datum #'cls.init-externals)
                        (syntax->datum #'cls.init-field-externals))
             'field-names
             (set-union (syntax->datum #'cls.field-externals)
                        (syntax->datum #'cls.init-field-externals))
             'public-names   (syntax->datum #'cls.public-externals)
             'override-names (syntax->datum #'cls.override-externals)
             'pubment-names  (syntax->datum #'cls.pubment-externals)
             'augment-names  (syntax->datum #'cls.augment-externals)
             'inherit-names  (syntax->datum #'cls.inherit-externals)
             'inherit-field-names
             (syntax->datum #'cls.inherit-field-externals)
             'private-names  (syntax->datum #'cls.private-names)
             'private-fields (syntax->datum #'cls.private-field-names)
             'overridable-names
             (set-union (syntax->datum #'cls.public-externals)
                        (syntax->datum #'cls.override-externals))
             'augmentable-names
             (set-union (syntax->datum #'cls.pubment-externals)
                        (syntax->datum #'cls.augment-externals))
             'method-names
             (set-union (syntax->datum #'cls.public-externals)
                        (syntax->datum #'cls.override-externals)
                        (syntax->datum #'cls.augment-externals)
                        (syntax->datum #'cls.pubment-externals))
             'all-internal
             (set-union (syntax->datum #'cls.init-internals)
                        (syntax->datum #'cls.init-field-internals)
                        (syntax->datum #'cls.field-internals)
                        (syntax->datum #'cls.public-internals)
                        (syntax->datum #'cls.override-internals)
                        (syntax->datum #'cls.inherit-internals)
                        (syntax->datum #'cls.inherit-field-internals)
                        (syntax->datum #'cls.pubment-internals)
                        (syntax->datum #'cls.augment-internals))
             'all-external
             (set-union (syntax->datum #'cls.init-externals)
                        (syntax->datum #'cls.init-field-externals)
                        (syntax->datum #'cls.field-externals)
                        (syntax->datum #'cls.public-externals)
                        (syntax->datum #'cls.override-externals)
                        (syntax->datum #'cls.inherit-externals)
                        (syntax->datum #'cls.inherit-field-externals)
                        (syntax->datum #'cls.pubment-externals)
                        (syntax->datum #'cls.augment-externals))))
     (with-timing
      (do-timestamp (format "methods ~a" (dict-ref parse-info 'method-names)))
      (extend-tvars/new type-parameters fresh-parameters
        (do-check expected super-type parse-info)))]))

;; do-check : Type Type Dict -> Type
;; The actual type-checking
(define (do-check expected super-type parse-info)
  ;; unpack superclass names and types
  (define-values (super-row super-inits super-fields
                  super-methods super-augments)
    (match super-type
      [(tc-result1: (Class: super-row super-inits super-fields
                            super-methods super-augments))
       (values super-row super-inits super-fields
               super-methods super-augments)]
      [(tc-result1: t)
       (tc-error/expr "expected a superclass but got value of type ~a" t
                      #:stx (hash-ref parse-info 'superclass-expr))
       (values #f null null null null)]))
  (define super-init-names    (dict-keys super-inits))
  (define super-field-names   (dict-keys super-fields))
  (define super-method-names  (dict-keys super-methods))
  (define super-augment-names (dict-keys super-augments))
  ;; establish a mapping between internal and external names
  (define internal-external-mapping
    (for/hash ([internal (hash-ref parse-info 'all-internal)]
               [external (hash-ref parse-info 'all-external)])
      (values internal external)))
  ;; trawl the body for top-level expressions
  (define make-methods-stx (hash-ref parse-info 'make-methods))
  (define top-level-exprs
    (trawl-for-property make-methods-stx 'tr:class:top-level))
  ;; augment annotations go in their own table, because they're
  ;; the only kind of type annotation that is allowed to be duplicate
  ;; (i.e., m can have type Integer -> Integer and an augment type of
  ;;          String -> String in the separate tables)
  (define-values (annotation-table augment-annotation-table)
    ((compose (setup-pubment-defaults (hash-ref parse-info 'pubment-names))
              register-annotations)
     top-level-exprs))
  (do-timestamp "built annotation table")
  ;; find the `super-new` call (or error if missing)
  (define super-new-stxs
    (trawl-for-property make-methods-stx 'tr:class:super-new))
  (define super-new-stx (check-super-new-exists super-new-stxs))
  (define provided-super-inits
    (if super-new-stx
        (find-provided-inits super-new-stx super-inits)
        '()))
  (define provided-init-names (dict-keys provided-super-inits))
  (define remaining-super-inits
    (for/list ([(name val) (in-dict super-inits)]
               #:unless (member name provided-init-names))
      (cons name val)))
  ;; define which init names are optional
  (define optional-inits (hash-ref parse-info 'optional-inits))
  (define optional-external (for/set ([n optional-inits])
                              (dict-ref internal-external-mapping n)))
  (define optional-super
    (for/set ([(name val) (in-dict remaining-super-inits)]
              #:when (cadr val))
      name))
  ;; Type for self in method calls
  (define self-type
    (infer-self-type parse-info
                     super-row
                     expected
                     annotation-table
                     augment-annotation-table
                     optional-inits
                     internal-external-mapping
                     remaining-super-inits
                     super-fields
                     super-methods
                     super-augments))
  (match-define (Instance: (Class: _ inits fields methods augments))
                self-type)
  (do-timestamp "built self type")
  ;; trawl the body for the local name table
  (define locals
    (trawl-for-property make-methods-stx 'tr:class:local-table))
  (define-values (local-method-table local-private-table local-field-table
                  local-private-field-table local-init-table
                  local-inherit-table local-inherit-field-table
                  local-super-table
                  local-augment-table local-inner-table)
    (construct-local-mapping-tables (car locals)))
  ;; types for private elements
  (define private-method-types
    (for/hash ([(name type) (in-dict annotation-table)]
               #:when (set-member? (hash-ref parse-info 'private-names) name))
      (values name type)))
  (define private-field-types
    (for/hash ([(name type) (in-dict annotation-table)]
               #:when (set-member? (hash-ref parse-info 'private-fields) name))
      (values name (list type))))
  ;; start type-checking elements in the body
  (define-values (lexical-names lexical-types
                  lexical-names/top-level lexical-types/top-level)
    (local-tables->lexical-env parse-info
                               internal-external-mapping
                               local-method-table methods
                               local-field-table fields
                               local-private-field-table private-field-types
                               local-init-table inits
                               local-inherit-table
                               local-inherit-field-table
                               local-super-table
                               super-methods super-fields
                               local-augment-table local-inner-table
                               augments super-augments
                               local-private-table private-method-types
                               self-type))
  (do-timestamp "built local tables")
  (with-lexical-env/extend lexical-names/top-level lexical-types/top-level
    (check-super-new provided-super-inits super-inits))
  (do-timestamp "checked super-new")
  (with-lexical-env/extend lexical-names/top-level lexical-types/top-level
    (for ([stx top-level-exprs]
          ;; avoid checking these to avoid duplication and to avoid checking
          ;; ignored expressions
          #:unless (syntax-property stx 'tr:class:super-new)
          #:unless (syntax-property stx 'tr:class:type-annotation))
      (tc-expr stx)))
  (do-timestamp "checked other top-level exprs")
  (with-lexical-env/extend lexical-names/top-level lexical-types/top-level
    (check-field-set!s (hash-ref parse-info 'initializer-body)
                       local-field-table
                       inits))
  (do-timestamp "checked field initializers")
  ;; trawl the body and find methods and type-check them
  (define meth-stxs (trawl-for-property make-methods-stx 'tr:class:method))
  (define checked-method-types
    (with-lexical-env/extend lexical-names lexical-types
      (check-methods (append (hash-ref parse-info 'pubment-names)
                             (hash-ref parse-info 'overridable-names))
                     internal-external-mapping meth-stxs
                     methods self-type)))
  (do-timestamp "checked methods")
  (define checked-augment-types
    (with-lexical-env/extend lexical-names lexical-types
      (check-methods (hash-ref parse-info 'augment-names)
                     internal-external-mapping meth-stxs
                     augments self-type)))
  (do-timestamp "checked augments")
  (with-lexical-env/extend lexical-names lexical-types
    (check-private-methods meth-stxs (hash-ref parse-info 'private-names)
                           private-method-types self-type))
  (do-timestamp "checked privates")
  (do-timestamp "finished methods")
  (define final-class-type
    (merge-types self-type checked-method-types checked-augment-types))
  (check-method-presence-and-absence
   parse-info
   expected
   (set-union optional-external optional-super)
   remaining-super-inits
   super-field-names
   super-method-names
   super-augment-names)
  (when expected
    (check-below final-class-type expected))
  (define class-type-parameters (hash-ref parse-info 'type-parameters))
  (do-timestamp "done")
  (if (null? class-type-parameters)
      final-class-type
      (make-Poly #:original-names class-type-parameters
                 (hash-ref parse-info 'fresh-parameters)
                 final-class-type)))

;; check-method-presence-and-absence : Dict Type Set<Symbol> ... -> Void
;; use the internal class: information to check whether clauses
;; exist or are absent appropriately
(define (check-method-presence-and-absence
         parse-info expected
         optional-external
         remaining-super-inits
         super-field-names super-method-names super-augment-names)
  (when expected
   (match-define (Class: _ inits fields methods augments) expected)
   (define exp-init-names (dict-keys inits))
   (define exp-field-names (dict-keys fields))
   (define exp-method-names (dict-keys methods))
   (define exp-augment-names (dict-keys augments))
   (define exp-optional-inits
     (for/set ([(name val) (in-dict inits)]
               #:when (cadr val))
              name))
   (check-same (set-union (hash-ref parse-info 'init-names)
                          (dict-keys remaining-super-inits))
               exp-init-names
               "initialization argument")
   (check-same (set-union (hash-ref parse-info 'public-names)
                          (hash-ref parse-info 'pubment-names)
                          super-method-names)
               exp-method-names
               "public method")
   (check-same (set-union (hash-ref parse-info 'field-names)
                          super-field-names)
               exp-field-names
               "public field")
   (check-same (set-union (hash-ref parse-info 'augmentable-names)
                          super-augment-names)
               exp-augment-names
               "public augmentable method")
   (check-same optional-external exp-optional-inits
               "optional init argument"))
  (check-exists super-method-names (hash-ref parse-info 'override-names)
                "overridable method")
  (check-exists super-augment-names (hash-ref parse-info 'augment-names)
                "augmentable method")
  (check-exists (set-union super-method-names super-augment-names)
                (hash-ref parse-info 'inherit-names)
                "method")
  (check-exists super-field-names (hash-ref parse-info 'inherit-field-names)
                "field")
  (check-absent super-field-names (hash-ref parse-info 'field-names)
                "public field")
  (check-absent super-method-names (hash-ref parse-info 'public-names)
                "public method")
  (check-absent super-augment-names (hash-ref parse-info 'pubment-names)
                "public augmentable method"))

;; merge-types : Type Dict<Symbol, Type> Dict<Symbol, Type> -> Type
;; Given a self object type, construct the real class type based on
;; new information found from type-checking. Only used when an expected
;; type was not provided.
(define (merge-types self-type method-types augment-types)
  (match-define
   (Instance:
    (and class-type
         (Class: row-var inits fields methods augments)))
   self-type)
  (define (make-new-methods methods method-types)
    (for/fold ([methods methods])
              ([(name type) (in-dict method-types)])
      (define old-type (dict-ref methods name #f))
      ;; sanity check, to ensure that the actual method type
      ;; is as precise as the annotated type
      ;; FIXME: should this be a type error and not internal?
      (when (and old-type (not (subtype (car type) (car old-type))))
        (int-err (~a "merge-types: actual type ~a not"
                     " a subtype of annotated type ~a")
                 (car type) (car old-type)))
      (dict-set methods name type)))
  (make-Class row-var inits fields
              (make-new-methods methods method-types)
              (make-new-methods augments augment-types)))

;; local-tables->lexical-env : Dict Dict<Symbol, Symbol>
;;                             LocalMapping NameTypeDict
;;                             (for each kind of clause) ...
;;                             Type
;;                             -> List<Id> List<Type> List<Id> List<Type>
;; Construct mappings to put into the lexical type-checking environment
;; from the class local accessor mappings
(define (local-tables->lexical-env parse-info
                                   internal-external-mapping
                                   local-method-table methods
                                   local-field-table fields
                                   local-private-field-table
                                   private-field-types
                                   local-init-table inits
                                   local-inherit-table
                                   local-inherit-field-table
                                   local-super-table
                                   super-types super-fields
                                   local-augment-table local-inner-table
                                   augments super-augments
                                   local-private-table
                                   private-types
                                   self-type)
  ;; localize to accessor names via the provided tables
  (define (localize local-table name-key-or-list)
    (define names
      (if (list? name-key-or-list)
          (apply append (map (λ (k) (hash-ref parse-info k))
                             name-key-or-list))
          (hash-ref parse-info name-key-or-list)))
    (for/list ([m names]) (dict-ref local-table m)))
  (define-values (localized-method-names
                  localized-field-pairs
                  localized-private-field-pairs
                  localized-inherit-field-pairs
                  localized-inherit-names
                  localized-private-methods
                  localized-override-names
                  localized-pubment-names
                  localized-augment-names
                  localized-inner-names
                  localized-init-names)
    (values
     (localize local-method-table 'method-internals)
     (localize local-field-table 'field-internals)
     (localize local-private-field-table 'private-fields)
     (localize local-inherit-field-table 'inherit-field-internals)
     (localize local-inherit-table 'inherit-internals)
     (localize local-private-table 'private-names)
     (localize local-super-table 'override-internals)
     (localize local-augment-table 'pubment-internals)
     (localize local-augment-table 'augment-internals)
     (localize local-inner-table '(pubment-internals augment-internals))
     (localize local-init-table 'only-init-internals)))
  (define-values (localized-field-get-names
                  localized-field-set-names
                  localized-private-field-get-names
                  localized-private-field-set-names
                  localized-inherit-field-get-names
                  localized-inherit-field-set-names)
    (values (map car localized-field-pairs)
            (map cadr localized-field-pairs)
            (map car localized-private-field-pairs)
            (map cadr localized-private-field-pairs)
            (map car localized-inherit-field-pairs)
            (map cadr localized-inherit-field-pairs)))

  ;; construct the types for method accessors
  (define (make-method-types method-names type-map
                             #:inner? [inner? #f])
    (for/list ([m (in-set method-names)])
      (define external (dict-ref internal-external-mapping m))
      (define maybe-type (dict-ref type-map external #f))
      (->* (list (make-Univ))
           (cond [(and maybe-type
                       (not (equal? (car maybe-type) top-func))
                       (not inner?))
                  (function->method (car maybe-type) self-type)]
                 [(and maybe-type
                       (not (equal? (car maybe-type) top-func)))
                  (Un (-val #f)
                      (function->method (car maybe-type) self-type))]
                 [else (make-Univ)]))))

  (define method-types
    (make-method-types (hash-ref parse-info 'method-internals) methods))
  (define inherit-types
    (make-method-types
     (hash-ref parse-info 'inherit-internals)
     (append super-types super-augments)))
  (define augment-types
    (make-method-types (hash-ref parse-info 'augment-internals) augments))
  (define inner-types
    (make-method-types
     (set-union (hash-ref parse-info 'pubment-internals)
                (hash-ref parse-info 'augment-internals))
     augments #:inner? #t))

  ;; construct field accessor types
  (define (make-field-types field-names type-map #:private? [private? #f])
    (for/lists (_1 _2) ([f (in-set field-names)])
      (define external
        (if private?
            f
            (dict-ref internal-external-mapping f)))
      (define maybe-type (dict-ref type-map external #f))
      (values
       (->* (list (make-Univ)) (or (and maybe-type (car maybe-type))
                                   (make-Univ)))
       (->* (list (make-Univ) (or (and maybe-type (car maybe-type))
                                  -Bottom))
            -Void))))

  (define-values (field-get-types field-set-types)
    (make-field-types (hash-ref parse-info 'field-internals) fields))
  (define-values (private-field-get-types private-field-set-types)
    (make-field-types (hash-ref parse-info 'private-fields)
                      private-field-types
                      #:private? #t))
  (define-values (inherit-field-get-types inherit-field-set-types)
    (make-field-types (hash-ref parse-info 'inherit-field-internals)
                      super-fields))

  ;; types for privates and super calls
  (define (make-private-like-types names type-map)
    (for/list ([f (in-set names)])
      (define pre-type (dict-ref type-map f #f))
      (define maybe-type (if (pair? pre-type) (car pre-type) pre-type))
      (or (and maybe-type
               (not (equal? maybe-type top-func))
               (function->method maybe-type self-type))
          (make-Univ))))

  (define private-method-types
    (make-private-like-types (hash-ref parse-info 'private-names)
                             private-types))
  (define super-call-types
    (make-private-like-types (hash-ref parse-info 'override-internals)
                             super-types))
  (define pubment-types
    (make-private-like-types (hash-ref parse-info 'pubment-internals)
                             methods))

  ;; omit init-fields here since they don't have
  ;; init accessors, only field accessors
  (define init-types
    (for/list ([i (in-set (hash-ref parse-info 'only-init-internals))])
      (define external (dict-ref internal-external-mapping i))
      (car (dict-ref inits external (list -Bottom)))))

  (define all-names (append localized-method-names
                            localized-private-methods
                            localized-field-get-names
                            localized-field-set-names
                            localized-private-field-get-names
                            localized-private-field-set-names
                            localized-inherit-names
                            localized-inherit-field-get-names
                            localized-inherit-field-set-names
                            localized-override-names
                            localized-pubment-names
                            localized-augment-names
                            localized-inner-names))
  (define all-types (append method-types private-method-types
                            field-get-types field-set-types
                            private-field-get-types private-field-set-types
                            inherit-types
                            inherit-field-get-types
                            inherit-field-set-types
                            super-call-types
                            pubment-types augment-types inner-types))
  (values all-names all-types
          ;; FIXME: consider removing method names and types
          ;;        from top-level environment to avoid <undefined>
          (append all-names
                  localized-init-names
                  ;; Set `self` to the self-type and `init-args`
                  ;; to Any, so that accessors can use them without
                  ;; problems.
                  ;; Be careful though!
                  (list (hash-ref parse-info 'initializer-self-id)
                        (hash-ref parse-info 'initializer-args-id)))
          (append all-types
                  init-types
                  (list self-type (make-Univ)))))

;; check-methods : Listof<Symbol> Listof<Syntax> Dict<Symbol, Symbol> Dict Type
;;                 -> Dict<Symbol, Type>
;; Type-check the methods inside of a class
(define (check-methods names-to-check internal-external-mapping
                       meths methods self-type)
  (for/fold ([checked '()])
            ([meth meths])
    (define method-name (syntax-property meth 'tr:class:method))
    (define external-name (dict-ref internal-external-mapping method-name #f))
    (define maybe-expected (and external-name (dict-ref methods external-name #f)))
    (cond [(and maybe-expected
                ;; fall back to tc-expr/t if the annotated type
                ;; was the default type (Procedure)
                (not (equal? (car maybe-expected) top-func))
                (set-member? names-to-check external-name))
           (define pre-method-type (car maybe-expected))
           (define method-type
             (function->method pre-method-type self-type))
           (define expected (ret method-type))
           (define annotated (annotate-method meth self-type method-type))
           (do-timestamp (format "started checking method ~a" external-name))
           (tc-expr/check annotated expected)
           (do-timestamp (format "finished method ~a" external-name))
           (cons (list external-name pre-method-type) checked)]
          ;; Only try to type-check if these names are in the
          ;; filter when it's provided. This allows us to, say, only
          ;; type-check pubments/augments.
          [(set-member? names-to-check external-name)
           (do-timestamp (format "started checking method ~a" external-name))
           (define type (tc-expr/t meth))
           (do-timestamp (format "finished method ~a" external-name))
           (cons (list external-name
                       (method->function type))
                 checked)]
          [else checked])))

;; check-private-methods : Listof<Syntax> Listof<Sym> Dict<Sym, Type> Type
;;                         -> Void
;; Type-check private methods
(define (check-private-methods stxs names types self-type)
  (for ([stx stxs])
    (define method-name (syntax-property stx 'tr:class:method))
    (define private? (set-member? names method-name))
    (define annotation (dict-ref types method-name #f))
    (cond [(and private? annotation)
           (define pre-method-type annotation)
           (define method-type
             (function->method pre-method-type self-type))
           (define expected (ret method-type))
           (define annotated (annotate-method stx self-type method-type))
           (tc-expr/check annotated expected)]
          ;; not private, then ignore since it's irrelevant
          [(not private?) (void)]
          [else (tc-expr/t stx)])))

;; check-field-set!s : Syntax Dict<Symbol, Symbol> Dict<Symbol, Type> -> Void
;; Check that fields are initialized to the correct type
;; FIXME: this function is too long
(define (check-field-set!s stx local-field-table inits)
  (for ([form (syntax->list stx)])
    (syntax-parse form
      #:literals (let-values #%plain-app quote)
      ;; init with default
      ;; FIXME: undefined can appear here
      [(set! internal-init:id
             (#%plain-app extract-arg:id
                          _
                          (quote init-external:id)
                          init-args:id
                          init-val:expr))
       (define init-name (syntax-e #'init-external))
       (define init-type (car (dict-ref inits init-name '(#f))))
       (cond [init-type
              ;; This is a type for the internal `extract-args` function
              ;; that extracts init arguments from the object. We just
              ;; want to make sure that init argument default value
              ;; (the last argument) matches the type for the init.
              ;;
              ;; The rest is plumbing to make the type system happy.
              (define extract-arg-type
                (cl->* (->* (list (Un (-val #f) -Symbol) (-val init-name)
                                  (make-Univ) (-val #f)) init-type)
                       (->* (list (Un (-val #f) -Symbol) (-val init-name)
                                  (make-Univ) (->* '() init-type))
                            init-type)))
              ;; Catch the exception because the error that is produced
              ;; in the case of a type error is incomprehensible for a
              ;; programmer looking at surface syntax. Raise a custom
              ;; type error instead.
              (with-handlers
                  ([exn:fail:syntax?
                    (λ (e) (tc-error/expr "Default init value has wrong type"))])
                (parameterize ([delay-errors? #f])
                  (with-lexical-env/extend
                   (list #'extract-arg)
                   (list extract-arg-type)
                   (tc-expr form))))]
             ;; If the type can't be found, it means that there was no
             ;; expected type or no annotation was provided via (: ...).
             ;;
             ;; FIXME: is this the right place to raise this error, or
             ;;        should it be caught earlier so that this function
             ;;        can be simpler?
             [else
              (tc-error/expr "Init argument ~a has no type annotation"
                             init-name)])]
      ;; init-field with default
      [(let-values (((obj1:id) self:id))
         (let-values (((x:id)
                       (#%plain-app extract-arg:id
                                    _
                                    (quote name:id)
                                    init-args:id
                                    init-val:expr)))
           (#%plain-app local-setter:id obj2:id y:id)))
       #:when (free-identifier=? #'x #'y)
       #:when (free-identifier=? #'obj1 #'obj2)
       (define init-name (syntax-e #'name))
       (define init-type (car (dict-ref inits init-name '(#f))))
       (cond [init-type
              (define extract-arg-type
                (cl->* (->* (list (Un (-val #f) -Symbol) (-val init-name)
                                  (make-Univ) (-val #f)) init-type)
                       (->* (list (Un (-val #f) -Symbol) (-val init-name)
                                  (make-Univ) (->* '() init-type))
                            init-type)))
              (with-handlers
                  ([exn:fail:syntax?
                    ;; FIXME: produce a better error message
                    (λ (e) (tc-error/expr "Default init value has wrong type"))])
                (parameterize ([delay-errors? #f])
                  (with-lexical-env/extend
                   (list #'extract-arg)
                   (list extract-arg-type)
                   (tc-expr form))))]
             [else
              (tc-error/expr "Init argument ~a has no type annotation"
                             init-name)])]
      ;; any field or init-field without default
      ;; FIXME: could use the local table to make sure the
      ;;        setter is known as a sanity check
      [(let-values (((obj1:id) self:id))
         (let-values (((x:id) init-val:expr))
           (#%plain-app local-setter:id obj2:id y:id)))
       #:when (free-identifier=? #'x #'y)
       #:when (free-identifier=? #'obj1 #'obj2)
       (tc-expr form)]
      [_ (void)])))

;; Syntax -> Dict<Symbol, Id> Dict<Symbol, Id>
;;           Dict<Symbol, (List Symbol Symbol)> Dict<Symbol, Id>
;; Construct tables mapping internal method names to the accessors
;; generated inside the untyped class macro.
(define (construct-local-mapping-tables stx)
  (syntax-parse stx
    #:literals (let-values if quote #%plain-app #%plain-lambda values)
    ;; See base-env/class-prims.rkt to see how this in-syntax
    ;; table is constructed at the surface syntax
    ;;
    ;; FIXME: factor out with syntax classes
    [(let-values ([(method:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (#%plain-app (#%plain-app local-method:id _) _))
                    ...)]
                  [(private:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda () (#%plain-app local-private:id _))
                    ...)]
                  [(field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (let-values (((_) _)) (#%plain-app local-field-get:id _))
                      (let-values (((_) _))
                        (let-values (((_) _)) (#%plain-app local-field-set:id _ _))))
                    ...)]
                  [(private-field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (let-values (((_) _)) (#%plain-app local-private-get:id _))
                      (let-values (((_) _))
                        (let-values (((_) _)) (#%plain-app local-private-set:id _ _))))
                    ...)]
                  [(inherit-field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (let-values (((_) _)) (#%plain-app local-inherit-get:id _))
                      (let-values (((_) _))
                        (let-values (((_) _)) (#%plain-app local-inherit-set:id _ _))))
                    ...)]
                  [(init:id ...)
                   (#%plain-app values (#%plain-lambda () local-init:id) ...)]
                  [(inherit:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (#%plain-app (#%plain-app local-inherit:id _) _))
                    ...)]
                  [(override:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (#%plain-app (#%plain-app local-override:id _) _)
                      (#%plain-app local-super:id _))
                    ...)]
                  [(augment:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (~or (#%plain-app local-augment:id _)
                           (#%plain-app (#%plain-app local-augment:id _) _))
                      (let-values ([(_) (#%plain-app local-inner:id _)])
                        (if _ (#%plain-app _ _) _)))
                    ...)])
       (#%plain-app void))
     (values (map cons
                  (append (syntax->datum #'(method ...))
                          (syntax->datum #'(override ...)))
                  (append (syntax->list #'(local-method ...))
                          (syntax->list #'(local-override ...))))
             (map cons
                  (syntax->datum #'(private ...))
                  (syntax->list #'(local-private ...)))
             (map list
                  (syntax->datum #'(field ...))
                  (syntax->list #'(local-field-get ...))
                  (syntax->list #'(local-field-set ...)))
             (map list
                  (syntax->datum #'(private-field ...))
                  (syntax->list #'(local-private-get ...))
                  (syntax->list #'(local-private-set ...)))
             (map cons
                  (syntax->datum #'(init ...))
                  (syntax->list #'(local-init ...)))
             (map cons
                  (syntax->datum #'(inherit ...))
                  (syntax->list #'(local-inherit ...)))
             (map list
                  (syntax->datum #'(inherit-field ...))
                  (syntax->list #'(local-inherit-get ...))
                  (syntax->list #'(local-inherit-set ...)))
             (map cons
                  (syntax->datum #'(override ...))
                  (syntax->list #'(local-super ...)))
             (map cons
                  (syntax->datum #'(augment ...))
                  (syntax->list #'(local-augment ...)))
             (map cons
                  (syntax->datum #'(augment ...))
                  (syntax->list #'(local-inner ...))))]))

;; check-super-new-exists : Listof<Syntax> -> (U Syntax #f)
;; Check if a `super-new` call exists and if there is only
;; one call. Return #f on error.
(define (check-super-new-exists stxs)
  (cond [(null? stxs)
         (tc-error/expr
          "typed classes must call super-new at the class top-level")
         #f]
        [(> (length stxs) 1)
         (tc-error/expr
          "typed classes must only call super-new a single time")
         #f]
        [else (car stxs)]))

;; find-provided-inits : Syntax Inits -> Dict<Symbol, Syntax>
;; Find the init arguments that were provided via super-new
(define (find-provided-inits stx super-inits)
  (syntax-parse stx
    #:literals (#%plain-app list cons quote)
    [(#%plain-app super-go _ _ _ _ _
                  (#%plain-app
                   list
                   (#%plain-app cons (quote init-id) arg:expr)
                   ...))
     (define provided-inits (syntax->datum #'(init-id ...)))
     (for ([name provided-inits])
       (unless (dict-ref super-inits name #f)
         (tc-error/expr "super-new: init argument ~a not accepted by superclass"
                        name)))
     (map cons provided-inits (syntax->list #'(arg ...)))]))

;; check-super-new : Dict<Symbol, Syntax> Dict<Symbol, Type> -> Void
;; Check if the super-new call is well-typed
(define (check-super-new provided-inits super-inits)
  (for ([(init-id init-arg) (in-dict provided-inits)])
    (define maybe-expected (dict-ref super-inits init-id #f))
    (if maybe-expected
        (tc-expr/check init-arg (ret (car maybe-expected)))
        (tc-error/expr "init argument ~a not accepted by superclass"
                       init-id))))

;; Syntax -> Listof<Syntax>
;; Look through the expansion of the class macro in search for
;; syntax with some property (e.g., methods)
(define (trawl-for-property form prop)
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx prop))
                       (syntax->list stx-list))))
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                #%plain-lambda letrec-syntaxes+values)
    [stx
     #:when (syntax-property form prop)
     (list form)]
    [(let-values (b ...) body ...)
     (recur-on-all #'(b ... body ...))]
    ;; for letrecs, traverse the RHSs too
    [(letrec-values ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(letrec-syntaxes+values (sb ...) ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(#%plain-app e ...)
     (recur-on-all #'(e ...))]
    [(#%plain-lambda (x ...) e ...)
     (recur-on-all #'(e ...))]
    [_ '()]))

;; register-annotations : Listof<Syntax>
;;                        -> Dict<Symbol, Type>, Dict<Symbol, Type>
;; Find : annotations and register them, error if duplicates are found
;; TODO: support `define-type`?
(define (register-annotations stxs)
  ;; check if the key is duplicated and return the new table
  ;; (erroring if it is a duplicate)
  (define (check-duplicate table name type)
    (cond [(and (hash-has-key? table name)
                (not (equal? (hash-ref table name) type)))
           (tc-error/expr
            #:stx #'name
            "Duplicate type annotation of ~a for ~a, previous was ~a"
            type name (hash-ref table name))
           table]
          [else (hash-set table name type)]))
  (for/fold ([table #hash()] [augment-table #hash()])
            ([stx stxs])
    (syntax-parse stx
      #:literals (let-values begin quote-syntax :-internal :-augment
                  #%plain-app values void)
      [(let-values ((()
                     (begin
                       (quote-syntax (:-internal name-stx:id type-stx:expr))
                       (#%plain-app values))))
         (#%plain-app void))
       (define name (syntax-e #'name-stx))
       (define type (parse-type #'type-stx))
       (values (check-duplicate table name type) augment-table)]
      [(quote-syntax (:-augment name-stx:id type-stx))
       (define name (syntax-e #'name-stx))
       (define type (parse-type #'type-stx))
       (values table (check-duplicate augment-table name type))]
      [_ (values table augment-table)])))

;; setup-pubment-defaults : Listof<Symbol> ->
;;                          Dict<Symbol, Type> Dict<Symbol, Type> ->
;;                          Dict<Symbol, Type> Dict<Symbol, Type>
;; this does a second pass through the type annotations and adds
;; the pubment types as default augment types if an augment type
;; was not already provided
(define ((setup-pubment-defaults pubment-names)
         annotations augment-annotations)
  (for/fold ([annotations annotations]
             [augment-annotations augment-annotations])
            ([name pubment-names])
    (cond [(and (not (dict-has-key? augment-annotations name))
                (dict-has-key? annotations name))
           (values annotations
                   (dict-set augment-annotations name
                             (dict-ref annotations name)))]
          [else (values annotations augment-annotations)])))

;; infer-self-type : Dict RowVar Class Dict<Symbol, Type> Dict<Symbol, Type>
;;                   Set<Symbol> Dict<Symbol, Symbol>
;;                   Inits Fields Methods
;;                   -> Type
;; Construct a self object type based on all type annotations
;; and the expected type
(define (infer-self-type parse-info
                         super-row
                         expected
                         annotation-table augment-annotation-table
                         optional-inits
                         internal-external-mapping
                         super-inits super-fields super-methods
                         super-augments)
  (define (make-type-dict names supers maybe-expected
                          #:inits [inits? #f]
                          #:annotations-from [annotation-table annotation-table]
                          #:default-type [default-type Univ])
    (for/fold ([type-dict supers])
              ([name names])
      (define external (dict-ref internal-external-mapping name))
      (define (update-dict type)
        (define entry
          (if inits?
              (list type (set-member? optional-inits name))
              (list type)))
        (dict-set type-dict external entry))
      ;; A type is assigned for each member in this order:
      ;;   (1) a type annotation from the user
      ;;   (2) the expected type
      ;;   (3) Any or Procedure
      (cond [(dict-ref annotation-table name #f) => update-dict]
            [(and maybe-expected
                  (dict-ref maybe-expected name #f))
             => (compose update-dict car)]
            [default-type => update-dict])))
  (define-values (expected-inits expected-fields
                  expected-publics expected-augments)
    (match expected
      [(Class: _ inits fields publics augments)
       (values inits fields publics augments)]
      [_ (values #f #f #f #f)]))
  (define-values (inits fields publics pubments)
    (values (hash-ref parse-info 'init-internals)
            (hash-ref parse-info 'field-internals)
            (hash-ref parse-info 'public-internals)
            (hash-ref parse-info 'pubment-internals)))
  (define init-types (make-type-dict inits super-inits expected-inits
                                     #:inits #t))
  (define field-types (make-type-dict fields super-fields expected-fields))
  (define public-types (make-type-dict (append publics pubments)
                                       super-methods expected-publics
                                       #:default-type top-func))
  (define augment-types (make-type-dict
                         pubments super-augments expected-augments
                         #:default-type top-func
                         #:annotations-from augment-annotation-table))
  (make-Instance (make-Class super-row init-types field-types
                             public-types augment-types)))

;; function->method : Function Type -> Function
;; Fix up a method's arity from a regular function type
(define (function->method type self-type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs]
                  ;; ignore top-arr, since the arity cannot
                  ;; be sensibly modified in that case
                  #:when (arr? arr))
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cons self-type doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [(Poly: ns body)
     (make-Poly ns (function->method body self-type))]
    [(PolyDots: ns body)
     (make-PolyDots ns (function->method body self-type))]
    [(PolyRow: ns constraints body)
     (make-PolyRow ns constraints (function->method body self-type))]
    [_ (int-err "function->method: ~a" type)]))

;; method->function : Function -> Function
;; Turn a "real" method type back into a function type
(define (method->function type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs])
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cdr doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [(Poly: ns body)
     (make-Poly ns (method->function body))]
    [(PolyDots: ns body)
     (make-PolyDots ns (method->function type))]
    [(PolyRow: ns constraints body)
     (make-PolyRow ns constraints (method->function type))]
    [_ (tc-error/expr "expected a function type for method")]))

;; annotate-method : Syntax Type -> Syntax
;; Adds a self type annotation for the first argument and annotated
;; the let-values binding for tc-expr
(define (annotate-method stx self-type method-type)
  (syntax-parse stx
    #:literals (let-values #%plain-lambda)
    [(let-values ([(meth-name:id)
                   (#%plain-lambda (self-param:id id:id ...)
                     body ...)])
       m)
     (define annotated-self-param
       (type-ascription-property #'self-param self-type))
     #`(let-values ([(#,(syntax-property #'meth-name 'type-label method-type))
                     ;; attach source location to the lambda in order to
                     ;; obtain better error messages for arity errors
                     #,(quasisyntax/loc stx
                         (#%plain-lambda (#,annotated-self-param id ...)
                                         body ...))])
         m)]
    [(let-values ([(meth-name:id)
                   (let-values (((core:id)
                                 (#%plain-lambda (param:id ...)
                                   core-body ...)))
                     method-body ...)])
       m)
     #`(let-values ([(#,(syntax-property #'meth-name 'type-label method-type))
                     #,(syntax-property
                        #`(let-values (((core)
                                        ;; see comment above
                                        #,(quasisyntax/loc stx
                                           (#%plain-lambda (param ...)
                                                           core-body ...))))
                            method-body ...)
                        'kw-lambda #t)])
         m)]
    [_ (tc-error "annotate-method: internal error")]))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that all the required names are actually present
;;
;; FIXME: This gives bad error messages. Consider using syntax
;;        object lists instead of sets.
(define (check-exists actual required msg)
  (define missing
    (for/or ([m (in-set required)])
      (and (not (set-member? actual m)) m)))
  (when missing
    (tc-error/expr (~a "superclass missing ~a ~a "
                       "that the current class requires")
                   msg missing)))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that names are absent when they should be
(define (check-absent actual should-be-absent msg)
  (define present
    (for/or ([m (in-set should-be-absent)])
      (and (set-member? actual m) m)))
  (when present
    (tc-error/expr "superclass defines conflicting ~a ~a"
                   msg present)))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that the names are exactly the same as expected
(define (check-same actual expected msg)
  (define missing
    (for/or ([m (in-set expected)])
      (and (not (set-member? actual m)) m)))
  (when missing
    (tc-error/expr (~a "class definition missing ~a ~a "
                       "that is required by the expected type")
                   msg missing))
  (define too-many
    (for/or ([m (in-set actual)])
      (and (not (set-member? expected m)) m)))
  (when too-many
    (tc-error/expr (~a "class definition contains ~a ~a "
                       "that is not in the expected type")
                   msg too-many)))

;; I wish I could write this
#;
(module+ test
  (check-equal? (function->method (parse-type #'(Integer -> Integer)))
                (parse-type #'(Any Integer -> Integer))))

