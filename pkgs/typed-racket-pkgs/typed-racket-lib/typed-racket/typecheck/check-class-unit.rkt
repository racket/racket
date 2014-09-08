#lang racket/unit

;; This module provides a unit for type-checking classes

(require "../utils/utils.rkt"
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/set
         racket/syntax
         syntax/id-table
         syntax/parse
         syntax/stx
         "signatures.rkt"
         (private parse-type syntax-properties type-annotation)
         (env lexical-env tvar-env global-env)
         (types utils abbrev union subtype resolve generalize)
         (typecheck check-below internal-forms)
         (utils tc-utils)
         (rep type-rep)
         (for-syntax racket/base)
         (for-template racket/base
                       (private class-literals)
                       (typecheck internal-forms)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-class^)

;; A super-init-stxs is a
;;   (super-init-stxs (Listof Syntax) (Dict Symbol Syntax))
;;
;; interp. Represents the by-position and by-name initialization
;;         arguments respectively provided by the class
(struct super-init-stxs (by-position by-name) #:transparent)

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

(define-syntax (do-timestamp stx)
  (syntax-case stx ()
    [(_ str)
     (if (syntax-local-value #'do-timing)
         #'(log-info
            (format "TR class time @ ~a: ~a"
                    str (- (current-inexact-milliseconds) (start-time))))
         #'(void))]))

;; Syntax classes for use in functions below
(define-syntax-class name-pair
  (pattern (internal:id external:id)))

(define-syntax-class internal-class-data
  #:literal-sets (kernel-literals)
  #:literals (class-internal values)
  (pattern (begin (quote-syntax
                   (class-internal
                    (#:forall type-parameter:id ...)
                    (#:all-inits all-init-names:id ...)
                    (#:init init-names:name-pair ...)
                    (#:init-field init-field-names:name-pair ...)
                    (#:init-rest (~optional init-rest-name:id))
                    (#:optional-init optional-names:id ...)
                    (#:field field-names:name-pair ...)
                    (#:public public-names:name-pair ...)
                    (#:override override-names:name-pair ...)
                    (#:private privates:id ...)
                    (#:private-field private-fields:id ...)
                    (#:inherit inherit-names:name-pair ...)
                    (#:inherit-field inherit-field-names:name-pair ...)
                    (#:augment augment-names:name-pair ...)
                    (#:pubment pubment-names:name-pair ...)))
                  (#%plain-app values))
           #:with type-parameters #'(type-parameter ...)
           #:with all-init-internals #'(all-init-names ...)
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
  #:literals (let-values letrec-syntaxes+values #%plain-app quote)
  #:attributes (superclass-expr
                type-parameters
                all-init-internals
                init-internals init-externals
                init-field-internals init-field-externals
                init-rest-name
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
              (#%plain-app
               compose-class:id
               name:expr
               superclass-expr:expr
               interface-expr:expr
               internal:expr ...
               (~and make-methods :make-methods-class)
               (quote :boolean)
               (quote #f))))))

;; This is similar to `type-declaration` from "internal-forms.rkt", but
;; the expansion is slightly different in a class so we use this instead.
(define-syntax-class class-type-declaration
  #:literal-sets (kernel-literals)
  #:literals (values void :-internal)
  #:attributes (name type)
  (pattern (let-values
             ([() (begin (quote-syntax (:-internal name:id type:expr))
                         (#%plain-app values))])
             (#%plain-app void))))

;; Syntax Option<TCResults> -> TCResults
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
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (match expected-type
    [(? Class? class-type)
     (ret (parse-and-check form class-type))]
    [(Poly-names: ns body-type)
     (match (check-class form (ret body-type))
       [(tc-result1: t f o)
        (ret (make-Poly ns t) f o)])]
    [_ (ret (parse-and-check form #f))]))

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
             ;; the order of these names reflect the order in the class,
             ;; so use this list when retaining the order is important
             'init-internals      (syntax->datum #'cls.all-init-internals)
             'init-rest-name     (and (attribute cls.init-rest-name)
                                      (syntax-e (attribute cls.init-rest-name)))
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
             (append (syntax->datum #'cls.init-internals)
                     (syntax->datum #'cls.init-field-internals)
                     (syntax->datum #'cls.field-internals)
                     (syntax->datum #'cls.public-internals)
                     (syntax->datum #'cls.override-internals)
                     (syntax->datum #'cls.inherit-internals)
                     (syntax->datum #'cls.inherit-field-internals)
                     (syntax->datum #'cls.pubment-internals)
                     (syntax->datum #'cls.augment-internals))
             'all-external
             (append (syntax->datum #'cls.init-externals)
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
                  super-methods super-augments super-init-rest)
    (match super-type
      [(tc-result1: t)
       (match (resolve t)
         [(Class: super-row super-inits super-fields
                  super-methods super-augments super-init-rest)
          (values super-row super-inits super-fields
                  super-methods super-augments super-init-rest)]
         [t
          (tc-error/fields "type mismatch"
                           #:more "superclass expression should produce a class"
                           #:stx (hash-ref parse-info 'superclass-expr)
                           #:delayed? #t
                           "expected" "a class"
                           "given" t)
          (values #f null null null null #f)])]
      [_ (int-err "Unhandled result")]))
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
    (trawl-for-property make-methods-stx tr:class:top-level-property))

  ;; Filter top level expressions into several groups, each processed
  ;; into appropriate data structures
  ;;
  ;; Augment annotations go in their own table, because they're
  ;; the only kind of type annotation that is allowed to be duplicate
  ;; (i.e., m can have type Integer -> Integer and an augment type of
  ;;          String -> String in the separate tables)
  (define-values (super-new initializers
                  annotation-table augment-annotation-table
                  other-top-level-exprs)
    (handle-top-levels top-level-exprs))

  (setup-pubment-defaults (hash-ref parse-info 'pubment-names)
                          annotation-table
                          augment-annotation-table)

  ;; Calculate remaining inits, optional inits, etc.
  (check-by-name super-new super-inits)

  ;; super-init-rest* - The init-rest passed to the `infer-self-type` function.
  ;;                    This reflects any changes to the `super-init-rest` type
  ;;                    that are necessary due to the super constructor call in
  ;;                    this class.
  (define-values (super-init-rest* remaining-super-inits)
    (handle-pos-inits super-new super-inits super-init-rest))

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
                     super-augments
                     super-init-rest*))
  (match-define (Instance: (Class: _ inits fields methods augments init-rest))
                self-type)
  (do-timestamp "built self type")
  ;; trawl the body for the local name table
  (define locals
    (trawl-for-property make-methods-stx tr:class:local-table-property))
  (define-values (local-method-table local-private-table local-field-table
                  local-private-field-table local-init-table
                  local-init-rest-table
                  local-inherit-table local-inherit-field-table
                  local-super-table
                  local-augment-table local-inner-table)
    (construct-local-mapping-tables (car locals)))

  ;; types for private elements
  (define private-method-types
    (for/hash ([(name type) (in-dict annotation-table)]
               #:when (set-member? (hash-ref parse-info 'private-names) name))
      (values name type)))
  (define private-field-types (make-hash))
  (for ([(name type) (in-dict annotation-table)]
        #:when (set-member? (hash-ref parse-info 'private-fields) name))
    (hash-set! private-field-types name (list type)))

  (synthesize-private-field-types initializers
                                  local-private-field-table
                                  private-field-types)

  ;; start type-checking elements in the body
  (define-values (lexical-names lexical-types
                  lexical-names/top-level lexical-types/top-level)
    (local-tables->lexical-env parse-info
                               internal-external-mapping
                               local-method-table methods
                               local-field-table fields
                               local-private-field-table private-field-types
                               local-init-table inits
                               local-init-rest-table init-rest
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
    (check-super-new super-new super-inits super-init-rest))
  (do-timestamp "checked super-new")
  (with-lexical-env/extend lexical-names/top-level lexical-types/top-level
    (for ([stx other-top-level-exprs])
      (tc-expr stx)))
  (do-timestamp "checked other top-level exprs")
  (with-lexical-env/extend lexical-names/top-level lexical-types/top-level
    (check-field-set!s (hash-ref parse-info 'initializer-body)
                       local-field-table
                       inits))
  (do-timestamp "checked field initializers")
  ;; trawl the body and find methods and type-check them
  (define meth-stxs
    (trawl-for-property make-methods-stx tr:class:method-property))
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

;; handle-top-levels : (Listof Syntax) ->
;;                     super-init-stxs Dict Dict Hash (Listof Syntax)
;; Divide top level expressions into several categories, and put them
;; in appropriate data structures.
(define (handle-top-levels exprs)
  (define super-new #f)
  (define initializers (make-free-id-table))
  (define annotations (make-hash))
  (define augment-annotations (make-hash))
  (define other-exprs
    (for/fold ([other-exprs '()])
              ([expr exprs])
      (syntax-parse expr
        #:literal-sets (kernel-literals)
        #:literals (:-augment)
        [(begin
           (quote ((~datum declare-field-initialization) _))
           (let-values ([(obj:id) self])
             (let-values ([(field:id) initial-value])
               (with-continuation-mark _ _
                 (#%plain-app setter:id obj2:id field2:id)))))
         ;; There should only be one initialization expression per field
         ;; since they are distinguished by a declaration.
         (cond [(not (dict-has-key? initializers #'setter))
                (free-id-table-set! initializers #'setter #'initial-value)]
               [else
                (int-err "more than one field initialization expression")])
         other-exprs]
        ;; The second part of this pattern ensures that we find the actual
        ;; initialization call, rather than the '(declare-super-new) in
        ;; the expansion.
        [(~and :tr:class:super-new^ (#%plain-app . rst))
         (when super-new
           (tc-error/fields #:delayed? #t
                            "ill-formed typed class"
                            #:more "must only call `super-new' a single time"))
         (set! super-new (find-provided-inits expr))
         other-exprs]
        [(~and t:class-type-declaration :tr:class:type-annotation^)
         (define name (syntax-e #'t.name))
         (define type (parse-type #'t.type))
         (unless (check-duplicate-member annotations name type)
           (hash-set! annotations name type))
         other-exprs]
        ;; FIXME: use internal-forms for this instead
        [(quote-syntax (:-augment name-stx:id type-stx))
         (define name (syntax-e #'name-stx))
         (define type (parse-type #'type-stx))
         (unless (check-duplicate-member augment-annotations name type)
           (hash-set! augment-annotations name type))
         other-exprs]
        [_ (cons expr other-exprs)])))
  (unless super-new
    (tc-error/fields #:delayed? #t
                     "ill-formed typed class"
                     #:more "must call `super-new' at the top-level of the class")
    (set! super-new (super-init-stxs null null)))
  (values super-new
          initializers
          annotations
          augment-annotations
          other-exprs))

;; check-duplicate-member : Hash Symbol Type -> Boolean
;; return true if the class member is already annotated
(define (check-duplicate-member table name type)
  (and (hash-has-key? table name)
       (not (equal? (hash-ref table name) type))
       (tc-error/expr/fields
        "duplicate type annotation in class"
        #:stx #`#,name
        #:return #t
        "for identifier" name
        "new type" type
        "previous type" (hash-ref table name))))

;; check-method-presence-and-absence : Dict Type Set<Symbol> ... -> Void
;; use the internal class: information to check whether clauses
;; exist or are absent appropriately
(define (check-method-presence-and-absence
         parse-info expected
         optional-external
         remaining-super-inits
         super-field-names super-method-names super-augment-names)
  (when expected
   (match-define (Class: _ inits fields methods augments _) expected)
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
         (Class: row-var inits fields methods augments init-rest)))
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
              (make-new-methods augments augment-types)
              init-rest))

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
                                   local-init-rest-table init-rest
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
       (-> (make-Univ) (or (and maybe-type (car maybe-type))
                           (make-Univ)))
       (-> (make-Univ) (or (and maybe-type (car maybe-type))
                           -Bottom)
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

  (define localized-init-rest-name
    (let ([name (hash-ref parse-info 'init-rest-name)])
      (if name
          (list (dict-ref local-init-rest-table name))
          null)))

  (define init-rest-type
    (if (hash-ref parse-info 'init-rest-name)
        (list (or init-rest Univ))
        null))

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
          (append all-names
                  localized-init-names
                  localized-init-rest-name
                  ;; Set `self` to the self-type and `init-args`
                  ;; to Any, so that accessors can use them without
                  ;; problems.
                  ;; Be careful though!
                  (list (hash-ref parse-info 'initializer-self-id)
                        (hash-ref parse-info 'initializer-args-id)))
          (append all-types
                  init-types
                  init-rest-type
                  (list self-type (make-Univ)))))

;; check-methods : Listof<Symbol> Listof<Syntax> Dict<Symbol, Symbol> Dict Type
;;                 -> Dict<Symbol, Type>
;; Type-check the methods inside of a class
(define (check-methods names-to-check internal-external-mapping
                       meths methods self-type)
  (for/fold ([checked '()])
            ([meth meths])
    (define method-name (tr:class:method-property meth))
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
           (register-method-ids meth self-type method-type)
           (do-timestamp (format "started checking method ~a" external-name))
           (tc-expr/check (add-kw-property meth) expected)
           (do-timestamp (format "finished method ~a" external-name))
           (cons (list external-name pre-method-type) checked)]
          ;; Only try to type-check if these names are in the
          ;; filter when it's provided. This allows us to, say, only
          ;; type-check pubments/augments.
          [(set-member? names-to-check external-name)
           (do-timestamp (format "started checking method ~a" external-name))
           ;; FIXME: this case doesn't work very well yet for keyword methods
           ;;        because TR can't recognize that the expansion is a kw
           ;;        function (unlike the expected case).
           (register-method-ids meth self-type #f)
           (define type (tc-expr/t (add-kw-property meth)))
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
    (define method-name (tr:class:method-property stx))
    (define private? (set-member? names method-name))
    (define annotation (dict-ref types method-name #f))
    (cond [(and private? annotation)
           (define pre-method-type annotation)
           (define method-type
             (function->method pre-method-type self-type))
           (define expected (ret method-type))
           (register-method-ids stx self-type method-type)
           (tc-expr/check (add-kw-property stx) expected)]
          ;; not private, then ignore since it's irrelevant
          [(not private?) (void)]
          [else
           (register-method-ids stx self-type #f)
           (tc-expr/t (add-kw-property stx))])))

;; check-field-set!s : Syntax Dict<Symbol, Symbol> Dict<Symbol, Type> -> Void
;; Check that fields are initialized to the correct type
;; FIXME: use syntax classes for matching and clearly separate the handling
;;        of field initialization and set! uses
(define (check-field-set!s stx local-field-table inits)
  (for ([form (syntax->list stx)])
    (syntax-parse form
      #:literal-sets (kernel-literals)
      ;; init with default
      [(set! internal-init:id
             (begin
               (#%plain-app extract-arg:id
                            _
                            (quote init-external:id)
                            init-args:id
                            init-val:expr)))
       (define init-name (syntax-e #'init-external))
       (check-init-arg init-name
                       (car (dict-ref inits init-name '(#f)))
                       #'init-val)]
      ;; init-field with default
      [(begin
         (quote ((~or (~datum declare-field-assignment)
                      (~datum declare-field-initialization))
                 _))
         (let-values (((obj1:id) self:id))
           (let-values (((x:id)
                         (#%plain-app extract-arg:id
                                      _
                                      (quote name:id)
                                      init-args:id
                                      init-val:expr)))
             (~or (with-continuation-mark _ _
                    (#%plain-app local-setter:id obj2:id y:id))
                  (#%plain-app local-setter:id obj2:id y:id)))))
       #:when (free-identifier=? #'x #'y)
       #:when (free-identifier=? #'obj1 #'obj2)
       (define init-name (syntax-e #'name))
       (check-init-arg init-name
                       (car (dict-ref inits init-name '(#f)))
                       #'init-val)]
      ;; any field or an init-field without default
      [(begin
         (quote ((~or (~datum declare-field-assignment)
                      (~datum declare-field-initialization))
                 _))
         (let-values (((obj1:id) self:id))
           (let-values (((x:id) init-val:expr))
             (~or (with-continuation-mark _ _
                    (#%plain-app local-setter:id obj2:id y:id))
                  (#%plain-app local-setter:id obj2:id y:id)))))
       #:when (free-identifier=? #'x #'y)
       #:when (free-identifier=? #'obj1 #'obj2)
       ;; Remove wcm for checking since TR can't handle these cases
       (define simplified
         (syntax/loc form
           (let-values (((obj1) self))
             (let-values (((x) init-val))
               (#%plain-app local-setter obj2 y)))))
       (tc-expr simplified)]
      [_ (void)])))

;; check-init-arg : Id Type Syntax -> Void
;; Check the initialization of an init arg variable against the
;; expected type provided by an annotation (or the default)
(define (check-init-arg init-name init-type init-val)
  (define thunk?
    (and (stx-pair? init-val)
         (free-identifier=? #'#%plain-lambda (stx-car init-val))))
  (unless (equal? (syntax->datum init-val) '(quote #f))
    (cond [thunk?
           (define type
             (tc-expr/check/t init-val (ret (->* null init-type))))
           (match type
             [(Function: (list (arr: _ (Values: (list (Result: result _ _)))
                                     _ _ _)))
              (check-below result init-type)]
             [_ (int-err "unexpected init value ~a"
                         (syntax->datum init-val))])]
          [else
           (tc-expr/check init-val (ret init-type))])))

;; synthesize-private-field-types : IdTable Dict Hash -> Void
;; Given top-level expressions in the class, synthesize types from
;; the initialization expressions for private fields.
(define (synthesize-private-field-types initializers locals types)
  (for ([(name getter+setter) (in-dict locals)]
        #:unless (hash-has-key? types name))
    (match-define (list _ setter) getter+setter)
    (define init-expr-stx (free-id-table-ref initializers setter #f))
    (when init-expr-stx
      (define type (tc-expr/t init-expr-stx))
      ;; FIXME: this always generalizes the private field
      ;;        type, but it's better to only generalize if
      ;;        the field is actually mutated.
      (hash-set! types name (list (generalize type))))))

;; Syntax -> Dict<Symbol, Id> Dict<Symbol, Id>
;;           Dict<Symbol, (List Symbol Symbol)> Dict<Symbol, Id>
;; Construct tables mapping internal method names to the accessors
;; generated inside the untyped class macro.
(define (construct-local-mapping-tables stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    #:literals (values)
    ;; See base-env/class-prims.rkt to see how this in-syntax
    ;; table is constructed at the surface syntax
    ;;
    ;; FIXME: factor out with syntax classes
    [(let-values ([(method:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-this-escapes)))
                      (#%plain-app (#%plain-app local-method:id _) _))
                    ...)]
                  [(private:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-this-escapes)))
                      (#%plain-app local-private:id _))
                    ...)]
                  [(field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-field-use) _))
                      (let-values (((_) _)) (#%plain-app local-field-get:id _))
                      (begin
                        (quote ((~datum declare-field-assignment) _))
                        (let-values (((_) _))
                          (let-values (((_) _)) (#%plain-app local-field-set:id _ _)))))
                    ...)]
                  [(private-field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-field-use) _))
                      (let-values (((_) _)) (#%plain-app local-private-get:id _))
                      (begin
                        (quote ((~datum declare-field-assignment) _))
                        (let-values (((_) _))
                          (let-values (((_) _)) (#%plain-app local-private-set:id _ _)))))
                    ...)]
                  [(inherit-field:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-inherit-use) _))
                      (let-values (((_) _)) (#%plain-app local-inherit-get:id _))
                      (let-values (((_) _))
                        (let-values (((_) _)) (#%plain-app local-inherit-set:id _ _))))
                    ...)]
                  [(init:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      ;; check-not-unsafe-undefined
                      (#%plain-app _ local-init:id _)) ...)]
                  [(init-rest:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      ;; check-not-unsafe-undefined
                      (#%plain-app _ local-init-rest:id _)) ...)]
                  [(inherit:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-this-escapes)))
                      (#%plain-app (#%plain-app local-inherit:id _) _))
                    ...)]
                  [(override:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-this-escapes)))
                      (#%plain-app (#%plain-app local-override:id _) _)
                      (quote ((~datum declare-this-escapes)))
                      (#%plain-app local-super:id _))
                    ...)]
                  [(augment:id ...)
                   (#%plain-app
                    values
                    (#%plain-lambda ()
                      (quote ((~datum declare-this-escapes)))
                      (~or (#%plain-app local-augment:id _)
                           (#%plain-app (#%plain-app local-augment:id _) _))
                      (quote ((~datum declare-this-escapes)))
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
             ;; this should only be a singleton list or null
             (map cons
                  (syntax->datum #'(init-rest ...))
                  (syntax->list #'(local-init-rest ...)))
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

;; find-provided-inits : Syntax -> super-init-stxs
;; Find the init arguments that were provided via super-new
(define (find-provided-inits stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    #:literals (cons list)
    [(#%plain-app
      (#%plain-lambda args
        (#%plain-app super-go _ _ _ _ _ _))
      pos-arg:expr ...)
     (super-init-stxs (syntax->list #'(pos-arg ...)) null)]
    [(#%plain-app super-go _ _ _ _
                  (~or (#%plain-app list pos-arg:expr ...)
                       (~and _ (~bind [(pos-arg 1) '()])))
                  (#%plain-app
                   list
                   (#%plain-app cons (quote init-id) arg:expr)
                   ...))
     (define provided-inits (syntax->datum #'(init-id ...)))
     (super-init-stxs
      (syntax->list #'(pos-arg ...))
      (map cons provided-inits (syntax->list #'(arg ...))))]))

;; handle-pos-inits : super-init-stxs Dict (Option Type) -> Type Dict
;; Check if the init-rest type works and return a potentially changed
;; init-rest type and the remaining init args after
(define (handle-pos-inits super-new super-inits super-init-rest)
  (match-define (super-init-stxs provided-pos-args provided-super-inits)
                super-new)
  (define provided-init-names (dict-keys provided-super-inits))
  (define pos-length (length provided-pos-args))
  (cond [;; too many init arguments, and no init-rest
           (and (not super-init-rest) (> pos-length (length super-inits)))
           (values super-init-rest
                   (tc-error/expr/fields
                    "invalid `super-make-object' or `super-instantiate'"
                    #:more "too many positional init arguments provided"
                    "expected" (length super-inits)
                    "given" pos-length
                    #:stx #`(#,@provided-pos-args)
                    #:return null))]
          [;; no remaining by-name inits, so change the init-rest type
           ;; and return a null remaining named inits list
           (> pos-length (length super-inits))
           (values (Un) null)]
          [else
           (values super-init-rest
                   (for/list ([(name val) (in-dict (drop super-inits pos-length))]
                              #:unless (member name provided-init-names))
                     (cons name val)))]))

;; check-by-name : super-init-stxs Dict -> Void
;; Check that by-name inits are valid for the superclass
(define (check-by-name init-stxs super-inits)
  (match-define (super-init-stxs _ by-name) init-stxs)
  (for ([(name _) (in-dict by-name)])
    (unless (dict-ref super-inits name #f)
      (tc-error/fields
       "invalid `super-new' or `super-instantiate'"
       #:more "init argument not accepted by superclass"
       "init name" name
       #:stx #`#,name
       #:delayed? #t))))

;; check-super-new : super-init-stxs Dict Type -> Void
;; Check if the super-new call is well-typed
(define (check-super-new super-new super-inits init-rest)
  (match-define (super-init-stxs provided-pos-args provided-inits)
                super-new)
  (define pos-init-diff (- (length provided-pos-args) (length super-inits)))
  (cond [(and (> pos-init-diff 0) (not init-rest))
         ;; errror case that's caught above, do nothing
         (void)]
        [(> pos-init-diff 0)
         (define-values (pos-args for-init-rest)
           (split-at provided-pos-args (length super-inits)))
         (for ([pos-arg pos-args]
               [init super-inits])
           (match-define (list _ type _) init)
           (tc-expr/check pos-arg (ret type)))
         (tc-expr/check #`(#%plain-app list #,@for-init-rest)
                        (ret init-rest))]
        [else
         (define-values (pos-inits remaining-inits)
           (split-at super-inits (length provided-pos-args)))
         (for ([pos-arg provided-pos-args]
               [init pos-inits])
           (match-define (list _ type _) init)
           (tc-expr/check pos-arg (ret type)))
         (for ([(init-id init-arg) (in-dict provided-inits)])
           (define maybe-expected (dict-ref remaining-inits init-id #f))
           (when maybe-expected
             (tc-expr/check init-arg (ret (car maybe-expected)))))]))

;; Syntax (Syntax -> Any) -> Listof<Syntax>
;; Look through the expansion of the class macro in search for
;; syntax with some property (e.g., methods)
(define (trawl-for-property form accessor)
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx accessor))
                       (syntax->list stx-list))))
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                #%plain-lambda letrec-syntaxes+values)
    [stx
     #:when (accessor #'stx)
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

;; setup-pubment-defaults : Listof<Symbol> Hash Hash -> Void
;; this does a second pass through the type annotations and adds
;; the pubment types as default augment types if an augment type
;; was not already provided
(define (setup-pubment-defaults pubment-names annotations augment-annotations)
  (for ([name pubment-names])
    (when (and (not (hash-has-key? augment-annotations name))
               (hash-has-key? annotations name))
      (hash-set! augment-annotations
                 name
                 (dict-ref annotations name)))))

;; infer-self-type : Dict RowVar Class Dict<Symbol, Type> Dict<Symbol, Type>
;;                   Set<Symbol> Dict<Symbol, Symbol>
;;                   Inits Fields Methods Type
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
                         super-augments super-init-rest)
  ;; Gets a type for a given name in the class.
  ;; A type is assigned for each member in this order:
  ;;   (1) a type annotation from the user
  ;;   (2) the expected type
  ;;   (3) Any or Procedure
  (define (assign-type name expected annotation-table update default-type)
    (cond [(dict-ref annotation-table name #f) => update]
          [(and expected (dict-ref expected name #f))
           => (compose update car)]
          [default-type => update]))

  ;; construct the new init type dict
  (define (make-inits names supers expected)
    (define-values (inits new-inits)
      (for/fold ([type-dict supers] [new-entries '()])
                ([name names])
        (define external (dict-ref internal-external-mapping name))
        (define (update-dict type)
          (define entry (list type (set-member? optional-inits name)))
          ;; new entries have to go on the front, so sort them separately
          (if (dict-has-key? type-dict external)
              (values (dict-set type-dict external entry) new-entries)
              (values type-dict (cons (cons external entry) new-entries))))
        (assign-type name expected annotation-table update-dict Univ)))
    (append (reverse new-inits) inits))

  ;; construct type dicts for fields, methods, and augments
  (define (make-type-dict names supers expected default-type
                          #:annotations-from [annotation-table annotation-table])
    (for/fold ([type-dict supers])
              ([name names])
      (define external (dict-ref internal-external-mapping name))
      (define (update-dict type)
        (define entry (list type))
        (dict-set type-dict external entry))
      (assign-type name expected annotation-table update-dict default-type)))

  (define-values (expected-inits expected-fields
                  expected-publics expected-augments
                  expected-init-rest)
    (match expected
      [(Class: _ inits fields publics augments init-rest)
       (values inits fields publics augments init-rest)]
      [_ (values #f #f #f #f #f)]))
  (define-values (inits fields publics pubments init-rest-name)
    (values (hash-ref parse-info 'init-internals)
            (hash-ref parse-info 'field-internals)
            (hash-ref parse-info 'public-internals)
            (hash-ref parse-info 'pubment-internals)
            (hash-ref parse-info 'init-rest-name)))
  (define init-types (make-inits inits super-inits expected-inits))
  (define field-types (make-type-dict fields super-fields expected-fields Univ))
  (define public-types (make-type-dict (append publics pubments)
                                       super-methods expected-publics
                                       top-func))
  (define augment-types (make-type-dict
                         pubments super-augments expected-augments top-func
                         #:annotations-from augment-annotation-table))
  ;; For the init-rest type, if the user didn't provide one, then
  ;; take the superclass init-rest. Otherwise, find the annotated type
  ;; or use (Listof Any) as the type if no annotation exists.
  (define init-rest-type
    (cond [(not init-rest-name) super-init-rest]
          [(dict-ref annotation-table init-rest-name #f)]
          [else (-lst Univ)]))
  (make-Instance (make-Class super-row init-types field-types
                             public-types augment-types init-rest-type)))

;; function->method : Function Type -> Function
;; Fix up a method's arity from a regular function type
(define (function->method type self-type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs])
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cons self-type doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [(Poly-names: ns body)
     (make-Poly ns (function->method body self-type))]
    [(PolyDots-names: ns body)
     (make-PolyDots ns (function->method body self-type))]
    [(PolyRow-names: ns constraints body)
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
    [(Poly-names: ns body)
     (make-Poly ns (method->function body))]
    [(PolyDots-names: ns body)
     (make-PolyDots ns (method->function type))]
    [(PolyRow-names: ns constraints body)
     (make-PolyRow ns constraints (method->function type))]
    [_ (tc-error/expr #:return -Bottom "expected a function type for method")]))

;; register-method-ids : Syntax (Option Type) -> Void
;; Register types for identifiers in a method that don't come with types
(define (register-method-ids stx self-type method-type)
  (define (do-register self-param meth-name)
    (when method-type
      (register-type meth-name method-type))
    (register-type self-param self-type))
  (syntax-parse stx
    #:literals (let-values #%plain-lambda case-lambda)
    [(let-values ([(meth-name:id)
                   (#%plain-lambda (self-param:id . params)
                     body ...)])
       m)
     (do-register #'self-param #'meth-name)]
    [(~and (let-values ([(meth-name:id)
                         (let-values (((core:id)
                                       (#%plain-lambda params
                                                       core-body ...)))
                           method-body ...)])
             m)
           (~or kw:kw-lambda^ opt:opt-lambda^))
     (do-register #'self-param #'meth-name)]
    ;; case-lambda methods
    [(let-values ([(meth-name:id)
                   (case-lambda
                     [(self x ...) body] ...)])
       m)
     (when method-type
       (register-type #'meth-name method-type))
     (for ([self-param (in-list (syntax->list #'(self ...)))])
       (register-type self-param self-type))]
    [_ (int-err "register-method-ids: internal error")]))

;; Syntax -> Syntax
;; If the method syntax is for a keyword method, then propagate the keyword
;; property further into the syntax object.
(define (add-kw-property stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(~and (let-values ([(meth-name:id) core]) m)
           kw:kw-lambda^)
      (quasisyntax/loc stx
        (let-values ([(meth-name)
                      #,(kw-lambda-property
                        (syntax/loc stx core)
                        (attribute kw.value))])
          m))]
    [(~and (let-values ([(meth-name:id) core]) m)
           opt:opt-lambda^)
     (match-define (list required optional) (attribute opt.value))
     (quasisyntax/loc stx
       (let-values ([(meth-name)
                     #,(opt-lambda-property
                        (syntax/loc stx core)
                        (list (add1 required) ; for `this` argument
                              optional))])
         m))]
    [_ stx]))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that all the required names are actually present
;;
;; FIXME: This gives bad error messages. Consider using syntax
;;        object lists instead of sets.
(define (check-exists actual required kind)
  (define missing
    (for/or ([m (in-set required)])
      (and (not (set-member? actual m)) m)))
  (when missing
    (tc-error/fields #:delayed? #t
                     "inheritance mismatch"
                     #:more (~a "the superclass is missing a required " kind)
                     (~a "missing " kind) missing)))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that names are absent when they should be
(define (check-absent actual should-be-absent kind)
  (define present
    (for/or ([m (in-set should-be-absent)])
      (and (set-member? actual m) m)))
  (when present
    (tc-error/fields #:delayed? #t
                     "inheritance mismatch"
                     #:more (~a "the superclass has a conflicting " kind)
                     kind present)))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that the names are exactly the same as expected
(define (check-same actual expected kind)
  (define missing
    (for/or ([m (in-set expected)])
      (and (not (set-member? actual m)) m)))
  (when missing
    (tc-error/fields #:delayed? #t
                     "type mismatch"
                     #:more (~a "the class is missing a required " kind)
                     (~a "missing " kind) missing))
  (define too-many
    (for/or ([m (in-set actual)])
      (and (not (set-member? expected m)) m)))
  (when too-many
    (tc-error/fields #:delayed? #t
                     "type mismatch"
                     #:more (~a "the class has a " kind " that should be absent")
                     kind too-many)))
