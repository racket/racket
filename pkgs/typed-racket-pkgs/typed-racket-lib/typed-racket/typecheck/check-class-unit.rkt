#lang racket/unit

;; This module provides a unit for type-checking classes

(require "../utils/utils.rkt"
         racket/dict
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
         (env lexical-env)
         (types utils abbrev union subtype resolve)
         (typecheck internal-forms)
         (utils tc-utils)
         (rep type-rep)
         (for-template racket/base
                       (prefix-in c: racket/class)
                       (base-env class-prims)
                       (typecheck internal-forms)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-class^)

;; Syntax classes for use in functions below
(define-syntax-class name-pair
  (pattern (internal:id external:id)))

(define-syntax-class internal-class-data
  #:literals (#%plain-app quote-syntax class:-internal begin
              values c:init c:init-field optional-init c:field
              c:public c:override c:private)
  (pattern (begin (quote-syntax
                   (class:-internal
                    (c:init init-names:name-pair ...)
                    (c:init-field init-field-names:name-pair ...)
                    (optional-init optional-names:id ...)
                    (c:field field-names:name-pair ...)
                    (c:public public-names:name-pair ...)
                    (c:override override-names:name-pair ...)
                    (c:private private-names:id ...)))
                  (#%plain-app values))
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
           #:with override-externals #'(override-names.external ...)))

;; Syntax TCResults -> Type
;; Type-check a class form by trawling its innards
;;
;; Assumptions:
;;  by the time this is called, we can be sure that
;;  init, field, and method presence/absence is guaranteed
;;  by the local-expansion done by class:
;;
;;  we know by this point that #'form is an actual typed
;;  class produced by class: due to the syntax property
(define (check-class form [expected #f])
  (match expected
    [(tc-result1: (and self-class-type (Class: _ _ _ _)))
     (do-check form #t self-class-type)]
    [#f (do-check form #f #f)]))

;; Syntax Boolean Option<Type> -> Type
;; Do the actual type-checking
(define (do-check form expected? self-class-type)
  (syntax-parse form
    #:literals (let-values #%plain-lambda begin
                #%plain-app values letrec-syntaxes+values)
    ;; Inspect the expansion of the class macro for the pieces that
    ;; we need to type-check like superclass, methods, top-level
    ;; expressions and so on
    [(let-values ()
       (letrec-syntaxes+values ()
                               ((()
                                 ;; residual class: data
                                 data:internal-class-data))
                               (let-values (((superclass) superclass-expr)
                                            ((interfaces) interface-expr))
                                 (?#%app compose-class
                                         internal ...
                                         (#%plain-lambda (local-accessor local-mutator ??? ...)
                                                         (let-values ([(field-name) accessor-or-mutator]
                                                                      ...)
                                                           body))
                                         ????))))
     ;; Make sure the superclass is a class
     ;; FIXME: maybe should check the property on this expression
     ;;        as a sanity check too
     (define super-type (tc-expr #'superclass-expr))
     (define-values (super-inits super-fields super-methods)
       (match super-type
         ;; FIXME: should handle the case where the super class is
         ;;        polymorphic
         [(tc-result1: (Class: _ super-inits super-fields super-methods))
          (values super-inits super-fields super-methods)]
         [(tc-result1: t)
          (tc-error/expr "expected a superclass but got ~a" t
                         #:stx #'superclass-expr)
          ;; FIXME: is this the right thing to do?
          (values null null null)]))
     ;; Define sets of names for use later
     (define super-init-names (list->set (dict-keys super-inits)))
     (define super-field-names (list->set (dict-keys super-fields)))
     (define super-method-names (list->set (dict-keys super-methods)))
     (define this%-init-internals
       (list->set (append (syntax->datum #'data.init-internals)
                          (syntax->datum #'data.init-internals))))
     (define this%-public-internals
       (list->set (syntax->datum #'data.public-internals)))
     (define this%-override-internals
       (list->set (syntax->datum #'data.override-internals)))
     (define this%-method-internals
       (set-union this%-public-internals this%-override-internals))
     (define this%-field-internals
       (list->set (syntax->datum #'data.field-internals)))
     (define this%-init-names
       (list->set
        (append (syntax->datum #'data.init-externals)
                (syntax->datum #'data.init-field-externals))))
     (define this%-field-names
       (list->set
        (append (syntax->datum #'data.field-externals)
                (syntax->datum #'data.init-field-externals))))
     (define this%-public-names
       (list->set (syntax->datum #'data.public-externals)))
     (define this%-override-names
       (list->set (syntax->datum #'data.override-externals)))
     (define this%-private-names
       (list->set (syntax->datum #'(data.private-names ...))))
     (define this%-method-names
       (set-union this%-public-names this%-override-names))
     (define all-internal
       (apply append
              (map (λ (stx) (syntax->datum stx))
                   (list #'data.init-internals
                         #'data.init-field-internals
                         #'data.field-internals
                         #'data.public-internals
                         #'data.override-internals))))
     (define all-external
       (apply append
              (map (λ (stx) (syntax->datum stx))
                   (list #'data.init-externals
                         #'data.init-field-externals
                         #'data.field-externals
                         #'data.public-externals
                         #'data.override-externals))))
     ;; establish a mapping between internal and external names
     (define internal-external-mapping
       (for/hash ([internal all-internal]
                  [external all-external])
         (values internal external)))
     ;; define which init names are optional
     (define optional-inits (list->set (syntax->datum #'data.optional-inits)))
     (define optional-external (for/set ([n optional-inits])
                                 (dict-ref internal-external-mapping n)))
     ;; trawl the body for top-level expressions
     (define top-level-exprs (trawl-for-property #'body 'tr:class:top-level))
     (define internals-table (register-internals top-level-exprs))
     ;; find the `super-new` call (or error if missing)
     (define super-new-stx (trawl-for-property #'body 'tr:class:super-new))
     (define provided-super-inits (check-super-new super-new-stx super-inits))
     (define remaining-super-inits
       (for/list ([(name val) (in-dict super-inits)]
                  #:unless (member name provided-super-inits))
         (cons name val)))
     ;; Type for self in method calls
     (define self-type
       (if self-class-type
           (make-Instance self-class-type)
           (infer-self-type internals-table
                            optional-inits
                            internal-external-mapping
                            remaining-super-inits
                            super-fields
                            super-methods
                            this%-init-internals
                            this%-field-internals
                            this%-public-internals)))
     (match-define (Instance: (Class: _ inits fields methods))
                   self-type)
     ;; trawl the body for the local name table
     (define locals (trawl-for-property #'body 'tr:class:local-table))
     (define-values (local-method-table local-private-table local-field-table)
       (construct-local-mapping-tables (car locals)))
     ;; types for private elements
     (define private-method-types
       (for/hash ([(name type) (in-dict internals-table)]
                  #:when (set-member? this%-private-names name))
         (values name type)))
     ;; start type-checking elements in the body
     (define-values (lexical-names lexical-types)
       (local-tables->lexical-env local-method-table methods
                                  this%-method-internals
                                  local-field-table fields
                                  this%-field-internals
                                  local-private-table private-method-types
                                  this%-private-names
                                  self-type))
     (with-lexical-env/extend lexical-names lexical-types
       (for ([stx top-level-exprs]
             #:unless (syntax-property stx 'tr:class:super-new))
         (tc-expr stx)))
     ;; trawl the body and find methods and type-check them
     (define meths (trawl-for-property #'body 'tr:class:method))
     (define checked-method-types
       (with-lexical-env/extend lexical-names lexical-types
         (check-methods internal-external-mapping meths methods self-type)))
     (define final-class-type
       (if expected?
           self-class-type
           (merge-types self-type checked-method-types)))
     (check-method-presence-and-absence
      final-class-type
      this%-init-names this%-field-names
      this%-public-names this%-override-names
      optional-external
      remaining-super-inits super-field-names
      super-method-names)
     final-class-type]))

;; check-method-presence-and-absence : Type Set<Symbol> * 8 -> Void
;; use the internal class: information to check whether clauses
;; exist or are absent appropriately
(define (check-method-presence-and-absence
         class-type this%-init-names this%-field-names
         this%-public-names this%-override-names
         optional-external
         remaining-super-inits super-field-names
         super-method-names)
  (match-define (Class: _ inits fields methods) class-type)
  (define exp-init-names (list->set (dict-keys inits)))
  (define exp-field-names (list->set (dict-keys fields)))
  (define exp-method-names (list->set (dict-keys methods)))
  (define exp-optional-inits
    (for/set ([(name val) (in-dict inits)]
              #:when (cadr val))
             name))
  (check-same (set-union this%-init-names
                         (list->set (dict-keys remaining-super-inits)))
              exp-init-names
              "initialization argument")
  (check-same (set-union this%-public-names super-method-names)
              exp-method-names
              "public method")
  (check-same (set-union this%-field-names super-field-names)
              exp-field-names
              "public field")
  (check-same optional-external exp-optional-inits
              "optional init argument")
  (check-exists super-method-names this%-override-names
                "override method")
  (check-absent super-field-names this%-field-names "public field")
  (check-absent super-method-names this%-public-names "public method"))

;; merge-types : Type Dict<Symbol, Type> -> Type
;; Given a self object type, construct the real class type based on
;; new information found from type-checking. Only used when an expected
;; type was not provided.
(define (merge-types self-type method-types)
  (match-define (Instance: (and class-type (Class: #f inits fields methods)))
                self-type)
  (define new-methods
    (for/fold ([methods methods])
              ([(name type) (in-dict method-types)])
      (define old-type (dict-ref methods name #f))
      ;; sanity check
      (when (and old-type (not (equal? old-type type)))
        (tc-error "merge-types: internal error"))
      (dict-set methods name type)))
  (make-Class #f inits fields new-methods))

;; local-tables->lexical-env : Dict<Symbol, Id> Dict List<Symbol>
;;                             Dict<Symbol, (List Id Id)> Dict List<Symbol>
;;                             Type
;;                             -> List<Id> List<Type>
;; Construct mappings to put into the lexical type-checking environment
;; from the class local accessor mappings
;;
;; FIXME: the types we put here are fine in the expected
;;        case, but not if the class doesn't have an annotation.
;;        Then we need to hunt down annotations in a first pass.
;;        (should probably do this in expected case anyway)
(define (local-tables->lexical-env local-method-table methods method-names
                                   local-field-table fields field-names
                                   local-private-table
                                   private-types private-methods
                                   self-type)
  ;; localize to accessor names via the provided tables
  (define (localize local-table names)
    (map (λ (m) (dict-ref local-table m))
                (set->list names)))
  (define localized-method-names (localize local-method-table method-names))
  (define localized-field-pairs (localize local-field-table field-names))
  (define localized-field-get-names (map car localized-field-pairs))
  (define localized-field-set-names (map cadr localized-field-pairs))
  (define localized-private-methods
    (localize local-private-table private-methods))
  (define default-type (list (make-Univ)))

  ;; construct the types for the accessors
  (define method-types
    (for/list ([m (in-set method-names)])
      (define maybe-type (dict-ref methods m #f))
      (->* (list (make-Univ))
           (if maybe-type
               (fixup-method-type (car maybe-type) self-type)
               (make-Univ)))))
  (define field-get-types
    (for/list ([f (in-set field-names)])
      (define maybe-type (dict-ref fields f #f))
      (->* (list (make-Univ)) (or (and maybe-type (car maybe-type))
                                  (make-Univ)))))
  (define field-set-types
    (for/list ([f (in-set field-names)])
      (define maybe-type (dict-ref fields f #f))
      (->* (list (make-Univ) (or (and maybe-type
                                      (car maybe-type))
                                 -bot))
           -Void)))
  (define private-method-types
    (for/list ([f (in-set private-methods)])
      (define maybe-type (dict-ref private-types f #f))
      (or (and maybe-type (fixup-method-type maybe-type self-type))
          (make-Univ))))
  (values (append localized-method-names
                  localized-private-methods
                  localized-field-get-names
                  localized-field-set-names)
          (append method-types private-method-types
                  field-get-types field-set-types)))

;; check-methods : Listof<Syntax> Dict<Symbol, Symbol> Dict Type
;;                 -> Dict<Symbol, Type>
;; Type-check the methods inside of a class
(define (check-methods internal-external-mapping
                       meths methods self-type)
  (for/list ([meth meths])
    (define method-name (syntax-property meth 'tr:class:method))
    (define external-name (dict-ref internal-external-mapping method-name))
    (define maybe-expected (dict-ref methods external-name #f))
    (cond [maybe-expected
           (define pre-method-type (car maybe-expected))
           (define method-type
             (fixup-method-type pre-method-type self-type))
           (define expected (ret method-type))
           (define annotated (annotate-method meth self-type method-type))
           (tc-expr/check annotated expected)
           (list external-name pre-method-type)]
          [else (list external-name
                      (unfixup-method-type (tc-expr/t meth)))])))

;; Syntax -> Dict<Symbol, Id> Dict<Symbol, (List Symbol Symbol)>
;; Construct tables mapping internal method names to the accessors
;; generated inside the untyped class macro.
(define (construct-local-mapping-tables stx)
  (syntax-parse stx
    #:literals (let-values #%plain-app #%plain-lambda values)
    ;; See base-env/class-prims.rkt to see how this in-syntax
    ;; table is constructed at the surface syntax
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
                    ...)])
       (#%plain-app void))
     (values (map cons
                  (syntax->datum #'(method ...))
                  (syntax->list #'(local-method ...)))
             (map cons
                  (syntax->datum #'(private ...))
                  (syntax->list #'(local-private ...)))
             (map list
                  (syntax->datum #'(field ...))
                  (syntax->list #'(local-field-get ...))
                  (syntax->list #'(local-field-set ...))))]))

;; check-super-new : Listof<Syntax> Inits -> Void
;; Check if the super-new call is well-typed
(define (check-super-new super-new-stx super-inits)
  (cond [(null? super-new-stx)
         (tc-error/expr
          "typed classes must call super-new at the class top-level")
         null]
        [(> (length super-new-stx) 1)
         (tc-error/expr
          "typed classes must only call super-new a single time")
         null]
        [(= (length super-new-stx) 1)
         (syntax-parse (car super-new-stx)
           #:literals (#%plain-app list cons quote)
           [(#%plain-app super-go _ _ _ _ _
                         (#%plain-app
                          list
                          (#%plain-app cons (quote init-id) arg:expr)
                          ...))
            (define provided-inits (syntax->datum #'(init-id ...)))
            (for ([(name val) (in-dict super-inits)]
                  #:when (not (cadr val)))
              (unless (member name provided-inits)
                (tc-error/expr "mandatory superclass init ~a not provided"
                               name)))
            (for ([init-id provided-inits]
                  [init-arg (syntax->list #'(arg ...))])
              (define maybe-expected (dict-ref super-inits init-id #f))
              (if maybe-expected
                  (tc-expr/check init-arg (ret (car maybe-expected)))
                  (tc-error/expr "init argument ~a not accepted by superclass"
                                 init-id)))
            provided-inits])]))

;; Syntax -> Listof<Syntax>
;; Look through the expansion of the class macro in search for
;; syntax with some property (e.g., methods)
(define (trawl-for-property form prop)
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                           letrec-syntaxes+values)
    [stx
     #:when (syntax-property form prop)
     (list form)]
    [(let-values (b ...)
       body)
     (trawl-for-property #'body prop)]
    [(letrec-values (b ...)
                    body)
     (trawl-for-property #'body prop)]
    [(letrec-syntaxes+values (sb ...) (vb ...)
                             body)
     (trawl-for-property #'body prop)]
    [(#%plain-app e ...)
     (apply append (map (λ (stx) (trawl-for-property stx prop))
                        (syntax->list #'(e ...))))]
    [(#%plain-lambda (x ...) e ...)
     (apply append (map (λ (stx) (trawl-for-property stx prop))
                        (syntax->list #'(e ...))))]
    [_ '()]))

;; register-internals : Listof<Syntax> -> Dict<Symbol, Type>
;; Find : annotations and register them
;; TODO: support `define-type`?
(define (register-internals stxs)
  (for/fold ([table '()])
            ([stx stxs])
    (syntax-parse stx
      #:literals (let-values begin quote-syntax :-internal
                  #%plain-app values void)
      [(let-values ((()
                     (begin
                       (quote-syntax (:-internal name:id type:expr))
                       (#%plain-app values))))
         (#%plain-app void))
       (cons (cons (syntax-e #'name) (parse-type #'type))
             table)]
      [_ table])))

;; infer-self-type : Dict<Symbol, Type> Set<Symbol> Dict<Symbol, Symbol>
;;                   Inits Fields Methods
;;                   Set<Symbol> * 3 -> Type
;; Construct a self object type based on the registered types
;; from : inside the class body.
(define (infer-self-type internals-table optional-inits
                         internal-external-mapping
                         super-inits super-fields super-methods
                         inits fields publics)
  (define (make-type-dict names supers [inits? #f])
    (for/fold ([type-dict supers])
              ([name names])
      (define external (dict-ref internal-external-mapping name))
      (cond [(dict-ref internals-table name #f) =>
             (λ (type)
               (define entry
                 (if inits?
                     (list type (set-member? optional-inits name))
                     (list type)))
               (dict-set type-dict external entry))]
            [else type-dict])))
  (define init-types (make-type-dict inits super-inits #t))
  (define field-types (make-type-dict fields super-fields))
  (define public-types (make-type-dict publics super-methods))
  (make-Instance (make-Class #f init-types field-types public-types)))

;; fixup-method-type : Function Type -> Function
;; Fix up a method's arity from a regular function type
(define (fixup-method-type type self-type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs])
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cons self-type doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [_ (tc-error "fixup-method-type: internal error")]))

;; unfixup-method-type : Function -> Function
;; Turn a "real" method type back into a function type
;; FIXME: this is a really badly named function
(define (unfixup-method-type type)
  (match type
    [(Function: (list arrs ...))
     (define fixed-arrs
       (for/list ([arr arrs])
         (match-define (arr: doms rng rest drest kws) arr)
         (make-arr (cdr doms) rng rest drest kws)))
     (make-Function fixed-arrs)]
    [_ (tc-error "fixup-method-type: internal error")]))

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
                     (#%plain-lambda (#,annotated-self-param id ...)
                       body ...)])
         m)]
    [_ (tc-error "annotate-method: internal error")]))

;; Set<Symbol> Set<Symbol> String -> Void
;; check that all the required names are actually present
(define (check-exists actual required msg)
  (define missing
    (for/or ([m (in-set required)])
      (and (not (set-member? actual m)) m)))
  (when missing
    ;; FIXME: make this a delayed error? Do it for every single
    ;;        name separately?
    (tc-error/expr "class definition missing ~a ~a" msg missing)))

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
    (tc-error/expr "class definition missing ~a ~a" msg missing))
  (define too-many
    (for/or ([m (in-set actual)])
      (and (not (set-member? expected m)) m)))
  (when too-many
    (tc-error/expr "class definition has unexpected ~a ~a"
                   msg too-many)))

;; check-no-extra : Set<Symbol> Set<Symbol> -> Void
;; check that the actual names don't include names not in the
;; expected type (i.e., the names must exactly match up)
(define (check-no-extra actual expected)
  (printf "actual : ~a expected : ~a~n" actual expected)
  (unless (subset? actual expected)
    ;; FIXME: better error reporting here
    (tc-error/expr "class defines names not in expected type")))

;; I wish I could write this
#;
(module+ test
  (check-equal? (fixup-method-type (parse-type #'(Integer -> Integer)))
                (parse-type #'(Any Integer -> Integer))))

