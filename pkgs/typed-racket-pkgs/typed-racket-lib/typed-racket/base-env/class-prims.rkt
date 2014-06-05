#lang racket/base

;; This module provides TR primitives for classes and objects

(require (rename-in (except-in racket/class
                               define/public
                               define/override
                               define/pubment
                               define/augment
                               define/private)
                    [class untyped-class])
         "colon.rkt"
         "../typecheck/internal-forms.rkt"
         "../private/class-literals.rkt"
         (only-in "prims.rkt" [define tr:define])
         (for-syntax
          racket/base
          racket/class
          racket/dict
          racket/list
          racket/match
          racket/syntax
          ;; Note: This imports `generate-class-expand-context` from
          ;;       the internals of the class system. It's needed for
          ;;       local expansion to establish the right context, but it
          ;;       is hacky.
          racket/private/classidmap
          syntax/flatten-begin
          syntax/id-table
          syntax/kerncase
          syntax/parse
          syntax/stx
          unstable/list
          "annotate-classes.rkt"
          "../private/syntax-properties.rkt"
          "../utils/tc-utils.rkt"))

(provide ;; Typed class macro that coordinates with TR
         class
         ;; override these macros to use TR define
         define/public
         define/override
         define/pubment
         define/augment
         define/private)

;; overriden forms
(define-syntax-rule (define-define/class-kw ((?id ?class-kw) ...))
  (begin (define-syntax (?id stx)
           (syntax-parse stx
             [(_ ??header:curried-formals . ??body)
              (quasisyntax/loc stx
                (begin (tr:define ??header . ??body)
                       (?class-kw ??header.fun-name)))]))
         ...))

(define-define/class-kw
  ([define/public public]
   [define/override override]
   [define/pubment pubment]
   [define/augment augment]
   [define/private private]))

(begin-for-syntax
 ;; forms that are not allowed by Typed Racket yet
 (define unsupported-forms
   (list (quote-syntax augride)
         ;; FIXME: see if override contracts are enough
         ;;        to keep these at bay or whether they
         ;;        need to be handled
         (quote-syntax public-final)
         (quote-syntax override-final)
         (quote-syntax augment-final)
         (quote-syntax overment)
         (quote-syntax abstract)
         (quote-syntax rename-super)
         (quote-syntax inherit/super)
         (quote-syntax inherit/inner)
         (quote-syntax rename-inner)))

 ;; basically the same stop forms that class-internal uses
 (define stop-forms
   (append (kernel-form-identifier-list)
           unsupported-forms
           (list
            (quote-syntax :)
            (quote-syntax #%app)
            (quote-syntax lambda)
            (quote-syntax init)
            (quote-syntax field)
            (quote-syntax init-field)
            (quote-syntax init-rest)
            (quote-syntax inherit-field)
            (quote-syntax private)
            (quote-syntax public)
            (quote-syntax override)
            (quote-syntax pubment)
            (quote-syntax augment)
            (quote-syntax inherit)
            (quote-syntax super)
            (quote-syntax inner)
            (quote-syntax this)
            (quote-syntax this%)
            (quote-syntax super-new)
            (quote-syntax super-instantiate)
            (quote-syntax super-make-object)
            (quote-syntax inspect)))))

;; export some syntax-time definitions for testing purposes
(module+ internal
  (provide (for-syntax init-decl class-clause class-clause-or-other
                       extract-names clause init-clause get-optional-inits)))

(begin-for-syntax
 ;; A Clause is a (clause Syntax Id Listof<Syntax> Listof<Option<Type>>)
 ;;
 ;; interp. a class clause such as init or field.
 ;;   stx   - the syntax of the entire clause with types erased
 ;;   kind  - the kind of clause (e.g., init, field)
 ;;   ids   - list of the ids defined in this clause
 ;;   types - types for each id, #f if a type is not provided
 (struct clause (stx kind ids types))

 ;; An InitClause is a (init-clause Syntax Id Listof<Syntax> Boolean)
 ;;
 ;; interp. an init class clause
 (struct init-clause clause (optional?))

 ;; A NonClause is a (non-clause Syntax)
 ;;
 ;; interp. a top-level class expression that is not one of the special
 ;;         class clauses such as init or field.
 (struct non-clause (stx))

 (define-literal-set class-literals
   (:))

 (define-splicing-syntax-class maybe-type-parameter
   (pattern (~seq (~or #:∀ #:forall) (type-variable:id ...))
            #:attr type-variables #'(type-variable ...))
   (pattern (~seq)
            #:attr type-variables #'()))

 ;; interp:
 ;;   optional? - optional init arg or not (has default value or not)
 ;;   ids       - internal and external id for this argument
 ;;   type      - type annotation, if any
 ;;   form      - type erased form
 (define-syntax-class init-decl
   #:attributes (optional? ids type form)
   #:literal-sets (class-literals)
   (pattern id:id
            #:attr optional? #f
            #:with ids #'(id id)
            #:attr type #f
            #:with form this-syntax)
   (pattern (id:id : type:expr)
            #:attr optional? #f
            #:with ids #'(id id)
            #:with form #'id)
   (pattern (ren:renamed (~optional (~seq : type:expr)))
            #:attr optional? #f
            #:with ids #'ren.ids
            #:with form #'(ren))
   (pattern (mren:maybe-renamed
             (~optional (~seq : type:expr))
             default-value:expr)
            #:attr optional? #t
            #:with ids #'mren.ids
            #:with form #'(mren default-value)))

 (define-syntax-class field-decl
   #:attributes (ids type form)
   #:literal-sets (class-literals)
   (pattern (mren:maybe-renamed
             (~optional (~seq : type:expr))
             default-value:expr)
            #:with ids #'mren.ids
            #:with form #'(mren default-value)))

 (define-syntax-class method-decl
   #:attributes (ids type form)
   #:literal-sets (class-literals)
   (pattern mren:maybe-renamed
            #:with ids #'mren.ids
            #:attr type #f
            #:with form this-syntax)
   (pattern (mren:maybe-renamed : type:expr)
            #:with ids #'mren.ids
            #:with form #'mren))

 (define-syntax-class private-decl
   #:attributes (id type form)
   #:literal-sets (class-literals)
   (pattern id:id
            #:attr type #f
            #:with form this-syntax)
   (pattern (id:id : type:expr)
            #:with form #'id))

 (define-syntax-class renamed
   #:attributes (ids)
   (pattern (internal-id:id external-id:id)
            #:with ids #'(internal-id external-id)))

 (define-syntax-class maybe-renamed
   #:attributes (ids)
   (pattern id:id
            #:with ids #'(id id))
   (pattern ren:renamed
            #:with ids #'ren.ids))

 (define-syntax-class init-like-clause-names
   (pattern (~or (~literal init)
                 (~literal init-field))))

 ;; matches ids with clauses shaped like method clauses,
 ;; not necessarily clauses that declare methods
 (define-syntax-class method-like-clause-names
   (pattern (~or (~literal inherit-field)
                 (~literal public)
                 (~literal pubment)
                 (~literal public-final)
                 (~literal override)
                 (~literal overment)
                 (~literal override-final)
                 (~literal augment)
                 (~literal augride)
                 (~literal augment-final)
                 (~literal inherit)
                 (~literal inherit/super)
                 (~literal inherit/inner)
                 (~literal rename-super))))

 (define-syntax-class private-like-clause-names
   (pattern (~or (~literal private)
                 (~literal abstract))))

 (define-syntax-class class-clause
   (pattern (clause-name:init-like-clause-names names:init-decl ...)
            ;; FIXME: in the future, use a data structure and
            ;; make this an attribute instead to represent
            ;; internal and external names
            #:attr data
            (init-clause #'(clause-name names.form ...)
                         #'clause-name
                         (stx->list #'(names.ids ...))
                         (attribute names.type)
                         (attribute names.optional?)))
   (pattern ((~literal init-rest) name:private-decl)
            #:attr data (clause #'(init-rest name.form)
                                #'init-rest
                                (stx->list #'(name.id))
                                (list (attribute name.type))))
   (pattern ((~literal field) names:field-decl ...)
            #:attr data (clause #'(field names.form ...)
                                #'field
                                (stx->list #'(names.ids ...))
                                (attribute names.type)))
   (pattern (clause-name:method-like-clause-names names:method-decl ...)
            #:attr data
            (clause #'(clause-name names.form ...)
                    #'clause-name
                    (stx->list #'(names.ids ...))
                    (attribute names.type)))
   (pattern (clause-name:private-like-clause-names names:private-decl ...)
            #:attr data
            (clause #'(clause-name names.form ...)
                    #'clause-name
                    (stx->list #'(names.id ...))
                    (attribute names.type))))

 (define-syntax-class class-clause-or-other
   #:attributes (data)
   (pattern :class-clause)
   (pattern e:expr #:attr data (non-clause #'e)))

 ;; Listof<Clause> -> Dict<Identifier, Names>
 ;; Extract names from init, public, etc. clauses
 (define (extract-names clauses)
   (for/fold ([clauses (make-immutable-free-id-table)])
             ([clause (in-list clauses)])
     (dict-update clauses (clause-kind clause)
                  (λ (old-names)
                    (append old-names (clause-ids clause)))
                  '())))

 ;; FIXME: less magic
 ;; magic used to disarm syntax after expansion
 (define class-insp (variable-reference->module-declaration-inspector
                      (#%variable-reference)))
 (define (disarm stx)
   (syntax-disarm stx class-insp))

 ;; Expand the syntax inside the class body
 ;; this is mostly cribbed from class-internal.rkt
 (define (expand-expressions stxs ctx def-ctx)
   (define (class-expand stx)
     (local-expand stx ctx stop-forms def-ctx))
   (let loop ([stxs stxs])
     (cond [(null? stxs) null]
           [else
            (define stx (disarm (class-expand (car stxs))))
            (syntax-parse stx
              #:literals (begin define-syntaxes define-values)
              [(begin . _)
               (loop (append (flatten-begin stx) (cdr stxs)))]
              ;; Handle syntax definitions in the expanded syntax
              ;; i.e., macro definitions in the class body
              ;; see class-internal.rkt as well
              [(define-syntaxes (name:id ...) rhs:expr)
               (syntax-local-bind-syntaxes
                (syntax->list #'(name ...)) #'rhs def-ctx)
               (cons stx (loop (cdr stxs)))]
              [(define-values (name:id ...) rhs:expr)
               (syntax-local-bind-syntaxes
                (syntax->list #'(name ...)) #f def-ctx)
               (cons stx (loop (cdr stxs)))]
              [_ (cons stx (loop (cdr stxs)))])])))

 ;; add-names-to-intdef-context : Intdef-Ctx Dict<Id, Names> -> Void
 ;; Establish accessor names in the internal definition context
 ;; to avoid unbound identifier errors at this level
 (define (add-names-to-intdef-context def-ctx name-dict)
   (define (add-kind kind)
     (define names (map stx-car (dict-ref name-dict kind null)))
     (syntax-local-bind-syntaxes names #f def-ctx))
   (add-kind #'init-field)
   (add-kind #'field)
   (add-kind #'public)
   (add-kind #'pubment)))

(define-syntax (class stx)
  (syntax-parse stx
    [(_ super forall:maybe-type-parameter e ...)
     (define class-ctx (generate-class-expand-context))
     (define def-ctx (syntax-local-make-definition-context))
     (define expanded-stx
       (expand-expressions (syntax->list #'(e ...)) class-ctx def-ctx))
     (syntax-parse expanded-stx
       [(class-elems:class-clause-or-other ...)
        (define-values (clauses others)
          (filter-multiple (attribute class-elems.data)
                           clause?
                           non-clause?))
        (define name-dict (extract-names clauses))
        (check-unsupported-features name-dict)
        (add-names-to-intdef-context def-ctx name-dict)
        (internal-definition-context-seal def-ctx)
        (define-values (annotated-methods other-top-level private-fields)
          (process-class-contents others name-dict))
        (define annotated-super (tr:class:super-property #'super #t))
        (define ordered-inits (get-all-init-names clauses))
        (define optional-inits (get-optional-inits clauses))
        (ignore
         (tr:class
          (quasisyntax/loc stx
           (let-values ()
             #,(internal (make-class-name-table (attribute forall.type-variables)
                                                private-fields
                                                ordered-inits
                                                optional-inits
                                                name-dict))
             (untyped-class #,annotated-super
               #,@(map clause-stx clauses)
               ;; construct in-body type annotations for clauses
               #,@(apply append
                         (for/list ([a-clause clauses])
                           (match-define (clause _1 _2 ids types) a-clause)
                           (for/list ([id ids] [type types]
                                      #:when type)
                             ;; FIXME: it might be cleaner to use the type-label-property
                             ;;        here and use the property to build annotation tables
                             ;;        in the class type-checker.
                             (tr:class:type-annotation-property
                              (tr:class:top-level-property
                               #`(: #,(if (stx-pair? id) (stx-car id) id)
                                    #,type)
                               #t)
                              #t))))
               #,@(map non-clause-stx annotated-methods)
               #,(tr:class:top-level-property
                  #`(begin #,@(map non-clause-stx other-top-level))
                  #t)
               #,(make-locals-table name-dict private-fields))))))])]))

(begin-for-syntax
  ;; process-class-contents : Listof<Syntax> Dict<Id, Listof<Id>>
  ;;                          -> Listof<Syntax> Listof<Syntax> Listof<Syntax>
  ;; Process methods and other top-level expressions and definitions
  ;; that aren't class clauses like `init` or `public`
  (define (process-class-contents contents name-dict)
    (for/fold ([methods '()]
               [rest-top '()]
               [private-fields '()])
              ([content contents])
      (define stx (non-clause-stx content))
      (syntax-parse stx
        #:literals (: define-values super-new
                    super-make-object super-instantiate)
        ;; if it's a method definition for a declared method, then
        ;; mark it as something to type-check
        [(define-values (id) . rst)
         #:when (method-id? #'id name-dict)
         (values (cons (non-clause (tr:class:method-property stx (syntax-e #'id)))
                       methods)
                 rest-top private-fields)]
        ;; private field definition
        [(define-values (id ...) . rst)
         (values methods
                 (append rest-top (list content))
                 (append (syntax->list #'(id ...))
                         private-fields))]
        ;; special : annotation for augment interface
        [(: name:id type:expr #:augment augment-type:expr)
         (define new-clause
           (non-clause (tr:class:type-annotation-property
                        #'(quote-syntax (:-augment name augment-type)) #t)))
         (define plain-annotation
           (non-clause (tr:class:type-annotation-property
                        (syntax/loc stx (: name type)) #t)))
         (values methods
                 (append rest-top (list plain-annotation new-clause))
                 private-fields)]
        ;; Just process this to add the property
        [(: name:id . rst)
         (define plain-annotation
           (non-clause (tr:class:type-annotation-property
                        (syntax/loc stx (: name . rst)) #t)))
         (values methods
                 (append rest-top (list plain-annotation))
                 private-fields)]
        ;; Identify super-new for the benefit of the type checker
        [(~or (super-new [init-id init-expr] ...)
              (super-make-object init-expr ...)
              (super-instantiate (init-expr ...) [name expr] ...))
         (define new-non-clause
           (non-clause (tr:class:super-new-property stx #t)))
         (values methods (append rest-top (list new-non-clause))
                 private-fields)]
        [_ (values methods (append rest-top (list content))
                   private-fields)])))

  ;; method-id? : Id Dict<Id, Id> -> Boolean
  ;; Check whether the given id is a known method name
  (define (method-id? id name-dict)
    (memf (λ (n) (free-identifier=? id n))
          (append (stx-map stx-car (dict-ref name-dict #'public '()))
                  (stx-map stx-car (dict-ref name-dict #'pubment '()))
                  (stx-map stx-car (dict-ref name-dict #'override '()))
                  (stx-map stx-car (dict-ref name-dict #'augment '()))
                  (dict-ref name-dict #'private '()))))

  ;; get-optional-inits : Listof<Clause> -> Listof<Id>
  ;; Get a list of the internal names of optional inits
  (define (get-optional-inits clauses)
    (flatten
     (for/list ([clause clauses]
                #:when (init-clause? clause))
       (for/list ([id-pair (in-list (stx->list (clause-ids clause)))]
                  [optional? (in-list (init-clause-optional? clause))]
                  #:when optional?)
         (stx-car id-pair)))))

  ;; get-all-init-names : Listof<Clause> -> Listof<Id>
  ;; Get a list of all the (internal) init names in order
  (define (get-all-init-names clauses)
    (flatten
     (for/list ([clause clauses]
                #:when (init-clause? clause))
       (stx-map stx-car (clause-ids clause)))))

  ;; check-unsupported-features : Dict<Identifier, Names> -> Void
  ;; Check if features that are not supported were used and
  ;; raise an error if they are present
  (define (check-unsupported-features id-table)
    (for ([form unsupported-forms])
      (define entry (dict-ref id-table form null))
      (unless (null? entry)
        (tc-error/stx
         (car entry)
         "unsupported class clause: ~a"
         (syntax-e form)))))

  ;; make-class-name-table : Listof<Id> Listof<Id> Listof<Id>
  ;;                         Listof<Id> Dict<Id, Id> -> Stx
  ;; construct syntax used by the class type-checker as a reliable source
  ;; for the member names that are in a given class, plus any type
  ;; variables that are bound
  (define (make-class-name-table foralls
                                 private-fields
                                 ordered-inits
                                 optional-inits
                                 name-dict)
    #`(class-internal
       (#:forall #,@foralls)
       (#:all-inits #,@ordered-inits)
       (#:init #,@(dict-ref name-dict #'init '()))
       (#:init-field #,@(dict-ref name-dict #'init-field '()))
       (#:init-rest #,@(dict-ref name-dict #'init-rest '()))
       (#:optional-init #,@optional-inits)
       (#:field #,@(dict-ref name-dict #'field '()))
       (#:public #,@(dict-ref name-dict #'public '()))
       (#:override #,@(dict-ref name-dict #'override '()))
       (#:private #,@(dict-ref name-dict #'private '()))
       (#:private-field #,@private-fields)
       (#:inherit #,@(dict-ref name-dict #'inherit '()))
       (#:inherit-field #,@(dict-ref name-dict #'inherit-field '()))
       (#:augment #,@(dict-ref name-dict #'augment '()))
       (#:pubment #,@(dict-ref name-dict #'pubment '()))))

  ;; This is a neat/horrible trick
  ;;
  ;; In order to detect the mappings that class-internal.rkt has
  ;; created for class-local field and method access, we construct
  ;; a in-syntax table mapping original names to the accessors.
  ;; The identifiers inside the lambdas below will expand via
  ;; set!-transformers to the appropriate accessors, which lets
  ;; us figure out the accessor identifiers.
  (define (make-locals-table name-dict private-field-names)
    (define public-names
      (stx-map stx-car (dict-ref name-dict #'public '())))
    (define override-names
      (stx-map stx-car (dict-ref name-dict #'override '())))
    (define private-names (dict-ref name-dict #'private '()))
    (define field-names
      (append (stx-map stx-car (dict-ref name-dict #'field '()))
              (stx-map stx-car (dict-ref name-dict #'init-field '()))))
    (define init-names
      (stx-map stx-car (dict-ref name-dict #'init '())))
    (define init-rest-name (dict-ref name-dict #'init-rest '()))
    (define inherit-names
      (stx-map stx-car (dict-ref name-dict #'inherit '())))
    (define inherit-field-names
      (stx-map stx-car (dict-ref name-dict #'inherit-field '())))
    (define augment-names
      (append (stx-map stx-car (dict-ref name-dict #'pubment '()))
              (stx-map stx-car (dict-ref name-dict #'augment '()))))
    (tr:class:local-table-property
     #`(let-values ([(#,@public-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx)))
                                     public-names))]
                    [(#,@private-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx)))
                                     private-names))]
                    [(#,@field-names)
                     (values #,@(map (λ (stx) #`(λ () #,stx (set! #,stx 0)))
                                     field-names))]
                    [(#,@private-field-names)
                     (values #,@(map (λ (stx) #`(λ () #,stx (set! #,stx 0)))
                                     private-field-names))]
                    [(#,@inherit-field-names)
                     (values #,@(map (λ (stx) #`(λ () #,stx (set! #,stx 0)))
                                     inherit-field-names))]
                    [(#,@init-names)
                     (values #,@(map (λ (stx) #`(λ () #,stx))
                                     init-names))]
                    [(#,@init-rest-name)
                     (values #,@(map (λ (stx) #`(λ () #,stx))
                                     init-rest-name))]
                    [(#,@inherit-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx)))
                                     inherit-names))]
                    [(#,@override-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx) (super #,stx)))
                                     override-names))]
                    [(#,@augment-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx) (inner #f #,stx)))
                                     augment-names))])
         (void))
     #t)))

