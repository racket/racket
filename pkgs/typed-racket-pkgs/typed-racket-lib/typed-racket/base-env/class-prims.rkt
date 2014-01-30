#lang racket/base

;; This module provides TR primitives for classes and objects

(require (rename-in racket/class [class untyped-class])
         "colon.rkt"
         "../typecheck/internal-forms.rkt"
         (for-syntax
          racket/base
          racket/class
          racket/dict
          racket/list
          racket/match
          racket/pretty ;; get rid of this later
          racket/syntax
          racket/private/classidmap ;; this is bad
          syntax/flatten-begin
          syntax/id-table
          syntax/kerncase
          syntax/parse
          syntax/stx
          unstable/list
          "../utils/tc-utils.rkt"
          "../types/utils.rkt"))

(provide ;; Typed class macro that coordinates with TR
         class
         ;; for use in ~literal clauses
         class-internal
         optional-init
         private-field
         :-augment)

;; give it a binding, but it shouldn't be used directly
(define-syntax (class-internal stx)
  (raise-syntax-error 'class "should only be used internally"))

(define-syntax (optional-init stx)
  (raise-syntax-error 'class "should only be used internally"))

(define-syntax (private-field stx)
  (raise-syntax-error 'class "should only be used internally"))

(define-syntax (:-augment stx)
  (raise-syntax-error 'class "should only be used internally"))

(begin-for-syntax
 (module+ test (require rackunit))

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
   (pattern (~seq #:forall type-variable:id)
            #:attr type-variables #'(type-variable))
   (pattern (~seq #:forall (type-variable:id ...))
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
            ;; in the future, use a data structure and
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
             ([clause clauses])
     (if (dict-has-key? clauses (clause-kind clause))
         (dict-update clauses (clause-kind clause)
                      (λ (old-names)
                        (append old-names (clause-ids clause))))
         (dict-set clauses
                   (clause-kind clause)
                   (clause-ids clause)))))

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
     ;; try using syntax-local-expand-expression?
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
               (define/with-syntax expanded-rhs
                 (local-transformer-expand #'rhs 'expression null))
               (syntax-local-bind-syntaxes
                (syntax->list #'(name ...)) #'expanded-rhs def-ctx)
               (cons #'(define-syntaxes (name ...) expanded-rhs)
                     (loop (cdr stxs)))]
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
   (add-kind #'pubment))
 
 (module+ test
   ;; equal? check but considers id & stx pair equality
   (define (equal?/id x y)
     (cond [(and (identifier? x) (identifier? y))
            (free-identifier=? x y)]
           [(and (syntax? x) (syntax? y))
            (and (free-identifier=? (stx-car x) (stx-car y))
                 (free-identifier=? (stx-car (stx-cdr x))
                                    (stx-car (stx-cdr y))))]
           (equal?/recur x y equal?/id)))

   ;; utility macro for checking if a syntax matches a
   ;; given syntax class
   (define-syntax-rule (syntax-parses? stx syntax-class)
     (syntax-parse stx
       [(~var _ syntax-class) #t]
       [_ #f]))

   ;; for rackunit with equal?/id
   (define-binary-check (check-equal?/id equal?/id actual expected))

   (check-true (syntax-parses? #'x init-decl))
   (check-true (syntax-parses? #'([x y]) init-decl))
   (check-true (syntax-parses? #'(x 0) init-decl))
   (check-true (syntax-parses? #'([x y] 0) init-decl))
   (check-true (syntax-parses? #'(init x y z) class-clause))
   (check-true (syntax-parses? #'(public f g h) class-clause))
   (check-true (syntax-parses? #'(public f) class-clause-or-other))
   (check-equal?/id
    (extract-names (list (clause #'(init x y z)
                                 #'init
                                 (list #'(x x) #'(y y) #'(z z)))
                         (clause #'(public f g h)
                                 #'public
                                 (list #'(f f) #'(g g) #'(h h)))))
    (make-immutable-free-id-table
     (hash #'public (list #'(f f) #'(g g) #'(h h))
           #'init (list #'(x x) #'(y y) #'(z z)))))))

(define-syntax (class stx)
  (syntax-parse stx
    [(_ super forall:maybe-type-parameter e ...)
     ;; FIXME: potentially needs to expand super clause?
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
        (define annotated-super
          (syntax-property #'super 'tr:class:super #t))
        (define optional-inits (get-optional-inits clauses))
        (syntax-property
         (syntax-property
          #`(let-values ()
              #,(internal
                 ;; FIXME: maybe put this in a macro and/or a syntax class
                 ;;        so that it's easier to deal with
                 #`(class-internal
                    (#:forall #,@(attribute forall.type-variables))
                    (init #,@(dict-ref name-dict #'init '()))
                    (init-field #,@(dict-ref name-dict #'init-field '()))
                    (init-rest #,@(dict-ref name-dict #'init-rest '()))
                    (optional-init #,@optional-inits)
                    (field #,@(dict-ref name-dict #'field '()))
                    (public #,@(dict-ref name-dict #'public '()))
                    (override #,@(dict-ref name-dict #'override '()))
                    (private #,@(dict-ref name-dict #'private '()))
                    (private-field #,@private-fields)
                    (inherit #,@(dict-ref name-dict #'inherit '()))
                    (inherit-field #,@(dict-ref name-dict #'inherit-field '()))
                    (augment #,@(dict-ref name-dict #'augment '()))
                    (pubment #,@(dict-ref name-dict #'pubment '()))))
              (untyped-class #,annotated-super
                #,@(map clause-stx clauses)
                ;; construct in-body type annotations for clauses
                #,@(apply append
                          (for/list ([a-clause clauses])
                            (match-define (clause _1 _2 ids types) a-clause)
                            (for/list ([id ids] [type types]
                                       #:when type)
                              (syntax-property
                               (syntax-property
                                #`(: #,(if (stx-pair? id) (stx-car id) id)
                                     #,type)
                                'tr:class:top-level #t)
                               'tr:class:type-annotation #t))))
                #,@(map non-clause-stx annotated-methods)
                #,(syntax-property
                   #`(begin #,@(map non-clause-stx other-top-level))
                   'tr:class:top-level #t)
                #,(make-locals-table name-dict private-fields)))
          'tr:class #t)
         'typechecker:ignore #t)])]))

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
         #:when (memf (λ (n) (free-identifier=? #'id n))
                      (append (stx-map stx-car (dict-ref name-dict #'public '()))
                              (stx-map stx-car (dict-ref name-dict #'pubment '()))
                              (stx-map stx-car (dict-ref name-dict #'override '()))
                              (stx-map stx-car (dict-ref name-dict #'augment '()))
                              (dict-ref name-dict #'private '())))
         (values (cons (non-clause (syntax-property stx
                                                    'tr:class:method
                                                    (syntax-e #'id)))
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
           (non-clause (syntax-property #'(quote-syntax (:-augment name augment-type))
                                        'tr:class:type-annotation #t)))
         (define plain-annotation
           (non-clause (syntax-property (syntax/loc stx (: name type))
                                        'tr:class:type-annotation #t)))
         (values methods
                 (append rest-top (list plain-annotation new-clause))
                 private-fields)]
        ;; Just process this to add the property
        [(: name:id type:expr)
         (define plain-annotation
           (non-clause (syntax-property (syntax/loc stx (: name type))
                                        'tr:class:type-annotation #t)))
         (values methods
                 (append rest-top (list plain-annotation))
                 private-fields)]
        ;; Identify super-new for the benefit of the type checker
        [(~or (super-new [init-id init-expr] ...)
              (super-make-object init-expr ...)
              (super-instantiate (init-expr ...) [name expr] ...))
         (define new-non-clause
           (non-clause (syntax-property stx 'tr:class:super-new #t)))
         (values methods (append rest-top (list new-non-clause))
                 private-fields)]
        [_ (values methods (append rest-top (list content))
                   private-fields)])))

  ;; get-optional-inits : Listof<Clause> -> Listof<Id>
  ;; Get a list of the internal names of mandatory inits
  (define (get-optional-inits clauses)
    (flatten
     (for/list ([clause clauses]
                #:when (init-clause? clause))
       (for/list ([id-pair (in-list (stx->list (clause-ids clause)))]
                  [optional? (in-list (init-clause-optional? clause))]
                  #:when optional?)
         (stx-car id-pair)))))

  (module+ test
    (check-equal?/id
     (get-optional-inits
      (list (init-clause #'(init [x 0]) #'init #'([x x]) (list #t))
            (init-clause #'(init [(a b)]) #'init #'([a b]) (list #f))))
     (list #'x)))

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
    (syntax-property
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
     'tr:class:local-table #t)))

