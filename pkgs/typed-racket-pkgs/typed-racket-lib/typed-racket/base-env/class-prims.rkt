#lang racket/base

;; This module provides TR primitives for classes and objects

(require (prefix-in untyped: racket/class)
         "class-clauses.rkt"
         "colon.rkt"
         "../typecheck/internal-forms.rkt"
         "../private/class-literals.rkt"
         (only-in "prims.rkt" [define tr:define])
         (for-syntax
          racket/base
          racket/class
          racket/list
          racket/match
          racket/syntax
          syntax/kerncase
          syntax/parse
          syntax/stx
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
         define/private
         ;; override these for type annotations
         init
         init-field
         field
         inherit-field
         init-rest
         public
         pubment
         override
         augment
         private
         inherit)

;; overriden define forms
(define-syntax-rule (define-define/class-kw ((?id ?class-kw) ...))
  (begin (define-syntax (?id stx)
           (syntax-parse stx
             [(_ ??header:curried-formals . ??body)
              (quasisyntax/loc stx
                (begin (tr:define ??header . ??body)
                       (?class-kw ??header.fun-name)))]))
         ...))

(define-define/class-kw
  ([define/public   public]
   [define/override override]
   [define/pubment  pubment]
   [define/augment  augment]
   [define/private  private]))

(begin-for-syntax
 ;; TRClassInfo stores information in the class macro that lets the
 ;; TR class helper macros coordinate amongst each other.
 ;;
 ;; It is a (tr-class-info List<Clause> List<Identifier>)
 ;;
 ;; clauses        - stores in reverse order all class clauses that appeared
 ;;                  in the class expression
 ;; private-fields - a list of private field names
 (struct tr-class-info (clauses private-fields) #:mutable)

 ;; forms that are not allowed by Typed Racket yet
 (define unsupported-forms
   (list (quote-syntax untyped:augride)
         ;; FIXME: see if override contracts are enough
         ;;        to keep these at bay or whether they
         ;;        need to be handled
         (quote-syntax untyped:public-final)
         (quote-syntax untyped:override-final)
         (quote-syntax untyped:augment-final)
         (quote-syntax untyped:overment)
         (quote-syntax untyped:abstract)
         (quote-syntax untyped:rename-super)
         (quote-syntax untyped:inherit/super)
         (quote-syntax untyped:inherit/inner)
         (quote-syntax untyped:rename-inner)))

 ;; similar to the same stop forms that the class macro uses
 (define stop-forms
   (append (kernel-form-identifier-list)
           unsupported-forms
           (list
            (quote-syntax :)
            (quote-syntax untyped:init)
            (quote-syntax untyped:field)
            (quote-syntax untyped:init-field)
            (quote-syntax untyped:init-rest)
            (quote-syntax untyped:inherit-field)
            (quote-syntax untyped:private)
            (quote-syntax untyped:public)
            (quote-syntax untyped:override)
            (quote-syntax untyped:pubment)
            (quote-syntax untyped:augment)
            (quote-syntax untyped:inherit)
            (quote-syntax untyped:super)
            (quote-syntax untyped:inner)
            (quote-syntax untyped:super-new)
            (quote-syntax untyped:super-instantiate)
            (quote-syntax untyped:super-make-object)
            (quote-syntax untyped:inspect))))

 (define-splicing-syntax-class maybe-type-parameter
   (pattern (~seq (~or #:∀ #:forall) (type-variable:id ...))
            #:attr type-variables #'(type-variable ...))
   (pattern (~seq)
            #:attr type-variables #'())))

;; export some syntax-time definitions for testing purposes
(module+ internal
  (provide (for-syntax get-optional-inits)))

(define-syntax (class stx)
  (syntax-parse stx
    [(_ super forall:maybe-type-parameter e ...)
     (define/with-syntax class-info (generate-temporary))
     (ignore
      (tr:class
       (quasisyntax/loc stx
         (untyped:class #,(tr:class:super-property #'super #t)
                        (define-syntax class-info (tr-class-info null null))
                        (add-annotations class-info e) ...
                        (make-locals-table class-info)
                        (make-class-name-table
                         class-info
                         #,(attribute forall.type-variables))))))]))

;; Add syntax properties and other metadata to class form so that the typechecker
;; can understand the expansion later
(define-syntax (add-annotations stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ class-info:id class-exp)
     (define expanded (local-expand #'class-exp (syntax-local-context) stop-forms))
     (syntax-parse expanded
       #:literal-sets (kernel-literals)
       #:literals (: untyped:super-new untyped:super-make-object
                     untyped:super-instantiate)
       [(begin e ...)
        (quasisyntax/loc #'class-exp
          (begin (add-annotations class-info e) ...))]
       [cls:class-clause
        (define info (syntax-local-value #'class-info))
        (define clause-data (attribute cls.data))
        (match-define (struct clause (stx kind ids types)) clause-data)
        ;; to avoid macro taint issues
        (define prop-val (tr:class:clause-ids-property #'cls))
        (define clause-data*
          (cond [(and prop-val (init-clause? clause-data))
                 (init-clause stx kind prop-val types
                              (init-clause-optional? clause-data))]
                [prop-val
                 (clause stx kind prop-val types)]
                [else clause-data]))
        (set-tr-class-info-clauses!
         info
         (cons clause-data* (tr-class-info-clauses info)))
        (check-unsupported-feature kind #'class-exp)
        #'class-exp]
       ;; if it's a method definition for a declared method, then
       ;; mark it as something to type-check
       [(define-values (id) body)
        #:when (method-procedure? #'body)
        (tr:class:method-property #'class-exp (syntax-e #'id))]
       ;; private field definition
       [(define-values (id ...) . rst)
        (define info (syntax-local-value #'class-info))
        (set-tr-class-info-private-fields!
         info
         (append (syntax->list #'(id ...))
                 (tr-class-info-private-fields info)))
        ;; set this property so that the initialization expression for
        ;; this field is counted as a top-level class expression
        (tr:class:top-level-property #'class-exp #t)]
       ;; special : annotation for augment interface
       [(: name:id type:expr #:augment augment-type:expr)
        (quasisyntax/loc #'class-exp
          (begin
            #,(tr:class:top-level-property
               (tr:class:type-annotation-property
                #'(quote-syntax (:-augment name augment-type)) #t) #t)
            #,(tr:class:top-level-property
               (tr:class:type-annotation-property
                (syntax/loc #'class-exp (: name type)) #t) #t)))]
       ;; Just process this to add the property
       [(: name:id . rst)
        (tr:class:top-level-property
         (tr:class:type-annotation-property
          #'class-exp
          #t)
         #t)]
       ;; Identify super-new for the benefit of the type checker
       [(~or (untyped:super-new [init-id init-expr] ...)
             (untyped:super-make-object init-expr ...)
             (untyped:super-instantiate (init-expr ...) [name expr] ...))
        (tr:class:top-level-property
         (tr:class:super-new-property #'class-exp #t)
         #t)]
       [_ (tr:class:top-level-property #'class-exp #t)])]))

;; Construct a table in the expansion that lets TR know about the generated
;; identifiers that are used for methods, fields, and such
(define-syntax (make-locals-table stx)
  (syntax-parse stx
    [(_ class-info:id)
     (match-define (tr-class-info clauses private-fields)
                   (syntax-local-value #'class-info))
     (do-make-locals-table (reverse clauses) private-fields)]))

;; Construct a table in the expansion that just records the names of clauses
;; in the class for convenience in later processing
(define-syntax (make-class-name-table stx)
  (syntax-parse stx
    [(_ class-info:id (type-variable:id ...))
     (match-define (tr-class-info clauses private-fields)
                   (syntax-local-value #'class-info))
     (do-make-class-name-table #'(type-variable ...)
                               (reverse clauses)
                               private-fields)]))

(begin-for-syntax
  ;; Determine if the given syntax object matches the "method-procedure"
  ;; non-terminal documented for the class macro
  (define (method-procedure? stx)
    (define stop-list (list #'lambda #'case-lambda
                            #'#%plain-lambda #'let-values
                            #'letrec-values))
    (define expanded (local-expand stx (syntax-local-context) stop-list))
    (define stx*
      (syntax-parse expanded
        #:literal-sets (kernel-literals)
        ;; an extra #%expression is inserted by the local expansion but
        ;; won't appear in the actual expansion, so ignore it
        [(#%expression e) #'e]
        [_ expanded]))
    (syntax-parse stx*
      #:literal-sets (kernel-literals)
      #:literals (lambda λ)
      [((~or lambda λ) formals e ...) #t]
      [(case-lambda (formals e ...) ...) #t]
      [(#%plain-lambda formals e ...) #t]
      [((~or let-values letrec-values) ([(x) m] ...) y:id)
       (andmap method-procedure? (syntax->list #'(m ...)))]
      [((~or let-values letrec-values) ([(x) m] ...) m1)
       (and (andmap method-procedure? (syntax->list #'(m ...)))
            (method-procedure? #'m1))]
      [_ #f]))

  ;; clauses->names : (-> Clause Boolean) Listof<Clause> -> Listof<Id>
  ;; filter clauses by some property and spit out the names in those clauses
  (define (clauses->names prop clauses [keep-pair? #f])
    (apply append
           (for/list ([clause (in-list clauses)]
                      #:when (prop clause))
             (define ids (clause-ids clause))
             (for/list ([id (in-list ids)])
               (if (and (not keep-pair?) (stx-pair? id))
                   (stx-car id)
                   id)))))

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
    (clauses->names init-clause? clauses))

  ;; check-unsupported-feature : Identifier Syntax -> Void
  ;; Check if the given identifier corresponds to an unsupported class form
  ;; and emit an error using the given syntax object if it does
  (define (check-unsupported-feature id stx)
    (when (member id unsupported-forms free-identifier=?)
      (tc-error/stx id "unsupported class clause: ~a" stx)))

  ;; do-make-class-name-table : Listof<Id> Listof<Clause> Listof<Id> -> Stx
  ;; construct syntax used by the class type-checker as a reliable source
  ;; for the member names that are in a given class, plus any type
  ;; variables that are bound
  (define (do-make-class-name-table foralls
                                    clauses
                                    private-fields)
    (define (get-names kind)
      (clauses->names (λ (clause)
                        (free-identifier=? (clause-kind clause) kind))
                      clauses #t))
    (tr:class:name-table-property
     (internal
      #`(class-internal
         (#:forall        #,@foralls)
         (#:all-inits     #,@(get-all-init-names clauses))
         (#:init          #,@(get-names #'untyped:init))
         (#:init-field    #,@(get-names #'untyped:init-field))
         (#:init-rest     #,@(get-names #'untyped:init-rest))
         (#:optional-init #,@(get-optional-inits clauses))
         (#:field         #,@(get-names #'untyped:field))
         (#:public        #,@(get-names #'untyped:public))
         (#:override      #,@(get-names #'untyped:override))
         (#:private       #,@(get-names #'untyped:private))
         (#:private-field #,@private-fields)
         (#:inherit       #,@(get-names #'untyped:inherit))
         (#:inherit-field #,@(get-names #'untyped:inherit-field))
         (#:augment       #,@(get-names #'untyped:augment))
         (#:pubment       #,@(get-names #'untyped:pubment))))
     #t))

  ;; This is a neat/horrible trick
  ;;
  ;; In order to detect the mappings that class-internal.rkt has
  ;; created for class-local field and method access, we construct
  ;; a in-syntax table mapping original names to the accessors.
  ;; The identifiers inside the lambdas below will expand via
  ;; set!-transformers to the appropriate accessors, which lets
  ;; us figure out the accessor identifiers.
  (define (do-make-locals-table clauses private-field-names)
    (define (get-names kind)
      (clauses->names (λ (clause)
                        (free-identifier=? (clause-kind clause) kind))
                      clauses))
    (define public-names        (get-names #'untyped:public))
    (define override-names      (get-names #'untyped:override))
    (define private-names       (get-names #'untyped:private))
    (define field-names         (append (get-names #'untyped:field)
                                        (get-names #'untyped:init-field)))
    (define init-names          (get-names #'untyped:init))
    (define init-rest-name      (get-names #'untyped:init-rest))
    (define inherit-names       (get-names #'untyped:inherit))
    (define inherit-field-names (get-names #'untyped:inherit-field))
    (define augment-names       (append (get-names #'untyped:pubment)
                                        (get-names #'untyped:augment)))
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
                     (values #,@(map (λ (stx) #`(λ () (#,stx) (untyped:super #,stx)))
                                     override-names))]
                    [(#,@augment-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx) (untyped:inner #f #,stx)))
                                     augment-names))])
         (void))
     #t)))

