#lang racket/base

;; This module provides TR primitives for classes and objects

(require racket/class
         (for-syntax
          racket/base
          racket/class
          racket/dict
          racket/list
          racket/pretty ;; get rid of this later
          racket/syntax
          racket/private/classidmap ;; this is bad
          syntax/flatten-begin
          syntax/id-table
          syntax/kerncase
          syntax/parse
          syntax/stx
          unstable/list
          (for-template "../typecheck/internal-forms.rkt")
          "../utils/tc-utils.rkt"
          "../types/utils.rkt"))

(provide ;; Typed class macro that coordinates with TR
         class:
         ;; for use in ~literal clauses
         class:-internal
         optional-init)

;; give it a binding, but it shouldn't be used directly
(define-syntax (class:-internal stx)
  (raise-syntax-error "should only be used internally"))

(define-syntax (optional-init stx)
  (raise-syntax-error "should only be used internally"))

(begin-for-syntax
 (module+ test (require rackunit))

 ;; basically the same stop forms that class-internal uses
 (define stop-forms
   (append (kernel-form-identifier-list)
           (list
            (quote-syntax #%app)
            (quote-syntax lambda)
            (quote-syntax init)
            (quote-syntax init-rest)
            (quote-syntax field)
            (quote-syntax init-field)
            (quote-syntax inherit-field)
            (quote-syntax private)
            (quote-syntax public)
            (quote-syntax override)
            (quote-syntax augride)
            (quote-syntax public-final)
            (quote-syntax override-final)
            (quote-syntax augment-final)
            (quote-syntax pubment)
            (quote-syntax overment)
            (quote-syntax augment)
            (quote-syntax rename-super)
            (quote-syntax inherit)
            (quote-syntax inherit/super)
            (quote-syntax inherit/inner)
            (quote-syntax rename-inner)
            (quote-syntax abstract)
            (quote-syntax super)
            (quote-syntax inner)
            (quote-syntax this)
            (quote-syntax this%)
            (quote-syntax super-instantiate)
            (quote-syntax super-make-object)
            (quote-syntax super-new)
            (quote-syntax inspect)))))

(begin-for-syntax
 ;; A Clause is a (clause Syntax Id Listof<Syntax>)
 ;;
 ;; interp. a class clause such as init or field.
 (struct clause (stx type ids))

 ;; An InitClause is a (init-clause Syntax Id Listof<Syntax> Boolean)
 ;;
 ;; interp. an init class clause
 (struct init-clause clause (optional?))

 ;; A NonClause is a (non-clause Syntax)
 ;;
 ;; interp. a top-level class expression that is not one of the special
 ;;         class clauses such as init or field.
 (struct non-clause (stx))
 
 (define-syntax-class init-decl
   (pattern id:id
            #:attr optional? #f
            #:with ids #'(id id))
   (pattern (ren:renamed)
            #:attr optional? #f
            #:with ids #'ren.ids)
   (pattern (mren:maybe-renamed default-value:expr)
            #:attr optional? #t
            #:with ids #'mren.ids))

 (define-syntax-class field-decl
   (pattern (mren:maybe-renamed default-value:expr)
            #:with ids #'mren.ids))

 (define-syntax-class renamed
   (pattern (internal-id:id external-id:id)
            #:with ids #'(internal-id external-id)))

 (define-syntax-class maybe-renamed
   (pattern id:id
            #:with ids #'(id id))
   (pattern ren:renamed
            #:with ids #'ren.ids))
 
 (define-syntax-class class-clause
   (pattern (~and ((~and clause-name (~or (~literal init)
                                          (~literal init-field)))
                   names:init-decl ...)
                  form)
            ;; in the future, use a data structure and
            ;; make this an attribute instead to represent
            ;; internal and external names
            #:attr data
            (init-clause #'form #'clause-name
                         (stx->list #'(names.ids ...))
                         (attribute names.optional?)))
   (pattern (~and ((~literal field) names:field-decl ...) form)
            #:attr data (clause #'form #'field
                                (stx->list #'(names.ids ...))))
   (pattern (~and ((~and clause-name (~or (~literal inherit-field)
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
                                          (~literal inherit/inner)))
                   names:maybe-renamed ...)
                  form)
            #:attr data
            (clause #'form #'clause-name
                    (stx->list #'(names.ids ...))))
   (pattern (~and ((~and clause-name (~or (~literal private)
                                          (~literal abstract)))
                   names:id ...)
                  form)
            #:attr data
            (clause #'form #'clause-name
                    (stx->list #'(names ...)))))

 (define-syntax-class class-clause-or-other
   (pattern e:class-clause #:attr data (attribute e.data))
   (pattern e:expr #:attr data (non-clause #'e)))

 ;; Listof<Clause> -> Hash<Identifier, Names>
 ;; Extract names from init, public, etc. clauses
 (define (extract-names clauses)
   (for/fold ([clauses (make-immutable-free-id-table)])
             ([clause clauses])
     (if (dict-has-key? clauses (clause-type clause))
         (dict-update clauses (clause-type clause)
                      (λ (old-names)
                        (append old-names (clause-ids clause))))
         (dict-set clauses
                   (clause-type clause)
                   (clause-ids clause)))))

 ;; Get rid of class top-level `begin` and local expand
 (define ((eliminate-begin expander) stx)
   (syntax-parse stx
     #:literals (begin)
     [(begin e ...)
      (stx-map (compose (eliminate-begin expander) expander)
               (flatten-begin stx))]
     [_ stx]))
 
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

(define-syntax (class: stx)
  (syntax-parse stx
    [(_ super e ...)
     (define class-context (generate-class-expand-context))
     ;; do a local expansion for class:
     (define (class-expand stx)
       (local-expand stx class-context stop-forms))
     ;; FIXME: potentially needs to expand super clause?
     (define expanded-stx (stx-map class-expand #'(e ...)))
     (define flattened-stx
       (flatten (map (eliminate-begin class-expand) expanded-stx)))
     (syntax-parse flattened-stx
       [(class-elems:class-clause-or-other ...)
        (define-values (clauses others)
          (filter-multiple (attribute class-elems.data)
                           clause?
                           non-clause?))
        (define name-dict (extract-names clauses))
        (define-values (annotated-methods other-top-level)
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
                 #`(class:-internal
                    (init #,@(dict-ref name-dict #'init '()))
                    (init-field #,@(dict-ref name-dict #'init-field '()))
                    (optional-init #,@optional-inits)
                    (field #,@(dict-ref name-dict #'field '()))
                    (public #,@(dict-ref name-dict #'public '()))
                    (override #,@(dict-ref name-dict #'override '()))
                    (private #,@(dict-ref name-dict #'private '()))
                    (inherit #,@(dict-ref name-dict #'inherit '()))))
              (class #,annotated-super
                #,@(map clause-stx clauses)
                #,@(map non-clause-stx annotated-methods)
                #,(syntax-property
                   #`(begin #,@(map non-clause-stx other-top-level))
                   'tr:class:top-level #t)
                #,(make-locals-table name-dict)))
          'tr:class #t)
         'typechecker:ignore #t)])]))

(begin-for-syntax
  ;; process-class-contents : Listof<Syntax> Dict<Id, Listof<Id>>
  ;;                          -> Listof<Syntax> Listof<Syntax>
  ;; Process methods and other top-level expressions and definitions
  ;; that aren't class clauses like `init` or `public`
  (define (process-class-contents contents name-dict)
    (for/fold ([methods '()]
               [rest-top '()])
              ([content contents])
      (define stx (non-clause-stx content))
      (syntax-parse stx
        #:literals (define-values super-new)
        ;; if it's a method definition for a declared method, then
        ;; mark it as something to type-check
        ;; FIXME: this needs to track augments, etc.
        [(define-values (id) . rst)
         #:when (memf (λ (n) (free-identifier=? #'id n))
                      (append (stx-map stx-car (dict-ref name-dict #'public '()))
                              (stx-map stx-car (dict-ref name-dict #'override '()))
                              (dict-ref name-dict #'private '())))
         (values (cons (non-clause (syntax-property stx
                                                    'tr:class:method
                                                    (syntax-e #'id)))
                       methods)
                 rest-top)]
        ;; Identify super-new for the benefit of the type checker
        [(super-new [init-id init-expr] ...)
         (define new-non-clause
           (non-clause (syntax-property stx 'tr:class:super-new #t)))
         (values methods (append rest-top (list new-non-clause)))]
        [_ (values methods (append rest-top (list content)))])))

  ;; get-optional-inits : Listof<Clause> -> Listof<Id>
  ;; Get a list of the internal names of mandatory inits
  (define (get-optional-inits clauses)
    (flatten
     (for/list ([clause clauses]
                #:when (init-clause? clause))
       (for/list ([id-pair (stx->list (clause-ids clause))]
                  [optional? (init-clause-optional? clause)]
                  #:when optional?)
         (stx-car id-pair)))))

  (module+ test
    (check-equal?/id
     (get-optional-inits
      (list (init-clause #'(init [x 0]) #'init #'([x x]) (list #t))
            (init-clause #'(init [(a b)]) #'init #'([a b]) (list #f))))
     (list #'x)))

  ;; This is a neat/horrible trick
  ;;
  ;; In order to detect the mappings that class-internal.rkt has
  ;; created for class-local field and method access, we construct
  ;; a in-syntax table mapping original names to the accessors.
  ;; The identifiers inside the lambdas below will expand via
  ;; set!-transformers to the appropriate accessors, which lets
  ;; us figure out the accessor identifiers.
  (define (make-locals-table name-dict)
    (define public-names (stx-map stx-car (dict-ref name-dict #'public '())))
    (define override-names
      (stx-map stx-car (dict-ref name-dict #'override '())))
    (define private-names (dict-ref name-dict #'private '()))
    (define field-names
      (append (stx-map stx-car (dict-ref name-dict #'field '()))
              (stx-map stx-car (dict-ref name-dict #'init-field '()))))
    (define init-names
      (stx-map stx-car (dict-ref name-dict #'init '())))
    (define inherit-names
      (stx-map stx-car (dict-ref name-dict #'inherit '())))
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
                    [(#,@init-names)
                     (values #,@(map (λ (stx) #`(λ () #,stx))
                                     init-names))]
                    [(#,@inherit-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx)))
                                     inherit-names))]
                    [(#,@override-names)
                     (values #,@(map (λ (stx) #`(λ () (#,stx) (super #,stx)))
                                     override-names))])
         (void))
     'tr:class:local-table #t)))

