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
         (private syntax-properties type-annotation)
         (base-env class-prims)
         (env lexical-env)
         (types utils abbrev union subtype resolve)
         (utils tc-utils)
         (rep type-rep)
         (for-template racket/base
                       (prefix-in c: racket/class)
                       (base-env class-prims)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-class^)

;; Syntax TCResults -> Void
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
    [(tc-result1: (and self-class-type (Class: _ inits fields methods)))
     (do-check form #t self-class-type inits fields methods)]
    [#f (do-check form #f #f null null null)]))

;; Syntax Boolean Option<Type> Inits Fields Methods -> Type
;; Do the actual type-checking
(define (do-check form expected? self-class-type inits fields methods)
  (syntax-parse form
    #:literals (let-values #%plain-lambda quote-syntax begin
                #%plain-app values class:-internal letrec-syntaxes+values
                c:init c:init-field c:field c:public c:override)
    ;; Inspect the expansion of the class macro for the pieces that
    ;; we need to type-check like superclass, methods, top-level
    ;; expressions and so on
    [(let-values ()
       (letrec-syntaxes+values ()
                               ((()
                                 ;; residual class: data
                                 ;; FIXME: put in syntax class
                                 (begin
                                   (quote-syntax
                                    (class:-internal
                                     (c:init internal-init-names ...)
                                     (c:init-field internal-init-field-names ...)
                                     (c:field internal-field-names ...)
                                     (c:public internal-public-names ...)
                                     (c:override internal-override-names ...)))
                                   (#%plain-app values))))
                               (let-values (((superclass) superclass-expr)
                                            ((interfaces) interface-expr))
                                 (?#%app compose-class
                                         internal ...
                                         (#%plain-lambda (local-accessor local-mutator ??? ...)
                                                         (let-values ([(field-name) accessor-or-mutator]
                                                                      ...)
                                                           body))
                                         ????))))
     ;; Type for self in method calls
     (define self-type (make-Instance self-class-type))
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
     (define this%-init-names
       (list->set
        (append (syntax->datum #'(internal-init-names ...))
                (syntax->datum #'(internal-init-field-names ...)))))
     (define this%-field-names
       (list->set
        (append (syntax->datum #'(internal-field-names ...))
                (syntax->datum #'(internal-init-field-names ...)))))
     (define this%-public-names
       (list->set (syntax->datum #'(internal-public-names ...))))
     (define this%-override-names
       (list->set (syntax->datum #'(internal-override-names ...))))
     (define this%-method-names
       (set-union this%-public-names this%-override-names))
     ;; Use the internal class: information to check whether clauses
     ;; exist or are absent appropriately
     (when expected?
      (define exp-init-names (list->set (dict-keys inits)))
      (define exp-field-names (list->set (dict-keys fields)))
      (define exp-method-names (list->set (dict-keys methods)))
      (check-exists (set-union this%-init-names super-init-names)
                    exp-init-names
                    "initialization argument")
      (check-exists (set-union this%-public-names super-method-names)
                    exp-method-names
                    "public method")
      (check-exists (set-union this%-field-names super-field-names)
                    exp-field-names
                    "public field"))
     (check-exists super-method-names this%-override-names
                   "override method")
     (check-absent super-field-names this%-field-names "public field")
     (check-absent super-method-names this%-public-names "public method")
     ;; FIXME: the control flow for the failure of these checks is
     ;;        still up in the air
     #|
     (check-no-extra (set-union this%-field-names super-field-names)
                     exp-field-names)
     (check-no-extra (set-union this%-public-names super-method-names)
                     exp-method-names)
     |#
     ;; trawl the body for the local name table
     (define locals (trawl-for-property #'body 'tr:class:local-table))
     (define local-table
      (syntax-parse (car locals)
        #:literals (let-values #%plain-app #%plain-lambda)
        [(let-values ([(method ...)
                       (#%plain-app
                        values
                        (#%plain-lambda ()
                          (#%plain-app (#%plain-app local-method self1) self2))
                        ...)])
            (#%plain-app void))
          (map cons
              (syntax->datum #'(method ...))
              (syntax->list #'(local-method ...)))]))
     ;; find the `super-new` call (or error if missing)
     (define super-new-stx (trawl-for-property #'body 'tr:class:super-new))
     (check-super-new super-new-stx super-inits)
     ;; trawl the body and find methods and type-check them
     (define meths (trawl-for-property #'body 'tr:class:method))
     (with-lexical-env/extend (map (λ (m) (dict-ref local-table m))
                                   (set->list this%-method-names))
                              ;; FIXME: the types we put here are fine in the expected
                              ;;        case, but not if the class doesn't have an annotation.
                              ;;        Then we need to hunt down annotations in a first pass.
                              ;;        (should probably do this in expected case anyway)
                              ;; FIXME: this doesn't work because the names of local methods
                              ;;        are obscured and need to be reconstructed somehow
                              (map (λ (m) (->* (list (make-Univ))
                                               (fixup-method-type (car (dict-ref methods m))
                                                                  self-type)))
                                   (set->list this%-method-names))
      (for ([meth meths])
        (define method-name (syntax-property meth 'tr:class:method))
        (define method-type
          (fixup-method-type
           (car (dict-ref methods method-name))
           self-type))
        (define expected (ret method-type))
        (define annotated (annotate-method meth self-type))
        (tc-expr/check annotated expected)))
     ;; trawl the body for top-level expressions too
     (define top-level-exprs (trawl-for-property #'body 'tr:class:top-level))
     (void)]))

;; check-super-new : Listof<Syntax> Inits -> Void
;; Check if the super-new call is well-typed
(define (check-super-new super-new-stx super-inits)
  (cond [(null? super-new-stx)
         (tc-error/expr
          "typed classes must call super-new at the class top-level")]
        [else
         (syntax-parse (car super-new-stx)
           #:literals (#%plain-app list cons quote)
           [(#%plain-app super-go _ _ _ _ _
                         (#%plain-app
                          list
                          (#%plain-app cons (quote init-id) arg:expr)
                          ...))
            (for ([init-id (syntax->datum #'(init-id ...))]
                  [init-arg (syntax->list #'(arg ...))])
              (define maybe-expected (dict-ref super-inits init-id #f))
              (if maybe-expected
                  (tc-expr/check init-arg (ret (car maybe-expected)))
                  (tc-error/expr "init argument ~a not accepted by superclass"
                                 init-id)))])]))

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

;; annotate-method : Syntax Type -> Syntax
;; Adds a self type annotation for the first argument
(define (annotate-method stx self-type)
  (syntax-parse stx
    #:literals (let-values #%plain-lambda)
    [(let-values ([(meth-name:id)
                   (#%plain-lambda (self-param:id id:id ...)
                     body ...)])
       m)
     (define annotated-self-param
       (type-ascription-property #'self-param self-type))
     #`(let-values ([(meth-name)
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

