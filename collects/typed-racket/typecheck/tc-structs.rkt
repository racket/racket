#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep object-rep free-variance)
         (private parse-type)
         (types abbrev utils union resolve substitute type-table)
         (env global-env type-env-structs type-name-env tvar-env)
         (utils tc-utils)
         "def-binding.rkt"
         syntax/kerncase
         syntax/struct
         syntax/parse
         racket/function
         racket/match
         racket/list
         (only-in racket/contract
                  listof any/c or/c
                  [->* c->*]
                  [-> c->])
         (for-syntax
          syntax/parse
          racket/base))


(require (for-template racket/base
                       "internal-forms.rkt"))

(provide tc/struct tc/poly-struct names-of-struct d-s)

(define-syntax-class parent
  #:attributes (name par)
  (pattern (name:id par:id))
  (pattern name:id #:attr par #f))


;; TODO make this not return a list
(define (names-of-struct stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    #:literals (define-typed-struct-internal values)
    [(#%define-values () (begin (quote-syntax
                                  (~or
                                    (define-typed-struct-internal
                                      (~optional (ids:id ...))
                                      nm/par:parent . rest)
                                    (define-typed-struct/exec-internal
                                      nm/par:parent . rest)))
                                (#%plain-app values)))
     (list #'nm/par.name)]))


;; parse name field of struct, determining whether a parent struct was specified
;; syntax -> (values identifier Option[Name] Option[Struct] Symbol Type)
(define (parse-parent nm/par)
  (syntax-parse nm/par
    [v:parent
      (if (attribute v.par)
          (let* ([parent0 (parse-type #'v.par)]
                 [parent (if (Name? parent0)
                             ;; TODO ensure this is a struct
                             (resolve-name parent0)
                             (tc-error/stx #'v.par "parent type not a valid structure name: ~a"
                                           (syntax->datum #'v.par)))])
                (values #'v.name parent0 parent))
          (values #'v.name #f #f))]))

;; generate struct names given type name and field names
;; generate setters if setters? is true
;; all have syntax loc of name
;; identifier listof[identifier] boolean ->
;;   (values identifier identifier list[identifier] Option[list[identifier]])
(define (struct-names nm flds setters?)
  (define (split l)
    (let loop ([l l] [getters '()] [setters '()])
      (if (null? l)
          (values (reverse getters) (reverse setters))
          (loop (cddr l) (cons (car l) getters) (cons (cadr l) setters)))))
  (match (build-struct-names nm flds #f (not setters?) nm)
    [(list sty maker pred getters/setters ...)
     (if setters?
         (let-values ([(getters setters) (split getters/setters)])
           (values sty maker pred getters setters))
         (values sty maker pred getters/setters #f))]))

;; gets the fields of the parent type, if they exist
;; Option[Struct-Ty] -> Listof[Type]
(define (get-parent-flds p)
  (match p
    [(Struct: _ _ flds _ _ _) flds]
    [#f null]))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[fld] listof[Type] boolean ->
;;  (values Type listof[Type] listof[Type])
(define/cond-contract (mk/register-sty nm flds parent parent-fields types
                                       #:wrapper [wrapper values]
                                       #:type-wrapper [type-wrapper values]
                                       #:pred-wrapper [pred-wrapper values]
                                       #:mutable [setters? #f]
                                       #:proc-ty [proc-ty #f]
                                       #:maker [maker* #f]
                                       #:poly? [poly? null]
                                       #:type-only [type-only #f])
     (c->* (identifier? (listof identifier?) (or/c Type/c #f) (listof fld?) (listof Type/c))
           (#:wrapper procedure?
            #:type-wrapper procedure?
            #:pred-wrapper procedure?
            #:mutable boolean?
            #:proc-ty (or/c #f Type/c)
            #:maker (or/c #f identifier?)
            #:poly? (listof symbol?)
            #:type-only boolean?)
           any/c)
  ;; create the approriate names that define-struct will bind
  (define-values (_1 maker** pred getters _2) (struct-names nm flds setters?))
  (define maker (or maker* maker**))
  (let* ([fld-names flds]
         [this-flds (for/list ([t (in-list types)]
                               [g (in-list getters)])
                       (make-fld t g setters?))]
         [flds (append parent-fields this-flds)]
         [sty (make-Struct nm parent flds proc-ty (not (null? poly?)) pred)]
         [external-fld-types/no-parent types]
         [external-fld-types (map fld-t flds)])

    (register-type-name nm (wrapper sty))
    (unless type-only
      (register-struct-types nm sty fld-names external-fld-types
                             external-fld-types/no-parent setters?
                             #:wrapper wrapper
                             #:type-wrapper type-wrapper
                             #:pred-wrapper pred-wrapper
                             #:maker maker
                             #:poly? poly?))))




;; generate names, and register the approriate types give field types and structure type
;; optionally wrap things
;; identifier Type Listof[identifier] Listof[Type] Listof[Type]
;;   #:wrapper (Type -> Type) #:maker identifier -> Void
(define/cond-contract (register-struct-types nm sty flds external-fld-types
                                             external-fld-types/no-parent setters?
                                             #:wrapper wrapper
                                             #:type-wrapper type-wrapper
                                             #:pred-wrapper pred-wrapper
                                             #:maker maker
                                             #:poly? poly?)
     (c-> identifier? Struct? (listof identifier?) (listof Type/c) (listof Type/c) boolean?
          #:wrapper procedure?
          #:type-wrapper procedure?
          #:pred-wrapper procedure?
          #:maker identifier?
          #:poly? (listof symbol?)
          void?)
  ;; create the approriate names that define-struct will bind
  (define-values (struct-type-id _2 pred getters setters) (struct-names nm flds setters?))
  ;; the type name that is used in all the types
  (define name (type-wrapper (make-Name nm)))
  ;; is this structure covariant in *all* arguments?
  (define covariant? (if (and setters? (list? poly?))
                         #f
                         (if poly?
                             (for*/and ([var (in-list poly?)]
                                        [t (in-list external-fld-types)])
                               (let ([variance (hash-ref (free-vars* t) var Constant)])
                                 (or (eq? variance Constant)
                                     (eq? variance Covariant))))
                             #t)))
  (define parent-count (- (length external-fld-types) (length external-fld-types/no-parent)))
  ;; the list of names w/ types
  (register-type struct-type-id (make-StructType sty))
  (register-type maker (wrapper (->* external-fld-types name)))
  (register-type pred
                 (make-pred-ty (if (not covariant?)
                                   (make-StructTop sty)
                                   (pred-wrapper name))))
  (for ([g (in-list getters)]
        [t (in-list external-fld-types/no-parent)]
        [i (in-naturals parent-count)])
    (let* ([path (make-StructPE name i)]
           [func (if setters?
                     (->* (list name) t)
                     (->acc (list name) t (list path)))])
      (add-struct-fn! g path #f)
      (register-type g (wrapper func))))
  (when setters?
    (for ([g (in-list setters)]
          [t (in-list external-fld-types/no-parent)]
          [i (in-naturals parent-count)])
      (add-struct-fn! g (make-StructPE name i) #t)
      (register-type g (wrapper (->* (list name t) -Void))))))

;; check and register types for a polymorphic define struct
;; tc/poly-struct : Listof[identifier] (U identifier (list identifier identifier))
;;                  Listof[identifier] Listof[syntax]
;;                  -> void
(define (tc/poly-struct vars nm/par flds tys #:maker [maker #f] #:mutable [mutable #f])
  ;; parent field types can't actually be determined here
  (define-values (nm parent-name parent) (parse-parent nm/par))
  ;; create type variables for the new type parameters
  (define tvars (map syntax-e vars))
  (define new-tvars (map make-F tvars))
  ;; parse the types
  (define types
    ;; add the type parameters of this structure to the tvar env
    (extend-tvars tvars
      (parameterize ([current-poly-struct `#s(poly ,nm ,new-tvars)])
        ;; parse the field types
        (map parse-type tys))))
  ;; instantiate the parent if necessary, with new-tvars
  (define concrete-parent
    (if (Poly? parent)
        (if (> (Poly-n parent) (length new-tvars))
            (tc-error "Could not instantiate parent struct type. Required ~a type variables, recieved ~a."
              (Poly-n parent)
              (length new-tvars))
            (instantiate-poly parent (take new-tvars (Poly-n parent))))
        parent))
  ;; get the fields of the parent, if it exists
  (define parent-field-types (get-parent-flds concrete-parent))
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  ;; then register them
  (mk/register-sty nm flds concrete-parent parent-field-types types
                   #:maker maker
                   #:mutable mutable
                   ;; wrap everything in the approriate forall
                   #:wrapper (λ (t) (make-Poly tvars t))
                   #:type-wrapper (λ (t) (make-App t new-tvars #f))
                   #:pred-wrapper (λ (t) (subst-all (make-simple-substitution
                                                      tvars (map (const Univ) tvars)) t))
                   #:poly? tvars))


;; typecheck a non-polymophic struct and register the approriate types
;; tc/struct : (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
(define/cond-contract (tc/struct nm/par flds tys [proc-ty #f]
                                 #:maker [maker #f]
                                 #:mutable [mutable #f]
                                 #:type-only [type-only #f])
     (c->* (syntax? (listof identifier?) (listof syntax?))
           ((or/c #f syntax?)
            #:maker any/c
            #:mutable boolean?
            #:type-only boolean?)
           any/c)
  ;; get the parent info and create some types and type variables
  (define-values (nm parent-name parent) (parse-parent nm/par))
  ;; parse the field types, and determine if the type is recursive
  (define types (map parse-type tys))
  (define proc-ty-parsed (and proc-ty (parse-type proc-ty)))

  (when (Poly? parent)
    (tc-error "Could not instantiate parent struct type. Required ~a type variables, recieved none."
            (Poly-n parent)))

  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  (mk/register-sty nm flds parent (get-parent-flds parent) types
                   ;; procedure
                   #:proc-ty proc-ty-parsed
                   #:maker maker
                   #:mutable mutable
                   #:type-only type-only))

;; register a struct type
;; convenience function for built-in structs
;; tc/builtin-struct : identifier Maybe[identifier] Listof[identifier]
;;                     Listof[Type] Maybe[identifier] Listof[Type]
;;                     -> void
;; FIXME - figure out how to make this lots lazier
(define/cond-contract (tc/builtin-struct nm parent flds tys kernel-maker)
     (c-> identifier? (or/c #f identifier?) (listof identifier?)
          (listof Type/c) (or/c #f identifier?)
          any/c)
  (define parent-name (and parent (make-Name parent)))
  (define parent-type (and parent (lookup-type-name parent)))
  (define parent-flds (get-parent-flds (and parent-name (resolve-name parent-name))))

  (define parent-tys (map fld-t parent-flds))
  (define defs (mk/register-sty nm flds parent-type parent-flds tys #:mutable #t))
  (when kernel-maker    
    (register-type kernel-maker (λ () (->* (append parent-tys tys) (lookup-type-name nm))))))


;; syntax for tc/builtin-struct
(define-syntax (d-s stx)
  (define-splicing-syntax-class options
   (pattern (~optional (~seq #:kernel-maker maker:id))
            #:attr kernel-maker (if (attribute maker) #'(quote-syntax maker) #'#f)))

  (syntax-parse stx
    [(_ (nm:id par:id) ([fld:id (~datum :) ty] ...) (par-ty ...) opts:options)
     #'(tc/builtin-struct #'nm #'par
                          (list #'fld ...)
                          (list ty ...)
                          opts.kernel-maker)]
    [(_ nm:id ([fld:id (~datum :) ty] ...) opts:options)
     #'(tc/builtin-struct #'nm #f
                          (list #'fld ...)
                          (list ty ...)
                          opts.kernel-maker)]))

