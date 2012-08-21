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

(provide tc/struct names-of-struct d-s)

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
;; syntax -> (values identifier Option[Name] Option[Struct])
(define/cond-contract (parse-parent nm/par)
  (c-> syntax? (values identifier? (or/c Name? #f) (or/c Mu? Poly? Struct? #f)))
  (syntax-parse nm/par
    [v:parent
      (if (attribute v.par)
          (let* ([parent0 (parse-type #'v.par)]
                 ;; TODO ensure this is a struct
                 [parent (let loop ((parent parent0))
                               (cond
                                 ((Name? parent) (loop (resolve-name parent)))
                                 ((or (Poly? parent) (Mu? parent) (Struct? parent))
                                  parent)
                                 (else 
                                  (tc-error/stx #'v.par "parent type not a valid structure name: ~a"
                                                (syntax->datum #'v.par)))))])
                (values #'v.name parent0 parent))
          (values #'v.name #f #f))]))


(struct struct-names (type-name struct-type constructor predicate getters setters))
;; generate struct names given type name, field names
;; and optional constructor name
;; all have syntax loc of name
;; identifier listof[identifier] Option[identifier] ->
;;   (values identifier identifier list[identifier] list[identifier])
(define (get-struct-names nm flds maker*)
  (define (split l)
    (let loop ([l l] [getters '()] [setters '()])
      (if (null? l)
          (values (reverse getters) (reverse setters))
          (loop (cddr l) (cons (car l) getters) (cons (cadr l) setters)))))
  (match (build-struct-names nm flds #f #f nm)
    [(list sty maker pred getters/setters ...)
     (let-values ([(getters setters) (split getters/setters)])
       (struct-names nm sty (or maker* maker) pred getters setters))]))


;; gets the fields of the parent type, if they exist
;; Option[Struct-Ty] -> Listof[Type]
(define (get-parent-flds p)
  (match p
    [(Struct: _ _ flds _ _ _) flds]
    [#f null]))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[fld] listof[Type] boolean ->
;;  (values Type listof[Type] listof[Type])
(define/cond-contract (mk/register-sty names parent types
                                       #:mutable [mutable #f]
                                       #:proc-ty [proc-ty #f]
                                       #:poly? [poly? null])
     (c->* (struct-names?  (or/c Struct? #f) (listof Type/c))
           (#:mutable boolean?
            #:proc-ty (or/c #f Type/c)
            #:poly? (listof symbol?))
           any/c)

  (define sty 
    (mk/inner-struct-type names parent types
                        #:mutable mutable
                        #:proc-ty proc-ty
                        #:poly? poly?))

  (register-type-name (struct-names-type-name names) (make-Poly poly? sty)))

(define (mk/inner-struct-type names parent types
                                       #:mutable mutable
                                       #:proc-ty proc-ty
                                       #:poly? poly?)


  (let* ([this-flds (for/list ([t (in-list types)]
                               [g (in-list (struct-names-getters names))])
                       (make-fld t g mutable))]
         [flds (append (get-parent-flds parent) this-flds)]
         [sty (make-Struct (struct-names-type-name names)
                           parent flds proc-ty (not (null? poly?))
                           (struct-names-predicate names)
                           values
                           (struct-names-constructor names))])
    sty))

(define/cond-contract (mk/register-struct-bindings
                        names parent types
                                       #:mutable [mutable #f]
                                       #:proc-ty [proc-ty #f]
                                       #:poly? [poly? null])
     (c->* (struct-names? (or/c Struct? #f)  (listof Type/c))
           (#:mutable boolean?
            #:proc-ty (or/c #f Type/c)
            #:poly? (listof symbol?))
           any/c)

  (define sty 
    (mk/inner-struct-type names parent types
                        #:mutable mutable
                        #:proc-ty proc-ty
                        #:poly? poly?))

  (let* ([this-flds (for/list ([t (in-list types)]
                               [g (in-list (struct-names-getters names))])
                       (make-fld t g mutable))]
         [flds (append (get-parent-flds parent) this-flds)]
         [external-fld-types/no-parent types]
         [external-fld-types (map fld-t flds)])


    (register-struct-types sty names external-fld-types
                           external-fld-types/no-parent mutable
                           #:poly? poly?)))





;; generate names, and register the approriate types give field types and structure type
;; optionally wrap things
;; identifier Type Listof[identifier] Listof[Type] Listof[Type]
;;   #:maker identifier -> Void
(define/cond-contract (register-struct-types sty names external-fld-types
                                             external-fld-types/no-parent mutable
                                             #:poly? poly?)
     (c-> Struct? struct-names? (listof Type/c) (listof Type/c) boolean?
          #:poly? (listof symbol?)
          void?)

  ;; the type name that is used in all the types
  (define name
    (if (null? poly?)
        (make-Name (struct-names-type-name names))
        (make-App (make-Name (struct-names-type-name names)) (map make-F poly?) #f)))

  (define (poly-wrapper t) (make-Poly poly? t))
  ;; is this structure covariant in *all* arguments?
  (define covariant?
    (for*/and ([var (in-list poly?)]
               [t (in-list external-fld-types)])
      (let ([variance (hash-ref (free-vars* t) var Constant)])
        (or (eq? variance Constant)
            (and (not mutable) (eq? variance Covariant))))))
  (define parent-count (- (length external-fld-types) (length external-fld-types/no-parent)))
  ;; the list of names w/ types
  (register-type (struct-names-struct-type names) (make-StructType sty))
  (register-type (struct-names-constructor names) (poly-wrapper (->* external-fld-types name)))
  (register-type (struct-names-predicate names)
                 (make-pred-ty (if (not covariant?)
                                   (make-StructTop sty)
                                   (subst-all (make-simple-substitution
                                                  poly? (map (const Univ) poly?)) name))))

  (for ([g (in-list (struct-names-getters names))]
        [t (in-list external-fld-types/no-parent)]
        [i (in-naturals parent-count)])
    (let* ([path (make-StructPE name i)]
           [func (if mutable
                     (->* (list name) t)
                     (->acc (list name) t (list path)))])
      (add-struct-fn! g path #f)
      (register-type g (poly-wrapper func))))
  (when mutable
    (for ([g (in-list (struct-names-setters names))]
          [t (in-list external-fld-types/no-parent)]
          [i (in-naturals parent-count)])
      (add-struct-fn! g (make-StructPE name i) #t)
      (register-type g (poly-wrapper (->* (list name t) -Void))))))

;; check and register types for a define struct
;; tc/struct : Listof[identifier] (U identifier (list identifier identifier))
;;             Listof[identifier] Listof[syntax]
;;             -> void
(define (tc/struct vars nm/par fld-names tys
                   #:proc-ty [proc-ty #f]
                   #:maker [maker #f]
                   #:mutable [mutable #f]
                   #:type-only [type-only #f])
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
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  ;; then register them
  (define names (get-struct-names nm fld-names maker))
  (mk/register-sty names concrete-parent types
                   #:mutable mutable
                   ;; wrap everything in the approriate forall
                   #:poly? tvars)

  (unless type-only 
    (mk/register-struct-bindings names concrete-parent types
                     #:mutable mutable
                     ;; wrap everything in the approriate forall
                     #:poly? tvars)))


;; register a struct type
;; convenience function for built-in structs
;; tc/builtin-struct : identifier Maybe[identifier] Listof[identifier]
;;                     Listof[Type] Maybe[identifier] Listof[Type]
;;                     -> void
;; FIXME - figure out how to make this lots lazier
(define/cond-contract (tc/builtin-struct nm parent fld-names tys kernel-maker)
     (c-> identifier? (or/c #f identifier?) (listof identifier?)
          (listof Type/c) (or/c #f identifier?)
          any/c)
  (define parent-type (and parent (resolve-name (make-Name parent))))
  (define parent-tys (map fld-t (get-parent-flds parent-type)))
  (define names (get-struct-names nm fld-names #f))

  (mk/register-sty names parent-type tys #:mutable #t)
  (mk/register-struct-bindings names parent-type tys #:mutable #t)
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

