#lang scheme/base

(require "../utils/utils.rkt" 
	 (except-in (rep type-rep free-variance) Dotted)
         (private parse-type)
	 (types convenience utils union resolve abbrev substitute)
	 (env global-env type-env-structs type-name-env tvar-env)
	 (utils tc-utils)
         "def-binding.rkt"
         syntax/kerncase
         syntax/struct
         mzlib/trace      
         unstable/debug
         racket/function
         scheme/match
         (for-syntax scheme/base))


(require (for-template scheme/base
                       "internal-forms.rkt"))

(provide tc/struct tc/poly-struct names-of-struct tc/builtin-struct d-s)

(define (names-of-struct stx)
  (define (parent? stx)
    (syntax-case stx ()
      [(a b)
       (and (identifier? #'a)
            (identifier? #'b))
       #t]
      [a
       (identifier? #'a)
       #t]
      [_ #f]))
  (kernel-syntax-case* stx #f 
    (define-typed-struct-internal values)
    [(#%define-values () (begin (quote-syntax (define-typed-struct-internal (ids ...) nm/par . rest)) (#%plain-app values)))
     (and (andmap identifier? (syntax->list #'(ids ...)))
          (parent? #'nm/par))
     (let-values ([(nm _1 _2 _3 _4) (parse-parent #'nm/par)])
       (list nm))]
    [(#%define-values () (begin (quote-syntax (define-typed-struct-internal nm/par . rest)) (#%plain-app values)))
     (let-values ([(nm _1 _2 _3 _4) (parse-parent #'nm/par)])
       (list nm))]
    [(#%define-values () (begin (quote-syntax (define-typed-struct/exec-internal nm/par . rest)) (#%plain-app values)))
     (let-values ([(nm _1 _2 _3 _4) (parse-parent #'nm/par)])
       (list nm))]
    [_ (int-err "not define-typed-struct: ~a" (syntax->datum stx))]))

;; parse name field of struct, determining whether a parent struct was specified
;; syntax -> (values identifier Option[Type](must be name) Option[Type](must be struct) List[Types] Symbol Type)
(define (parse-parent nm/par)
  (syntax-case nm/par ()
    [nm (identifier? #'nm) (values #'nm #f #f (syntax-e #'nm) (make-F (syntax-e #'nm)))]
    [(nm par) (let* ([parent0 (parse-type #'par)]
                     [parent (if (Name? parent0) (resolve-name parent0) (tc-error/stx #'par "parent type not a valid structure name: ~a" (syntax->datum #'par)))])
                (values #'nm parent0 parent (syntax-e #'nm) (make-F (syntax-e #'nm))))]
    [_ (int-err "not a parent: ~a" (syntax->datum nm/par))]))

;; generate struct names given type name and field names
;; generate setters if setters? is true
;; all have syntax loc of name
;; identifier listof[identifier] boolean -> (values identifier identifier list[identifier] Option[list[identifier]])
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
    [(Struct: _ _ flds _ _ _ _ _ _) flds]
    [(Name: n) (get-parent-flds (lookup-type-name n))]
    [#f null]))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[Type] listof[Type] boolean -> Type listof[Type] listof[Type]
(define (mk/register-sty nm flds parent parent-field-types types 
                         #:wrapper [wrapper values] 
                         #:type-wrapper [type-wrapper values]
                         #:pred-wrapper [pred-wrapper values]
                         #:mutable [setters? #f]
                         #:struct-info [si #f]
                         #:proc-ty [proc-ty #f]
                         #:maker [maker* #f]
                         #:predicate [pred* #f]
                         #:constructor-return [cret #f]
                         #:poly? [poly? #f]
                         #:type-only [type-only #f])
  ;; create the approriate names that define-struct will bind
  (define-values (struct-type-id maker pred getters setters) (struct-names nm flds setters?))
  (let* ([name (syntax-e nm)]
         [fld-types (append parent-field-types types)]
         [sty (make-Struct name parent fld-types proc-ty poly? pred (syntax-local-certifier) getters (or maker* maker))]
         [external-fld-types/no-parent types]
         [external-fld-types fld-types])
    (if type-only
        (register-type-name nm (wrapper sty))
        (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters? 
                               #:wrapper wrapper
                               #:type-wrapper type-wrapper
                               #:pred-wrapper pred-wrapper
                               #:maker (or maker* maker)
                               #:predicate (or pred* pred)
                               #:struct-info si
                               #:poly? poly?
                               #:constructor-return cret))))

;; generate names, and register the approriate types give field types and structure type
;; optionally wrap things
;; identifier Type Listof[identifer] Listof[Type] Listof[Type] #:wrapper (Type -> Type) #:maker identifier
(define (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters?
                               #:wrapper [wrapper values]
                               #:struct-info [si #f]
                               #:type-wrapper [type-wrapper values]
                               #:pred-wrapper [pred-wrapper values]
                               #:maker [maker* #f]
                               #:predicate [pred* #f]
                               #:poly? [poly? #f]
                               #:constructor-return [cret #f])
  ;; create the approriate names that define-struct will bind
  (define-values (struct-type-id maker pred getters setters) (struct-names nm flds setters?))
  ;; the type name that is used in all the types
  (define name (type-wrapper (make-Name nm)))
  ;; is this structure covariant in *all* arguments?
  (define covariant? (if (and setters? poly?)
                         #f
                         (if poly?
                             (for*/and ([var (in-list poly?)]
                                        [t (in-list external-fld-types)])
                               (let ([variance (hash-ref (free-vars* t) var Constant)])
                                 (or (eq? variance Constant)
                                     (eq? variance Covariant))))
                             #t)))
  ;; the list of names w/ types
  (define bindings
    (append 
     (list 
      (cons struct-type-id
            (make-StructType sty))
      (cons (or maker* maker)
            (wrapper (->* external-fld-types (if cret cret name))))
      (cons (or pred* pred)
            (make-pred-ty (if (not covariant?)
                              (make-StructTop sty)
                              (pred-wrapper name)))))
     (for/list ([g (in-list getters)] [t (in-list external-fld-types/no-parent)] [i (in-naturals)])
       (let ([func (if setters? 
                       (->* (list name) t)
		       (->acc (list name) t (list (make-StructPE name i))))])
         (cons g (wrapper func))))
     (if setters?
         (map (lambda (g t) (cons g (wrapper (->* (list name t) -Void)))) setters external-fld-types/no-parent)
         null)))
  (register-type-name nm (wrapper sty))
  (cons
   (make-def-struct-stx-binding nm si)
   (for/list ([e bindings])
             (let ([nm (car e)]
                   [t (cdr e)])
               (register-type nm t)
               (make-def-binding nm t)))))

;; check and register types for a polymorphic define struct
;; tc/poly-struct : Listof[identifier] (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
(define (tc/poly-struct vars nm/par flds tys #:maker [maker #f])
  ;; parent field types can't actually be determined here
  (define-values (nm parent-name parent name name-tvar) (parse-parent nm/par))
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
        (instantiate-poly parent new-tvars)
        parent))
  ;; get the fields of the parent, if it exists
  (define parent-field-types (get-parent-flds concrete-parent))
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  ;; then register them
  (mk/register-sty nm flds parent-name parent-field-types types
                   #:maker maker
                   ;; wrap everything in the approriate forall
                   #:wrapper (λ (t) (make-Poly tvars t))
                   #:type-wrapper (λ (t) (make-App t new-tvars #f))
                   #:pred-wrapper (λ (t) (subst-all (make-simple-substitution tvars (map (const Univ) tvars)) t))
                   #:poly? tvars))


;; typecheck a non-polymophic struct and register the approriate types
;; tc/struct : (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
(define (tc/struct nm/par flds tys [proc-ty #f] 
                   #:maker [maker #f] #:constructor-return [cret #f] #:mutable [mutable #f]
                   #:predicate [pred #f]
                   #:type-only [type-only #f])
  ;; get the parent info and create some types and type variables
  (define-values (nm parent-name parent name name-tvar) (parse-parent nm/par))
  ;; parse the field types, and determine if the type is recursive
  (define types (map parse-type tys))
  (define proc-ty-parsed  
    (if proc-ty
        (parse-type proc-ty)
        #f))
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  (mk/register-sty nm flds parent-name (get-parent-flds parent) types
                   ;; procedure
                   #:proc-ty proc-ty-parsed
                   #:maker maker
                   #:predicate pred
                   #:struct-info (syntax-property nm/par 'struct-info)
		   #:constructor-return (and cret (parse-type cret))
                   #:mutable mutable
                   #:type-only type-only))

;; register a struct type
;; convenience function for built-in structs
;; tc/builtin-struct : identifier identifier Listof[identifier] Listof[Type] Listof[Type] -> void
(define (tc/builtin-struct nm parent flds tys parent-tys)
  (let ([parent* (if parent (make-Name parent) #f)])
    (mk/register-sty nm flds parent* parent-tys tys
                     #:mutable #t)))

;; syntax for tc/builtin-struct
(define-syntax (d-s stx) 
  (syntax-case stx (:) 
    [(_ (nm par) ([fld : ty] ...) (par-ty ...))
     #'(tc/builtin-struct #'nm #'par
                          (list #'fld ...)
                          (list ty ...)
                          (list par-ty ...))]
    [(_ nm ([fld : ty] ...) (par-ty ...))
     #'(tc/builtin-struct #'nm #f
                          (list #'fld ...)
                          (list ty ...)
                          (list par-ty ...))]))

