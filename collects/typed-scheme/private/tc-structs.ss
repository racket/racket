#lang scheme/base

(require (lib "struct.ss" "syntax")
         (lib "etc.ss")
         "type-rep.ss" ;; doesn't need tests
         "type-effect-convenience.ss" ;; maybe needs tests
         "type-env.ss" ;; maybe needs tests
         "type-utils.ss"
         "parse-type.ss" ;; has tests
         "type-environments.ss" ;; doesn't need tests
         "type-name-env.ss" ;; maybe needs tests
         "union.ss"
         "tc-utils.ss"
         "resolve-type.ss"
         (lib "kerncase.ss" "syntax")
         (lib "trace.ss")
         (lib "kw.ss")
         (lib "plt-match.ss"))


(require (for-template scheme/base
                       "internal-forms.ss"))

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
                     [parent (resolve-name parent0)])
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
    [(list _ maker pred getters/setters ...) 
     (if setters?
         (let-values ([(getters setters) (split getters/setters)])
           (values maker pred getters setters))
         (values maker pred getters/setters #f))]))

;; gets the fields of the parent type, if they exist
;; Option[Struct-Ty] -> Listof[Type]
(define (get-parent-flds p)
  (match p
    [(Struct: _ _ flds _) flds]
    [(Name: n) (get-parent-flds (lookup-type-name n))]
    [#f null]))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[Type] listof[Type] boolean -> Type listof[Type] listof[Type]
(define (mk/register-sty nm flds parent parent-field-types types 
                         #:wrapper [wrapper values] 
                         #:type-wrapper [type-wrapper values]
                         #:mutable [setters? #f]
                         #:proc-ty [proc-ty #f]
                         #:maker [maker #f])
  (let* ([name (syntax-e nm)]
         [fld-types (append parent-field-types types)]
         [sty (make-Struct name parent fld-types proc-ty)]
         [external-fld-types/no-parent types]
         [external-fld-types fld-types])
    (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters? 
                           #:wrapper wrapper
                           #:type-wrapper type-wrapper
                           #:maker maker)))

;; generate names, and register the approriate types give field types and structure type
;; optionally wrap things
;; identifier Type Listof[identifer] Listof[Type] Listof[Type] #:wrapper (Type -> Type) #:maker identifier
(define (register-struct-types nm sty flds external-fld-types external-fld-types/no-parent setters?
                               #:wrapper [wrapper (lambda (x) x)]
                               #:type-wrapper [type-wrapper values]
                               #:maker [maker* #f])
  ;; create the approriate names that define-struct will bind
  (define-values (maker pred getters setters) (struct-names nm flds setters?))
  ;; the type name that is used in all the types
  (define name (type-wrapper (make-Name nm)))
  ;; register the type name
  (register-type-name nm (wrapper sty))
  ;; register the various function types
  (register-type (or maker* maker) (wrapper (->* external-fld-types name)))
  (register-types getters
                  (map (lambda (t) (wrapper (->* (list name) t))) external-fld-types/no-parent))
  (when setters?    
    #;(printf "setters: ~a~n" (syntax-object->datum setters))
    (register-types setters
                    (map (lambda (t) (wrapper (->* (list name t) -Void))) external-fld-types/no-parent)))
  (register-type pred (make-pred-ty (wrapper name))))

;; check and register types for a polymorphic define struct
;; tc/poly-struct : Listof[identifier] (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
(define (tc/poly-struct vars nm/par flds tys)
  ;; parent field types can't actually be determined here
  (define-values (nm parent-name parent name name-tvar) (parse-parent nm/par))
  ;; create type variables for the new type parameters
  (define tvars (map syntax-e vars))
  (define new-tvars (map make-F tvars))
  ;; parse the types
  (define types
    ;; add the type parameters of this structure to the tvar env
    (parameterize ([current-tvars (extend-env tvars new-tvars (current-tvars))])
      ;; parse the field types
      (map parse-type tys)))
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
                   ;; wrap everything in the approriate forall
                   #:wrapper (lambda (t) (make-Poly tvars t))
                   #:type-wrapper (lambda (t) (make-App t new-tvars #f))))


;; typecheck a non-polymophic struct and register the approriate types
;; tc/struct : (U identifier (list identifier identifier)) Listof[identifier] Listof[syntax] -> void
(define (tc/struct nm/par flds tys [proc-ty #f] #:maker [maker #f])
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
                   #:maker maker))

;; register a struct type
;; convenience function for built-in structs
;; tc/builtin-struct : identifier identifier Listof[identifier] Listof[Type] Listof[Type] -> void
(define (tc/builtin-struct nm parent flds tys parent-tys)
  (let ([parent* (if parent (make-Name parent) #f)])
    (mk/register-sty nm flds parent* parent-tys tys #:mutable #t)))

;; syntax for tc/builtin-struct
(define-syntax d-s 
  (syntax-rules (:) 
    [(_ (nm par) ([fld : ty] ...) (par-ty ...))
     (tc/builtin-struct #'nm #'par
                        (list #'fld ...)
                        (list ty ...)
                        (list par-ty ...))]
    [(_ nm ([fld : ty] ...) (par-ty ...))
     (tc/builtin-struct #'nm #f
                        (list #'fld ...)
                        (list ty ...)
                        (list par-ty ...))]))

;; This is going away!
#|

;; parent-nm is an identifier with the name of the defined type
;; variants is (list id id (list (cons id unparsed-type))) - first id is name of variant, second is name of maker, 
;;     list is name of field w/ type
;; top-pred is an identifier
;; produces void
(define (tc/define-type parent-nm top-pred variants)
  ;; the symbol and type variable used for parsing
  (define parent-sym (syntax-e parent-nm))
  (define parent-tvar (make-F parent-sym))
  
  ;; create the initial struct type, which contains type variables
  (define (mk-initial-variant nm fld-tys-stx)
    ;; parse the types (recursiveness doesn't matter)
    (define-values (fld-tys _) (FIXME parent-sym parent-tvar fld-tys-stx))     
    (make-Struct (syntax-e nm) #f fld-tys #f))
  
  ;; create the union type that is the total type
  (define (mk-un-ty parent-sym variant-struct-tys)
    (make-Mu parent-sym (apply Un variant-struct-tys)))     
  
  ;; generate the names and call mk-variant
  (define (mk-variant nm maker-name fld-names un-ty variant-struct-ty parent-nm)
    ;; construct the actual type of this variant
    (define variant-ty (subst parent-nm un-ty variant-struct-ty))
    ;; the fields of this variant
    (match-define (Struct: _ _ fld-types _) variant-ty)
    ;; register all the types (with custon maker name)
    (register-struct-types nm variant-ty fld-names fld-types fld-types #f #:maker maker-name))
  
  ;; all the names
  (define variant-names (map car variants))
  (define variant-makers (map cadr variants))
  (define variant-flds (map caddr variants))    
  ;; create the initial variants, which don't have the parent substituted in
  (define variant-struct-tys (map (lambda (n flds) (mk-initial-variant n (map car flds))) variant-names variant-flds))
  ;; just the names of each variant's fields
  (define variant-fld-names (map (lambda (x) (map cdr x)) variant-flds))
  
  ;; the type of the parent
  (define un-ty (mk-un-ty parent-sym variant-struct-tys))
  
  ;; register the types for the parent
  (register-type top-pred (make-pred-ty un-ty))
  (register-type-name parent-nm un-ty)
  
  ;; construct all the variants, and register the appropriate names
  (for-each (lambda (nm mk fld-names sty) (mk-variant nm mk fld-names un-ty sty parent-sym))
            variant-names variant-makers variant-fld-names variant-struct-tys))



|#