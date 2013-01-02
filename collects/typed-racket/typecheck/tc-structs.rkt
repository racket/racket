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

(provide tc/struct names-of-struct d-s
         refine-struct-variance!
         register-parsed-struct-sty!
         register-parsed-struct-bindings!)

(define-syntax-class parent
  #:attributes (name par)
  (pattern (name:id par:id))
  (pattern name:id #:attr par #f))

;; sty : Struct?
;; names : Listof[Identifier]
;; desc : struct-desc
;; type-only : Boolean
(struct parsed-struct (sty names desc struct-info type-only) #:transparent)

;; type-name : Id
;; struct-type : Id
;; constructor : Id
;; predicate : Id
;; getters : Listof[Id]
;; setters : Listof[Id] or #f
(struct struct-names (type-name struct-type constructor predicate getters setters) #:transparent)

;;struct-fields: holds all the relevant information about a struct type's types
(struct struct-desc (parent-fields self-fields tvars mutable proc-ty) #:transparent)

(define (struct-desc-all-fields fields)
  (append (struct-desc-parent-fields fields) (struct-desc-self-fields fields)))
(define (struct-desc-parent-count fields)
  (length (struct-desc-parent-fields fields)))


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
                                  (displayln parent0)
                                  (tc-error/stx #'v.par "parent type not a valid structure name: ~a"
                                                (syntax->datum #'v.par)))))])
                (values #'v.name parent0 parent))
          (values #'v.name #f #f))]))


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
(define/cond-contract (get-flds p)
  (c-> (or/c Struct? #f) (listof fld?))
  (match p
    [(Struct: _ _ flds _ _ _) flds]
    [#f null]))


;; Constructs the Struct value for a structure type
;; The returned value has free type variables
(define (mk/inner-struct-type names desc parent)
  (c-> struct-names? struct-desc? (or/c Struct? #f) void?)

  (let* ([this-flds (for/list ([t (in-list (struct-desc-self-fields desc))]
                               [g (in-list (struct-names-getters names))])
                       (make-fld t g (struct-desc-mutable desc)))]
         [flds (append (get-flds parent) this-flds)])
    (make-Struct (struct-names-type-name names)
                 parent flds (struct-desc-proc-ty desc)
                 (not (null? (struct-desc-tvars desc)))
                 (struct-names-predicate names))))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[fld] listof[Type] boolean ->
;;  (values Type listof[Type] listof[Type])
(define/cond-contract (register-sty! sty names desc)
  (c-> Struct? struct-names? struct-desc? void?)

  (register-type-name (struct-names-type-name names)
                      (make-Poly (struct-desc-tvars desc) sty)))




;; Register the approriate types to the struct bindings.
(define/cond-contract (register-struct-bindings! sty names desc si)
  (c-> Struct? struct-names? struct-desc? (or/c #f struct-info?) (listof def-binding?))


  (define tvars (struct-desc-tvars desc))
  (define all-fields (struct-desc-all-fields desc))
  (define self-fields (struct-desc-self-fields desc))
  (define mutable (struct-desc-mutable desc))
  (define parent-count (struct-desc-parent-count desc))


  ;; the base-type, with free type variables
  (define poly-base
    (if (null? tvars)
        (make-Name (struct-names-type-name names))
        (make-App (make-Name (struct-names-type-name names)) (map make-F tvars) #f)))

  ;; is this structure covariant in *all* arguments?
  (define covariant?
    (for*/and ([var (in-list tvars)]
               [t (in-list all-fields)])
      (let ([variance (hash-ref (free-vars-hash (free-vars* t)) var Constant)])
        (or (eq? variance Constant)
            (and (not mutable) (eq? variance Covariant))))))

  (define (poly-wrapper t) (make-Poly tvars t))
  (define bindings
    (list*
     ;; the list of names w/ types
     (cons (struct-names-struct-type names) (make-StructType sty))
     (cons (struct-names-constructor names) (poly-wrapper (->* all-fields poly-base)))
     (cons (struct-names-predicate names)
           (make-pred-ty (if (not covariant?)
                             (make-StructTop sty)
                             (subst-all (make-simple-substitution
                                         tvars (map (const Univ) tvars)) poly-base))))
     (append
      (for/list ([g (in-list (struct-names-getters names))]
                 [t (in-list self-fields)]
                 [i (in-naturals parent-count)])
        (let* ([path (make-StructPE poly-base i)]
               [func (poly-wrapper
                      (if mutable
                          (->* (list poly-base) t)
                          (->acc (list poly-base) t (list path))))])
          (add-struct-fn! g path #f)
          (cons g func)))
      (if mutable
          (for/list ([s (in-list (struct-names-setters names))]
                     [t (in-list self-fields)]
                     [i (in-naturals parent-count)])
            (add-struct-fn! s (make-StructPE poly-base i) #t)
            (cons s (poly-wrapper (->* (list poly-base t) -Void))))
          null))))

  (add-struct-constructor! (struct-names-constructor names))

  (define def-bindings
    (for/list ([b bindings])
        (define id (car b))
        (define t (cdr b))
        (register-type id t)
        (make-def-binding id t)))
  (if si
    (cons
      (make-def-struct-stx-binding (struct-names-type-name names) si)
      def-bindings)
    def-bindings))

(define (register-parsed-struct-sty! ps)
  (match ps
    ((parsed-struct sty names desc si type-only)
     (register-sty! sty names desc))))

(define (register-parsed-struct-bindings! ps)
  (match ps
    ((parsed-struct sty names desc si type-only)
     (if type-only
         null
         (register-struct-bindings! sty names desc si)))))

(define (refine-struct-variance! parsed-structs)
  (define stys (map parsed-struct-sty parsed-structs))
  (define tvarss (map (compose struct-desc-tvars parsed-struct-desc)  parsed-structs))
  (let loop ()
    (define sames
      (for/list ((sty stys) (tvars tvarss))
        (cond
          ((null? tvars) #t)
          (else
            (define name (Struct-name sty))
            (define free-vars (free-vars-hash (free-vars* sty)))
            (define variance (map (λ (v) (hash-ref free-vars v Constant)) tvars))
            (define old-variance (lookup-type-variance name))

            (register-type-variance! name variance)
            (equal? variance old-variance)))))
    (unless (andmap values sames)
      (loop))))


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
            (tc-error "Could not instantiate parent struct type. Required ~a type variables, received ~a."
              (Poly-n parent)
              (length new-tvars))
            (instantiate-poly parent (take new-tvars (Poly-n parent))))
        parent))
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  ;; then register it
  (define names (get-struct-names nm fld-names maker))
  (define desc (struct-desc
                 (map fld-t (get-flds concrete-parent))
                 types
                 tvars
                 mutable
                 (and proc-ty (parse-type proc-ty))))
  (define sty (mk/inner-struct-type names desc concrete-parent))

  (parsed-struct sty names desc (syntax-property nm/par 'struct-info) type-only))


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
  (define parent-tys (map fld-t (get-flds parent-type)))

  (define names (get-struct-names nm fld-names #f))
  (define desc (struct-desc parent-tys tys null #t #f))
  (define sty (mk/inner-struct-type names desc parent-type))

  (register-sty! sty names desc)
  (register-struct-bindings! sty names desc #f)
  (when kernel-maker
    (register-type kernel-maker (λ () (->* (struct-desc-all-fields desc) sty)))))

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

