#lang racket/base

(require syntax/parse
         syntax/parse/experimental/template
         "../private/parse-classes.rkt"
         "../private/syntax-properties.rkt"
         (for-label "colon.rkt"))
(provide (all-defined-out))

;; Data definitions
;; ----------------
;;
;; A LambdaKeywords is a
;;   (lambda-kws (Listof Keyword) (Listof Keyword))
(struct lambda-kws (mand opt))

;; interp.
;;   - the first list contains the mandatory keywords
;;   - the second list contains the optional keywords
;;
;; The TR lambda form sets this as a syntax property on lambda expansions
;; to allow TR to check for missing keywords.

(define-literal-set colon #:for-label (:))

(define-splicing-syntax-class annotated-name
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literal-sets (colon)
  (pattern [~seq name:id : ty]
           #:with ann-name (type-label-property #'name #'ty))
  (pattern name:id
           #:attr *ty (type-label-property #'name)
           #:when (attribute *ty)
           #:attr ty (datum->syntax #'name (attribute *ty))
           #:with ann-name #'name))

(define-splicing-syntax-class optionally-annotated-name
  #:attributes (name ty ann-name)
  #:description "optionally type-annotated identifier"
  #:literal-sets (colon)
  (pattern n:annotated-name
           #:with name #'n.name
           #:with ty #'n.ty
           #:with ann-name #'n.ann-name)
  (pattern n:id
           #:with name #'n
           #:attr ty #f
           #:with ann-name #'n))

(define-splicing-syntax-class (param-annotated-name trans)
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literal-sets (colon)
  (pattern [~seq name:id : ty]
           #:with ann-name (type-label-property #'name (trans #'ty))))

(define-syntax-class annotated-binding
  #:attributes (name ty ann-name binding rhs)
  (pattern (~and whole [:annotated-name rhs:expr])
           #:with binding (syntax/loc #'whole [ann-name rhs])))

(define-syntax-class optionally-annotated-binding
  #:attributes (name ann-name binding rhs)
  #:description "optionally type-annotated binding"
  #:literal-sets (colon)
  (pattern b:annotated-binding
           #:with name #'b.name
           #:with ann-name #'b.ann-name
           #:with binding #'b.binding
           #:with rhs #'b.rhs)
  (pattern (~and whole [n:id rhs:expr])
           #:with name #'n
           #:with ann-name #'n
           #:with binding #'whole))

(define-syntax-class annotated-values-binding
  #:attributes ((name 1) (ty 1) (ann-name 1) binding rhs)
  (pattern (~and whole [(~describe "sequence of type-annotated identifiers" ([:annotated-name] ...)) rhs:expr])
           #:with binding (syntax/loc #'whole [(ann-name ...) rhs])))

(define-syntax-class optionally-annotated-values-binding
  #:attributes ((name 1) (ann-name 1) binding rhs)
  (pattern b:annotated-values-binding
           #:with (name ...) #'(b.name ...)
           #:with (ann-name ...) #'(b.ann-name ...)
           #:with binding #'b.binding
           #:with rhs #'b.rhs)
  (pattern (~and whole [(~describe "sequence of optionally type-annotated identifiers" (n:optionally-annotated-formal ...)) rhs:expr])
           #:with (name ...) #'(n.name ...)
           #:with (ann-name ...) #'(n.ann-name ...)
           #:with binding #'whole))

(define-splicing-syntax-class annotated-star-rest
  #:attributes (name ann-name ty formal-ty)
  #:literal-sets (colon)
  (pattern (~seq name:id : ty s:star)
           #:with formal-ty #'(ty s)
           #:with ann-name (type-label-property #'name #'ty)))

(define-splicing-syntax-class annotated-dots-rest
  #:attributes (name ann-name bound ty formal-ty)
  #:literal-sets (colon)
  (pattern (~seq name:id : ty bnd:ddd/bound)
           #:with formal-ty #'(ty . bnd)
           #:attr bound (attribute bnd.bound)
           #:with ann-name (type-dotted-property
                             (type-label-property #'name #'ty)
                             (attribute bnd.bound))))

(define-syntax-class annotated-formal
  #:description "annotated variable of the form [x : T]"
  #:opaque
  #:attributes (name ty ann-name)
  (pattern [:annotated-name]))

(define-syntax-class optionally-annotated-formal
  #:description "optionally annotated variable of the form [x : T] or just x"
  #:opaque
  #:attributes (name ty ann-name)
  (pattern f:annotated-formal
           #:with name #'f.name
           #:attr ty #'f.ty
           #:with ann-name #'f.ann-name)
  (pattern f:id
           #:with name #'f
           #:attr ty #f
           #:with ann-name #'f))

(define-syntax-class annotated-formals
  #:attributes (ann-formals (arg-ty 1))
  #:literal-sets (colon)
  (pattern (n:annotated-formal ...)
           #:with ann-formals #'(n.ann-name ...)
           #:with (arg-ty ...) #'(n.ty ...))
  (pattern (n:annotated-formal ... (~describe "dotted or starred type"
                                              (~or rest:annotated-star-rest rest:annotated-dots-rest)))
           #:with ann-formals #'(n.ann-name ... . rest.ann-name)
           #:with (arg-ty ...) #'(n.ty ... . rest.formal-ty)))

(define-syntax-class opt-lambda-annotated-formal
  #:description "annotated variable, potentially with a default value"
  #:opaque
  #:attributes (name ty ann-name)
  (pattern [:annotated-name])
  (pattern [n:annotated-name val]
           #:with name #'n.name
           #:with ty #'n.name
           #:with ann-name #'(n.ann-name val)))

(define-syntax-class opt-lambda-annotated-formals
  #:attributes (ann-formals (arg-ty 1))
  #:literal-sets (colon)
  (pattern (n:opt-lambda-annotated-formal ...)
           #:with ann-formals #'(n.ann-name ...)
           #:with (arg-ty ...) #'(n.ty ...))
  (pattern (n:opt-lambda-annotated-formal ...
            (~describe "dotted or starred type"
                       (~or rest:annotated-star-rest rest:annotated-dots-rest)))
           #:with ann-formals #'(n.ann-name ... . rest.ann-name)
           #:with (arg-ty ...) #'(n.ty ... . rest.formal-ty)))

(define-splicing-syntax-class standalone-annotation
  #:literal-sets (colon)
  (pattern (~seq : t)
           #:with ty #'t))
(define-splicing-syntax-class optional-standalone-annotation
  (pattern (~optional a:standalone-annotation)
           #:attr ty (if (attribute a) #'a.ty #f)))

(define-syntax-class type-variables
  #:attributes ((vars 1))
  #:description "a sequence of type variables"
  (pattern (vars:id ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'(vars ...)))
           "duplicate type variable declaration"))

(define-splicing-syntax-class lambda-type-vars
  #:description "optional type parameters"
  #:attributes (type-vars)
  (pattern (~seq (~or #:forall #:∀) (var:id ...))
           #:attr type-vars #'(var ...)))

(define-splicing-syntax-class maybe-lambda-type-vars
  #:description "optional type parameters"
  #:attributes (type-vars)
  (pattern :lambda-type-vars)
  (pattern (~seq) #:attr type-vars #f))

(define-splicing-syntax-class kw-formal
  #:attributes (form id default type kw)
  #:literal-sets (colon)
  (pattern (~seq kw:keyword id:id)
           #:with form #'(kw id)
           #:attr default #f
           #:attr type #f)
  (pattern (~seq kw:keyword [id:id default:expr])
           #:with form #'(kw [id default])
           #:attr type #f)
  (pattern (~seq kw:keyword [id:id : type:expr])
           #:with form #`(kw #,(type-label-property #'id #'type))
           #:attr default #f)
  (pattern (~seq kw:keyword [id:id : type:expr default:expr])
           #:with form #`(kw [#,(type-label-property #'id #'type) default])))

(define-splicing-syntax-class mand-formal
  #:description "lambda argument"
  #:attributes (form id default type kw)
  #:literal-sets (colon)
  (pattern id:id
           #:with form #'(id)
           #:attr default #f
           #:attr type #f
           #:attr kw #f)
  (pattern [id:id : type:expr]
           #:with form #`(#,(type-label-property #'id #'type))
           #:attr default #f
           #:attr kw #f)
  (pattern :kw-formal))

(define-splicing-syntax-class opt-formal
  #:description "optional lambda argument"
  #:attributes (form id default type kw)
  #:literal-sets (colon)
  (pattern [id:id default:expr]
           #:with form #'([id default])
           #:attr type #f
           #:attr kw #f)
  (pattern [id:id : type:expr default:expr]
           #:with form #`([#,(type-label-property #'id #'type) default])
           #:attr kw #f)
  (pattern :kw-formal))

(define-syntax-class rest-arg
  #:description "rest argument"
  #:attributes (form)
  #:literal-sets (colon)
  ;; specifying opaque here helps produce a better error
  ;; message for optional argumenents, but produces worse
  ;; error messages for rest arguments.
  #:opaque
  (pattern rest:id #:attr form #'rest)
  (pattern (rest:id : type:expr :star)
           #:attr form (type-label-property #'rest #'type))
  (pattern (rest:id : type:expr bnd:ddd/bound)
           #:attr bound (attribute bnd.bound)
           #:attr form (type-dotted-property
                        (type-label-property #'rest #'type)
                        (attribute bound))))

(define-syntax-class lambda-formals
  #:attributes (opt-property kw-property erased)
  (pattern (~or (mand:mand-formal ... opt:opt-formal ... . rest:rest-arg)
                (~and (mand:mand-formal ... opt:opt-formal ...)
                      (~bind [rest.form #'()])))
           #:attr kw-property
           ;; separate raw keywords into mandatory and optional and
           ;; put them in a struct for later use by tc-expr
           (let ([kws (append (attribute mand.kw)
                              (attribute opt.kw))]
                 [opt?s (append (attribute mand.default)
                                (attribute opt.default))])
             (define-values (mand-kws opt-kws)
               (for/fold ([mand-kws '()]
                          [opt-kws '()])
                         ([kw (in-list kws)]
                          [opt? (in-list opt?s)]
                          #:when kw)
                 (if opt?
                     (values mand-kws (cons (syntax-e kw) opt-kws))
                     (values (cons (syntax-e kw) mand-kws) opt-kws))))
             (and (or (not (null? mand-kws))
                      (not (null? opt-kws)))
                  (lambda-kws mand-kws opt-kws)))
           #:attr opt-property
           (list (length (attribute mand)) (length (attribute opt)))
           #:attr erased
           (template ((?@ . mand.form) ... (?@ . opt.form) ... . rest.form))))

(define-syntax-class curried-formals
  #:attributes (erased fun-name)
  (pattern fun:id
           #:with fun-name #'fun
           #:with erased #'fun)
  (pattern (fun:curried-formals . formals:lambda-formals)
           #:with fun-name #'fun.fun-name
           #:with erased #`(fun.erased . #,(attribute formals.erased))))

(define-splicing-syntax-class return-ann
  #:description "return type annotation"
  #:literal-sets (colon)
  (pattern (~seq : type:expr))
  (pattern (~seq) #:attr type #f))
