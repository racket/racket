#lang racket/base

(require syntax/parse
         "../private/parse-classes.rkt"
         "../private/syntax-properties.rkt"
         (for-template "colon.rkt"))
(provide (all-defined-out))

(define-splicing-syntax-class annotated-name
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literals (:)
  (pattern [~seq name:id : ty]
           #:with ann-name (type-label-property #'name #'ty))
  (pattern name:id
           #:with ty (type-label-property #'name)
           #:when #'ty
           #:with ann-name #'name))

(define-splicing-syntax-class optionally-annotated-name
  #:attributes (name ann-name)
  #:description "optionally type-annotated identifier"
  #:literals (:)
  (pattern n:annotated-name
           #:with name #'n.name
           #:with ann-name #'n.ann-name)
  (pattern n:id
           #:with name #'n
           #:with ann-name #'n))

(define-splicing-syntax-class (param-annotated-name trans)
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literals (:)
  (pattern [~seq name:id : ty]
           #:with ann-name (type-label-property #'name (trans #'ty))))

(define-syntax-class annotated-binding
  #:attributes (name ty ann-name binding rhs)
  (pattern (~and whole [:annotated-name rhs:expr])
           #:with binding (syntax/loc #'whole [ann-name rhs])))

(define-syntax-class optionally-annotated-binding
  #:attributes (name ann-name binding rhs)
  #:description "optionally type-annotated binding"
  #:literals (:)
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
  #:literals (:)
  (pattern (~seq name:id : ty s:star)
           #:with formal-ty #'(ty s)
           #:with ann-name (type-label-property #'name #'ty)))

(define-splicing-syntax-class annotated-dots-rest
  #:attributes (name ann-name bound ty formal-ty)
  #:literals (:)
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
  #:attributes (name ann-name)
  (pattern f:annotated-formal
           #:with name #'f.name
           #:with ann-name #'f.ann-name)
  (pattern f:id
           #:with name #'f
           #:with ann-name #'f))

(define-syntax-class annotated-formals
  #:attributes (ann-formals (arg-ty 1))
  #:literals (:)
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
  #:literals (:)
  (pattern (n:opt-lambda-annotated-formal ...)
           #:with ann-formals #'(n.ann-name ...)
           #:with (arg-ty ...) #'(n.ty ...))
  (pattern (n:opt-lambda-annotated-formal ...
            (~describe "dotted or starred type"
                       (~or rest:annotated-star-rest rest:annotated-dots-rest)))
           #:with ann-formals #'(n.ann-name ... . rest.ann-name)
           #:with (arg-ty ...) #'(n.ty ... . rest.formal-ty)))

(define-splicing-syntax-class standalone-annotation
  #:literals (:)
  (pattern (~seq : t)
           #:with ty #'t))
(define-splicing-syntax-class optional-standalone-annotation
  (pattern (~optional a:standalone-annotation)
           #:with ty (if (attribute a) #'a.ty #f)))
