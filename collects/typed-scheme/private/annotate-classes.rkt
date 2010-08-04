#lang scheme/base

(require syntax/parse "colon.rkt" (for-template "colon.rkt") "parse-type.rkt")
(provide (all-defined-out))

(define-splicing-syntax-class annotated-name
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literals (:)
  (pattern [~seq name:id : ty]
           #:with ann-name (syntax-property #'name 'type-label #'ty))
  (pattern name:id
           #:when (syntax-property #'name 'type-label)
           #:with ty (syntax-property #'name 'type-label)
           #:with ann-name #'name))

(define-splicing-syntax-class (param-annotated-name trans)
  #:attributes (name ty ann-name)
  #:description "type-annotated identifier"
  #:literals (:)
  (pattern [~seq name:id : ty]
           #:with ann-name (syntax-property #'name 'type-label (trans #'ty))))

(define-syntax-class annotated-binding
  #:attributes (name ty ann-name binding rhs)
  (pattern (~and whole [:annotated-name rhs:expr])
           #:with binding (syntax/loc #'whole [ann-name rhs])))

(define-syntax-class annotated-values-binding
  #:attributes ((name 1) (ty 1) (ann-name 1) binding rhs)
  (pattern (~and whole [(~describe "sequence of type-annotated identifiers" ([:annotated-name] ...)) rhs:expr])
           #:with binding (syntax/loc #'whole [(ann-name ...) rhs])))

(define-splicing-syntax-class annotated-star-rest
  #:attributes (name ann-name ty formal-ty)
  #:literals (:)  
  (pattern (~seq name:id : ty s:star)
           #:with formal-ty #'(ty s)
           #:with ann-name (syntax-property #'name 'type-label #'ty)))

(define-splicing-syntax-class annotated-dots-rest
  #:attributes (name ann-name bound ty formal-ty)
  #:literals (:)
  (pattern (~seq name:id : ty bnd:ddd/bound)
           #:with formal-ty #'(ty bnd)
           #:attr bound (attribute bnd.bound)
           #:with ann-name (syntax-property (syntax-property #'name 'type-label #'ty)
                                            'type-dotted (attribute bnd.bound))))

(define-syntax-class annotated-formal
  #:description "annotated variable of the form [x : T]"
  #:opaque
  #:attributes (name ty ann-name)
  (pattern [:annotated-name]))

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
