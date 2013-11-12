#lang racket/base

(require
  (only-in "utils.rkt" typecheck)
  syntax/parse 
  (for-template (typecheck internal-forms))
  (for-template racket/base))
(provide 
  type-alias 
  type-refinement
  typed-struct
  typed-struct/exec
  typed-require
  typed-require/struct
  predicate-assertion
  type-declaration
  
  type-alias?
  typed-struct?
  typed-struct/exec?
  )

(define-syntax-class internal
   #:attributes (value)
   #:literals (values)
   #:literal-sets (kernel-literals)
   (pattern (define-values () (begin (quote-syntax value:expr) (#%plain-app values)))))


(define-syntax-class type-alias
  #:attributes (name type)
  (pattern i:internal
    #:with ((~literal define-type-alias-internal) name type) #'i.value))

(define-syntax-class type-refinement
  #:attributes (predicate)
  (pattern i:internal
    #:with ((~literal declare-refinement-internal) predicate) #'i.value))

(define-syntax-class typed-struct
  #:attributes (name mutable type-only maker nm (tvars 1) (fields 1) (types 1))
  (pattern i:internal
    #:with ((~literal define-typed-struct-internal) . :define-typed-struct-body) #'i.value))

(define-syntax-class typed-struct/exec
  #:attributes (nm proc-type (fields 1) (types 1))
  (pattern i:internal
    #:with ((~literal define-typed-struct/exec-internal)
            nm ([fields:id : types] ...) proc-type) #'i.value))

(define-syntax-class typed-require
  #:attributes (name type)
  (pattern i:internal
    #:with ((~literal require/typed-internal) name type) #'i.value))

(define-syntax-class typed-require/struct
  #:attributes (name type)
  (pattern i:internal
    #:with ((~literal require/typed-internal) name type #:struct-maker parent) #'i.value))

(define-syntax-class predicate-assertion
  #:attributes (type predicate)
  (pattern i:internal
    #:with ((~literal assert-predicate-internal) type predicate) #'i.value))

(define-syntax-class type-declaration
  #:attributes (id type)
  (pattern i:internal
    #:with ((~literal :-internal) id:identifier type) #'i.value))


(define type-alias?
  (syntax-parser
    [:type-alias #t]
    [_ #f]))

(define typed-struct?
  (syntax-parser
    [:typed-struct #t]
    [_ #f]))

(define typed-struct/exec?
  (syntax-parser
    [:typed-struct/exec #t]
    [_ #f]))

;;; Helpers

(define-splicing-syntax-class dtsi-fields
 #:attributes (mutable type-only maker)
 (pattern
  (~seq
    (~or (~optional (~and #:mutable (~bind (mutable #t))))
         (~optional (~and #:type-only (~bind (type-only #t))))
         (~optional (~seq #:maker maker))) ...)))

(define-syntax-class struct-name
 (pattern nm:id)
 (pattern (nm:id parent:id)))


(define-syntax-class define-typed-struct-body
  #:attributes (name mutable type-only maker nm (tvars 1) (fields 1) (types 1))
  (pattern ((~optional (tvars:id ...) #:defaults (((tvars 1) null)))
            nm:struct-name ([fields:id : types:expr] ...) options:dtsi-fields)
           #:attr name #'nm.nm
           #:attr mutable (attribute options.mutable)
           #:attr type-only (attribute options.type-only)
           #:attr maker (or (attribute options.maker) #'nm.nm)))


