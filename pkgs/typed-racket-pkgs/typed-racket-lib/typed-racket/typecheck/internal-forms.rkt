#lang racket/base

(require
  "../utils/utils.rkt"
  syntax/parse
  (private parse-type)
  (typecheck internal-forms-base)
  (rep type-rep)
  (types abbrev)
  (env type-name-env lexical-env)
  (for-syntax racket/base racket/syntax
              syntax/parse syntax/parse/experimental/template)
  (for-template racket/base))

(provide
  (for-syntax internal)

  type-alias
  type-refinement
  typed-struct
  typed-struct/exec
  typed-require
  predicate-assertion
  type-declaration
  typecheck-failure

  type-alias?
  typed-struct?
  typed-struct/exec?)


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


(define-syntax-class parsed-type
  #:attributes (type literal-alls)
  (pattern t:expr
    #:attr type (parse-type #'t)
    #:attr literal-alls (parse-literal-alls #'t)))

(define-syntax-class predicate-type
  #:attributes (type)
  (pattern t:parsed-type
    #:attr type (make-pred-ty (attribute t.type))))

(define-splicing-syntax-class required-type
  #:attributes (type)
  (pattern (~seq :parsed-type))
  (pattern (~seq ty:parsed-type #:struct-maker _)
    #:attr type 
      (let* ([t (attribute ty.type)]
             [flds (map fld-t (Struct-flds (lookup-type-name (Name-id t))))])
        (flds #f . ->* . t))))

(define-syntax-class refinement-type
  #:attributes (predicate type)
  (pattern (~and predicate :typed-id/lexical/fail^)))

;;; Internal form syntax matching


(define-syntax-class internal^
   #:attributes (value)
   #:literals (values)
   #:literal-sets (kernel-literals)
   (pattern (define-values () (begin (quote-syntax value:expr) (#%plain-app values)))))

(define-syntax (define-internal-classes stx)
  (define-syntax-class clause
    (pattern [name:id (lit:id . body:expr)]
     #:with pred (format-id #'name "~a?" #'name)))

  (syntax-parse stx
    [(_ :clause ...)
     (template
       (begin
         (begin
           (define-syntax-class name
             #:auto-nested-attributes
             #:literal-sets ((internal-literals #:at name))
             (pattern i:internal^ #:with (lit . body) #'i.value))
           (define pred
             (syntax-parser
               [(~var _ name) #t]
               [_ #f]))) ...))]))


(define-internal-classes
  [type-alias
    (define-type-alias-internal name type)]
  [type-refinement
    (declare-refinement-internal :refinement-type)]
  [typed-struct
    (define-typed-struct-internal . :define-typed-struct-body)]
  [typed-struct/exec
    (define-typed-struct/exec-internal nm ([fields:id : types] ...) proc-type)]
  [typed-require
    (require/typed-internal name :required-type)]
  [predicate-assertion
    (assert-predicate-internal :predicate-type predicate)]
  [type-declaration
    (:-internal id:identifier :parsed-type)]
  [typecheck-failure
    (typecheck-fail-internal stx message:str var:id)])

