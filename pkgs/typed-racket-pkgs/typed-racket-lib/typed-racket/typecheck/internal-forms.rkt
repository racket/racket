#lang racket/base

(require
  syntax/parse
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
  typed-require/struct
  predicate-assertion
  type-declaration
  typecheck-failure

  type-alias?
  typed-struct?
  typed-struct/exec?)

;; Forms
(define-syntax-rule (internal-forms set-name nms ...)
  (begin
    (provide nms ... set-name)
    (define-literal-set set-name (nms ...))
    (define-syntax (nms stx) (raise-syntax-error 'typecheck "Internal typechecker form used out of context" stx)) ...))

(internal-forms internal-literals
 require/typed-internal
 define-type-alias-internal
 define-type-internal
 define-typed-struct-internal
 define-typed-struct/exec-internal
 assert-predicate-internal
 declare-refinement-internal
 :-internal
 typecheck-fail-internal)



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
    (declare-refinement-internal predicate)]
  [typed-struct
    (define-typed-struct-internal . :define-typed-struct-body)]
  [typed-struct/exec
    (define-typed-struct/exec-internal nm ([fields:id : types] ...) proc-type)]
  [typed-require
    (require/typed-internal name type)]
  [typed-require/struct
    (require/typed-internal name type #:struct-maker parent)]
  [predicate-assertion
    (assert-predicate-internal type predicate)]
  [type-declaration
    (:-internal id:identifier type)]
  [typecheck-failure
    (typecheck-fail-internal stx message:str var:id)])

;;; Internal form creation
(begin-for-syntax
  (define (internal stx)
    (quasisyntax/loc stx
      (define-values ()
        (begin
          (quote-syntax #,stx)
          (#%plain-app values))))))
