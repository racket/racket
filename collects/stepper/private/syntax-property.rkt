#lang racket/base

(require (for-syntax racket/base))

(provide stepper-syntax-property
         with-stepper-syntax-properties
         
         skipto/cdr
         skipto/cddr
         skipto/first
         skipto/second
         skipto/third
         skipto/fourth
         skipto/firstarg)


;; stepper-syntax-property : like syntax property, but adds properties to an association
;; list associated with the syntax property 'stepper-properties

(define stepper-syntax-property
  (case-lambda 
    [(stx tag)
     (unless (member tag known-stepper-syntax-property-names)
       (raise-type-error 'stepper-syntax-property
                         "known stepper property symbol" 1 stx tag))
     (let ([stepper-props (syntax-property stx 'stepper-properties)])
       (if stepper-props
           (let ([table-lookup (assq tag stepper-props)])
             (if table-lookup
                 (cadr table-lookup)
                 #f))
           #f))]
    [(stx tag new-val) 
     (unless (member tag known-stepper-syntax-property-names)
       (raise-type-error 'stepper-syntax-property
                         "known stepper property symbol" 1 
                         stx tag new-val))
     (syntax-property stx 'stepper-properties
                      (cons (list tag new-val)
                            (or (syntax-property stx 'stepper-properties)
                                null)))]))



;; if the given property name isn't in this list, signal an error...
(define known-stepper-syntax-property-names 
  '(stepper-skip-completely
    stepper-hint
    stepper-define-type
    stepper-xml-hint
    stepper-xml-value-hint
    stepper-proc-define-name
    stepper-orig-name
    stepper-prim-name
    stepper-binding-type
    stepper-no-lifting-info
    stepper-and/or-clauses-consumed
    stepper-skipto
    stepper-skipto/discard
    stepper-replace
    stepper-else
    stepper-black-box-expr
    stepper-test-suite-hint
    stepper-highlight
    stepper-fake-exp
    stepper-args-of-call
    stepper-hide-completed
    stepper-hide-reduction
    stepper-use-val-as-final
    stepper-lifted-name
    lazy-op
    ))


;; with-stepper-syntax-properties : like stepper-syntax-property, 
;; but in a "let"-like form
(define-syntax (with-stepper-syntax-properties stx)
  (syntax-case stx ()
    [(_ ([property val] ...) body)
     (foldl (lambda (property val b)
              #`(stepper-syntax-property #,b #,property #,val))
            #'body
            (syntax->list #`(property ...))
            (syntax->list #`(val ...)))]))


;; commonly used values for stepper-syntax-property:
(define skipto/cdr `(syntax-e cdr))
(define skipto/cddr `(syntax-e cdr cdr))
(define skipto/first `(syntax-e car))
(define skipto/second `(syntax-e cdr car))
(define skipto/third `(syntax-e cdr cdr car))
(define skipto/fourth `(syntax-e cdr cdr cdr car))
(define skipto/firstarg (append skipto/cdr skipto/second))
