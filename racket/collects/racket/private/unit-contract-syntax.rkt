#lang racket/base

(require syntax/parse
         "unit-compiletime.rkt"
         "unit-keywords.rkt"
         racket/contract
         (for-template "unit-keywords.rkt" racket/base racket/contract))

(provide import-clause/contract export-clause/contract body-clause/contract dep-clause
         import-clause/c export-clause/c body-clause/c)

(define-syntax-class sig-spec #:literals (prefix rename only except)
  #:attributes ((name 0))
  #:transparent
  (pattern name:identifier)
  (pattern (prefix i:identifier s:sig-spec)
           #:with name #'s.name)
  (pattern (rename s:sig-spec [int:identifier ext:identifier] ...)
           #:with name #'s.name)
  (pattern (only s:sig-spec i:identifier ...)
           #:with name #'s.name)
  (pattern (except s:sig-spec i:identifier ...)
           #:with name #'s.name))

(define-syntax-class tagged-sig-spec #:literals (tag)
  #:auto-nested-attributes
  #:transparent
  (pattern s:sig-spec
           #:with i #f)
  (pattern (tag i:identifier s:sig-spec)))

(define-syntax-class tagged-sig-id #:literals (tag)
  #:attributes ()
  #:transparent
  (pattern s:identifier)
  (pattern (tag i:identifier s)))

(define-syntax-class unit/c-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-id [x:identifier c:expr] ...))
  (pattern s:tagged-sig-id ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/c #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/c-clause ...)))
(define-syntax-class export-clause/c #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/c-clause ...)))

;; Helper to reduce the size of unit contract expansion
;; This has to be defined in a module in order for
;; `unit/c-check-invoke-values` to be defined at the
;; correct phase during expansion when the body-clause/c
;; syntax class is parsed
(module unit-check-values racket/base
  (provide unit/c-check-invoke-values)
  (require racket/contract/base
           racket/contract/combinator)
  (define ((unit/c-check-invoke-values len blame ctcs) . args)
    (define args-len (length args))
    (unless (= len args-len)
      (raise-blame-error (blame-add-context blame "the body of")
                         (blame-value blame)
                         (format "expected ~a values, returned ~a" len args-len)))
    (apply values 
           (map 
            (lambda (ctc arg) (ctc arg))
            ctcs args))))

(require (for-template 'unit-check-values))
(define-splicing-syntax-class body-clause/c
  #:literals (values)
  #:auto-nested-attributes
  #:transparent
  (pattern (~seq)
           #:attr name #'()
           #:attr make-define-ctcs/blame
           (lambda (name blame) #'())
           #:attr apply-invoke-ctcs
           (lambda (id blame ctcs) id))
  (pattern (values ctc:expr ...)
           #:attr name #'('(values ctc ...))
           #:attr make-define-ctcs/blame
           (lambda (name blame)
             #`((define #,name
                  (map (lambda (c) ((contract-projection c)
                                    (blame-add-context #,blame "the body of")))
                       (list ctc ...)))))
           #:attr apply-invoke-ctcs
           ;; blame here is really syntax representing a blame object
           (lambda (id blame ctcs)
             (define len (length (syntax->list #'(ctc ...))))
             #`(call-with-values
                (lambda () #,id)
                (unit/c-check-invoke-values #,len #,blame #,ctcs))))
  (pattern b:expr
           #:attr name #'('b)
           #:attr make-define-ctcs/blame
           (lambda (name blame)
             #`((define #,name ((contract-projection b)
                                (blame-add-context #,blame "the body of")))))
           #:attr apply-invoke-ctcs
           (lambda (id blame ctcs)
             #`(#,ctcs #,id))))

(define-syntax-class unit/contract-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-spec [x:identifier c:expr] ...))
  (pattern s:tagged-sig-spec ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/contract #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/contract-clause ...)))
(define-syntax-class export-clause/contract #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/contract-clause ...)))
(define-syntax-class dep-clause #:literals (init-depend)
  #:auto-nested-attributes
  #:transparent
  (pattern (init-depend s:tagged-sig-id ...)))
(define-splicing-syntax-class body-clause/contract
  #:auto-nested-attributes
  #:transparent
  (pattern (~seq #:invoke/contract b:expr)))
