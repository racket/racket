#lang racket/base

;; Definitions with contracts and contract documentation.

(require racket/contract
         (for-syntax racket/base racket/list syntax/parse
                     "serialize-syntax.rkt")
         (prefix-in s. scribble/manual)
         (prefix-in s. scribble/core)
         (prefix-in s. scribble/html-properties))

(provide defproc defparam defcontract doc-apply)

(begin-for-syntax
  (struct proc+doc (proc-transformer doc-transformer)
    #:property prop:procedure (λ (p stx) ((proc+doc-proc-transformer p) stx)))
  
  (define-syntax-class argument-spec
    #:description "argument specification"
    (pattern [name:id contract:expr])
    (pattern [name:id contract:expr default:expr])
    (pattern [kw:keyword name:id contract:expr])
    (pattern [kw:keyword name:id contract:expr default:expr]))
  )

(define-for-syntax (make-proc+doc id-stx doc-transformer)
  (proc+doc
   (λ (stx)
     (syntax-case stx ()
       [(_ . args)  (quasisyntax/loc stx (#,id-stx . args))]
       [_           (quasisyntax/loc stx #,id-stx)]))
   doc-transformer))

;; Applies the documentation transformer (use within a scribble/manual module)
(define-syntax (doc-apply stx)
  (define (error) (raise-syntax-error 'doc-apply "no associated doc transformer" stx))
  (syntax-parse stx
    [(_ name:id . pre-flows)
     (define p (syntax-local-value #'name error))
     (when (not (proc+doc? p)) (error))
     ((proc+doc-doc-transformer p) (syntax/loc stx (name . pre-flows)))]))


;; A define-with-value form for scribble documentation
(define (def/value def val . pre-flows)
  (apply s.nested (s.tabular #:style def/value-table-style
                             (list (list (s.nested def) 'cont)
                                   (list "=" val)))
         pre-flows))

(define def/value-table-style
  (s.style 'boxed
           (list (s.table-columns
                  (list (s.style 'plain (list 'top (s.attributes '((width . "0%")))))
                        (s.style 'plain (list 'top 'left (s.attributes '((width . "100%"))))))))))

;; ===================================================================================================
;; Helpers

(define-for-syntax (get-required-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr)             (list #'contract)]
    [(kw:keyword name:id contract:expr)  (list #'kw #'contract)]
    [_  empty]))

(define-for-syntax (get-optional-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr default:expr)             (list #'contract)]
    [(kw:keyword name:id contract:expr default:expr)  (list #'kw #'contract)]
    [_  empty]))

(define-for-syntax (remove-contract arg-stx)
  (syntax-parse arg-stx
    [(name:id contract:expr)                          (list #'name)]
    [(name:id contract:expr default:expr)             (list #'(name default))]
    [(kw:keyword name:id contract:expr)               (list #'kw #'name)]
    [(kw:keyword name:id contract:expr default:expr)  (list #'kw #'(name default))]))

(define-for-syntax (parameter-name->arg-name name-stx)
  (define name-str (symbol->string (syntax->datum name-stx)))
  (define arg-name-str
    (cond [(regexp-match #rx".*-(.*)$" name-str)  => (λ (m) (last m))]
          [(regexp-match #rx"^$" name-str)        => (λ (m) "value")]
          [else  (substring name-str 0 1)]))
  (datum->syntax name-stx (string->symbol arg-name-str)))

(define-for-syntax (make-value-name id-stx)
  (datum->syntax #f (syntax->datum id-stx)))

(define-for-syntax (make-doc-name ctx id-stx)
  (datum->syntax ctx (syntax->datum id-stx)))

;; ===================================================================================================
;; define-with-contract+doc forms

;; Define a procedure
(define-syntax (defproc stx)
  (syntax-parse stx
    [(_ (name:id arg:argument-spec ...) result:expr body ...+)
     (define arg-list (syntax->list #'(arg ...)))
     (with-syntax ([value-name          (make-value-name #'name)]
                   [(new-arg ...)       (append* (map remove-contract arg-list))]
                   [(req-contract ...)  (append* (map get-required-contract arg-list))]
                   [(opt-contract ...)  (append* (map get-optional-contract arg-list))]
                   [serialized-args     (serialize-syntax #'(arg ...))]
                   [serialized-result   (serialize-syntax #'result)])
       (syntax/loc stx
         (begin
           (define/contract (value-name new-arg ...) (->* (req-contract ...) (opt-contract ...)
                                                             result)
             body ...)
           (define-syntax name
             (make-proc+doc
              #'value-name
              (λ (doc-stx)
                (syntax-case doc-stx ()
                  [(ctx . pre-flows)
                   (with-syntax ([doc-name    (make-doc-name #'ctx #'name)]
                                 [doc-args    (unserialize-syntax #'ctx 'serialized-args)]
                                 [doc-result  (unserialize-syntax #'ctx 'serialized-result)])
                     #'(s.defproc (doc-name . doc-args) doc-result . pre-flows))])))))))]))

;; Define a parameter
(define-syntax (defparam stx)
  (syntax-parse stx
    [(_ name:id arg:id contract:expr default:expr)
     (with-syntax ([value-name           (make-value-name #'name)]
                   [serialized-contract  (serialize-syntax #'contract)]
                   [serialized-default   (serialize-syntax #'default)])
       (syntax/loc stx
         (begin
           (define/contract value-name (parameter/c contract) (make-parameter default))
           (define-syntax name
             (make-proc+doc
              #'value-name
              (λ (doc-stx)
                (syntax-case doc-stx ()
                  [(ctx . pre-flows)
                   (with-syntax ([doc-name      (make-doc-name #'ctx #'name)]
                                 [doc-arg       (make-doc-name #'ctx #'arg)]
                                 [doc-contract  (unserialize-syntax #'ctx 'serialized-contract)]
                                 [doc-default   (unserialize-syntax #'ctx 'serialized-default)])
                     #'(def/value
                         (s.defparam doc-name doc-arg doc-contract)
                         (s.racketblock doc-default)
                         . pre-flows))])))))))]
    [(_ name:id contract:expr default:expr)
     (quasisyntax/loc stx
       (defparam name #,(parameter-name->arg-name #'name) contract default))]))

;; Define a contract or a procedure that returns a contract
(define-syntax (defcontract stx)
  (syntax-parse stx
    [(_ name:id value:expr)
     (with-syntax ([value-name        (make-value-name #'name)]
                   [serialized-value  (serialize-syntax #'value)])
       (syntax/loc stx
         (begin
           (define value-name value)
           (define-syntax name
             (make-proc+doc
              #'value-name
              (λ (doc-stx)
                (syntax-case doc-stx ()
                  [(ctx . pre-flows)
                   (with-syntax ([doc-name       (make-doc-name #'ctx #'name)]
                                 [doc-contract?  (make-doc-name #'ctx #'contract?)]
                                 [doc-value      (unserialize-syntax #'ctx 'serialized-value)])
                     #'(def/value
                         (s.defthing doc-name doc-contract?)
                         (s.racketblock doc-value)
                         . pre-flows))])))))))]
    [(_ (name:id arg:argument-spec ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (with-syntax ([value-name          (make-value-name #'name)]
                   [(new-arg ...)       (append* (map remove-contract arg-list))]
                   [(req-contract ...)  (append* (map get-required-contract arg-list))]
                   [(opt-contract ...)  (append* (map get-optional-contract arg-list))]
                   [serialized-args     (serialize-syntax #'(arg ...))]
                   [serialized-body     (serialize-syntax #'body)])
       (syntax/loc stx
         (begin
           (define/contract (value-name new-arg ...) (->* (req-contract ...) (opt-contract ...)
                                                             contract?)
             body)
           (define-syntax name
             (make-proc+doc
              #'value-name
              (λ (doc-stx)
                (syntax-case doc-stx (doc)
                  [(ctx . pre-flows)
                   (with-syntax ([doc-name       (make-doc-name #'ctx #'name)]
                                 [doc-contract?  (make-doc-name #'ctx #'contract?)]
                                 [doc-args       (unserialize-syntax #'ctx 'serialized-args)]
                                 [doc-body       (unserialize-syntax #'ctx 'serialized-body)])
                     #'(def/value
                         (s.defproc (doc-name . doc-args) doc-contract?)
                         (s.racketblock doc-body)
                         . pre-flows))])))))))]))
