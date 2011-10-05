#lang racket/base

;; Definitions with contracts and contract documentation.

(require racket/contract
         (for-syntax racket/base racket/list syntax/parse racket/syntax syntax/strip-context
                     racket/vector)
         (prefix-in s. scribble/manual)
         (prefix-in s. scribble/core)
         (prefix-in s. scribble/html-properties))

(provide defproc defparam defcontract doc-apply)

(begin-for-syntax
  (struct proc+doc (proc-transformer doc-transformer)
    #:transparent
    #:property prop:procedure
    (λ (t stx)
      ((proc+doc-proc-transformer t) stx)))
  
  (define-syntax-class argument-spec
    #:description "argument specification"
    (pattern [name:id contract:expr])
    (pattern [name:id contract:expr default:expr])
    (pattern [kw:keyword name:id contract:expr])
    (pattern [kw:keyword name:id contract:expr default:expr]))
  )

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

;; Applies the documentation transformer (use within a scribble/manual module)
(define-syntax (doc-apply stx)
  (syntax-parse stx
    [(_ name:id . pre-flows)
     (let ([t  (syntax-local-value #'name)])
       ((proc+doc-doc-transformer t) (syntax/loc stx (name . pre-flows))))]))

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
  (format-id name-stx "~a" arg-name-str))

;; ===================================================================================================
;; define-with-contract+doc forms

;; Define a procedure
(define-syntax (defproc stx)
  (syntax-parse stx
    [(_ (name:id arg:argument-spec ...) result-contract:expr body ...+)
     (define arg-list (syntax->list #'(arg ...)))
     (define/with-syntax proc-name (strip-context #'name))
     (define/with-syntax (new-arg ...) (append* (map remove-contract arg-list)))
     (define/with-syntax (req-contract ...) (append* (map get-required-contract arg-list)))
     (define/with-syntax (opt-contract ...) (append* (map get-optional-contract arg-list)))
     (syntax/loc stx
       (begin
         (define/contract (proc-name new-arg ...) (->* (req-contract ...) (opt-contract ...)
                                                       result-contract)
           body ...)
         (define-syntax name
           (proc+doc
            (λ (app-stx)
              (syntax-case app-stx ()
                [(_ . args) (syntax/loc app-stx (proc-name . args))]
                [_          (syntax/loc app-stx proc-name)]))
            (λ (doc-stx)
              (syntax-case doc-stx ()
                [(the-name . pre-flows)
                 (with-syntax ([doc-name             (replace-context #'the-name #'name)]
                               [doc-args             (replace-context #'the-name #'(arg ...))]
                               [doc-result-contract  (replace-context #'the-name #'result-contract)])
                   #'(s.defproc (doc-name . doc-args) doc-result-contract . pre-flows))]))))))]))

;; Define a parameter
(define-syntax (defparam stx)
  (syntax-parse stx
    [(_ name:id arg:id contract:expr default:expr)
     (define/with-syntax proc-name (strip-context #'name))
     (syntax/loc stx
       (begin
         (define/contract proc-name (parameter/c contract) (make-parameter default))
         (define-syntax name
           (proc+doc
            (λ (app-stx)
              (syntax-case app-stx ()
                [(_ . args)  (syntax/loc app-stx (proc-name . args))]
                [_           (syntax/loc app-stx proc-name)]))
            (λ (doc-stx)
              (syntax-case doc-stx ()
                [(the-name . pre-flows)
                 (with-syntax ([doc-name      (replace-context #'the-name #'name)]
                               [doc-arg       (replace-context #'the-name #'arg)]
                               [doc-contract  (replace-context #'the-name #'contract)]
                               [doc-default   (replace-context #'the-name #'default)])
                   (syntax/loc doc-stx
                     (def/value
                       (s.defparam doc-name doc-arg doc-contract)
                       (s.racketblock doc-default)
                       . pre-flows)))]))))))]
    [(_ name:id contract:expr default:expr)
     (define/with-syntax arg-name (parameter-name->arg-name #'name))
     (syntax/loc stx (defparam name arg-name contract default))]))

;; Define a contract or a procedure that returns a contract
(define-syntax (defcontract stx)
  (syntax-parse stx
    [(_ name:id value:expr)
     (define/with-syntax contract-name (strip-context #'name))
     (syntax/loc stx
       (begin
         (define contract-name value)
         (define-syntax name
           (proc+doc
            (λ (app-stx)
              (syntax-case app-stx ()
                [(_ . args)  (syntax/loc app-stx (contract-name . args))]
                [_           (syntax/loc app-stx contract-name)]))
            (λ (doc-stx)
              (syntax-case doc-stx ()
                [(the-name . pre-flows)
                 (with-syntax ([doc-name       (replace-context #'the-name #'name)]
                               [doc-contract?  (replace-context #'the-name #'contract?)]
                               [doc-value      (replace-context #'the-name #'value)])
                   (syntax/loc doc-stx
                     (def/value
                       (s.defthing doc-name doc-contract?)
                       (s.racketblock doc-value)
                       . pre-flows)))]))))))]
    [(_ (name:id arg:argument-spec ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (define/with-syntax proc-name (strip-context #'name))
     (define/with-syntax (new-arg ...) (append* (map remove-contract arg-list)))
     (define/with-syntax (req-contract ...) (append* (map get-required-contract arg-list)))
     (define/with-syntax (opt-contract ...) (append* (map get-optional-contract arg-list)))
     (syntax/loc stx
       (begin
         (define/contract (proc-name new-arg ...) (->* (req-contract ...) (opt-contract ...)
                                                       contract?)
           body)
         (define-syntax name
           (proc+doc
            (λ (app-stx)
              (syntax-case app-stx ()
                [(_ . args) (syntax/loc app-stx (proc-name . args))]
                [_          (syntax/loc app-stx proc-name)]))
            (λ (doc-stx)
              (syntax-case doc-stx ()
                [(the-name . pre-flows)
                 (with-syntax ([doc-name       (replace-context #'the-name #'name)]
                               [doc-args       (replace-context #'the-name #'(arg ...))]
                               [doc-contract?  (replace-context #'the-name #'contract?)]
                               [doc-body       (replace-context #'the-name #'body)])
                   (syntax/loc doc-stx
                     (def/value
                       (s.defproc (doc-name . doc-args) doc-contract?)
                       (s.racketblock doc-body)
                      . pre-flows)))]))))))]))
