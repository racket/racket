#lang racket/base

(provide contract
         (rename-out [-recursive-contract recursive-contract])
         current-contract-region)

(require (for-syntax racket/base syntax/name)
         racket/stxparam
         syntax/srcloc
         syntax/location
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "arrow.rkt"
         "misc.rkt")

(define-for-syntax lifted-ccrs (make-hasheq))

(define-syntax-parameter current-contract-region
  (λ (stx)
     (if (eq? (syntax-local-context) 'expression)
         (let* ([ctxt (syntax-local-lift-context)]
                [id (hash-ref lifted-ccrs ctxt #f)])
           (with-syntax ([id (or id
                                 (let ([id (syntax-local-lift-expression (syntax/loc stx (quote-module-name)))])
                                   (hash-set! lifted-ccrs ctxt (syntax-local-introduce id))
                                   id))])
             #'id))
         (quasisyntax/loc stx (#%expression #,stx)))))

(define-syntax (contract stx)
  (syntax-case stx ()
    [(_ c v pos neg name loc)
     (syntax/loc stx
       (apply-contract c v pos neg name loc))]
    [(_ c v pos neg)
     (with-syntax ([name (syntax-local-infer-name stx)])
      (syntax/loc stx
        (apply-contract c v pos neg 'name
                        (build-source-location #f))))]
    [(_ c v pos neg src)
     (raise-syntax-error 'contract
       (string-append
        "please update contract application to new protocol "
        "(either 4 or 6 arguments)"))]))

(define (apply-contract c v pos neg name loc)
  (let ([c (coerce-contract 'contract c)])
    (check-source-location! 'contract loc)
    (let ([new-val
           (((contract-projection c)
             (make-blame (build-source-location loc) name (λ () (contract-name c)) pos neg #t))
            v)])
      (if (and (not (parameter? new-val))  ;; when PR 11221 is fixed, remove this line
               (procedure? new-val)
               (object-name v)
               (not (eq? (object-name v) (object-name new-val))))
          (let ([vs-name (object-name v)])
            (cond
              [(contracted-function? new-val)
               ;; when PR11222 is fixed, change these things:
               ;;   - eliminate this cond case
               ;;   - remove the require of arrow.rkt above
               ;;   - change (struct-out contracted-function) 
               ;;     in arrow.rkt to make-contracted-function
               (make-contracted-function 
                (procedure-rename (contracted-function-proc new-val) vs-name)
                (contracted-function-ctc new-val))]
              [else
               (procedure-rename new-val vs-name)]))
          new-val))))

(define-syntax (-recursive-contract stx)
  (define (do-recursive-contract arg type name)
    (define local-name (syntax-local-infer-name stx))
    (with-syntax ([maker
                   (case (syntax-e type)
                     [(#:impersonator) #'impersonator-recursive-contract]
                     [(#:chaperone) #'chaperone-recursive-contract]
                     [(#:flat) #'flat-recursive-contract]
                     [else (raise-syntax-error 'recursive-contract
                                               "type must be one of #:impersonator, #:chaperone, or #:flat"
                                               stx
                                               type)])])
      #`(maker '#,name (λ () #,arg) '#,local-name)))
  (syntax-case stx ()
    [(_ arg type)
     (keyword? (syntax-e #'type))
     (do-recursive-contract #'arg #'type #'(recursive-contract arg type))]
    [(_ arg)
     (do-recursive-contract #'arg #'#:impersonator #'(recursive-contract arg))]))

(define (force-recursive-contract ctc)
  (define current (recursive-contract-ctc ctc))
  (cond
    [(or (symbol? current) (not current))
     (define thunk (recursive-contract-thunk ctc))
     (define old-name (recursive-contract-name ctc))
     (set-recursive-contract-name! ctc (or current '<recursive-contract>))
     (define forced-ctc
       (cond
         [(flat-recursive-contract? ctc)
          (coerce-flat-contract 'recursive-contract (thunk))]
         [(chaperone-recursive-contract? ctc)
          (coerce-chaperone-contract 'recursive-contract (thunk))]
         [(impersonator-recursive-contract? ctc)
          (coerce-contract 'recursive-contract (thunk))]))
     (set-recursive-contract-ctc! ctc forced-ctc)
     (set-recursive-contract-name! ctc (append `(recursive-contract ,(contract-name forced-ctc))
                                               (cddr old-name)))
     forced-ctc]
    [else current]))

(define ((recursive-contract-projection ctc) blame)
  (define r-ctc (force-recursive-contract ctc))
  (define f (contract-projection r-ctc))
  (define blame-known (blame-add-context blame #f))
  (λ (val)
    ((f blame-known) val)))

(define (recursive-contract-stronger this that)
  (and (recursive-contract? that)
       (procedure-closure-contents-eq? (recursive-contract-thunk this)
                                       (recursive-contract-thunk that))))

(define ((recursive-contract-first-order ctc) val)
  (contract-first-order-passes? (force-recursive-contract ctc)
                                val))

(struct recursive-contract ([name #:mutable] thunk [ctc #:mutable]))

(struct flat-recursive-contract recursive-contract ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))
(struct chaperone-recursive-contract recursive-contract ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))
(struct impersonator-recursive-contract recursive-contract ()
  #:property prop:contract
  (build-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))