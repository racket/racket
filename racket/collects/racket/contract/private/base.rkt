#lang racket/base

(provide contract
         (rename-out [-recursive-contract recursive-contract])
         current-contract-region
         invariant-assertion)

(require (for-syntax racket/base syntax/name syntax/srcloc)
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
                                 (let ([id (syntax-local-lift-expression 
                                            (syntax/loc stx (quote-module-name)))])
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
    (define cvfp (contract-val-first-projection c))
    (define blame
      (make-blame (build-source-location loc)
                  name
                  (λ () (contract-name c))
                  
                  ;; hack! We need to allow pos = #f for backwards
                  ;; compatibility, but we cannot put #f into the
                  ;; blame struct now because #f means that the 
                  ;; name is not known. Since #f is not a very good
                  ;; name, we'll just put something stupid here 
                  ;; instead of changing the library around.
                  (or pos "false")
                                  
                  (if cvfp #f neg)
                  #t))
    (define new-val
      (cond
        [cvfp (((cvfp blame) v) neg)]
        [else (((contract-projection c) blame) v)]))
    (cond
      [(and (not (parameter? new-val))  ;; when PR 11221 is fixed, remove this line
            (procedure? new-val)
            (object-name v)
            (not (eq? (object-name v) (object-name new-val))))
       (define vs-name (object-name v))
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
          (procedure-rename new-val vs-name)])]
      [else new-val])))

(define-syntax (invariant-assertion stx)
  (syntax-case stx ()
    [(_ ctc e)
     (quasisyntax/loc stx
       (let ([me (quote-module-name)])
         (contract ctc e
                   me me
                   '#,(syntax-local-infer-name stx)
                   '#,(build-source-location-vector #'ctc))))]))

(define-syntax (-recursive-contract stx)
  (define (do-recursive-contract arg type name)
    (define local-name (syntax-local-infer-name stx))
    (define maker
      (case (syntax-e type)
        [(#:impersonator) #'impersonator-recursive-contract]
        [(#:chaperone) #'chaperone-recursive-contract]
        [(#:flat) #'flat-recursive-contract]
        [else (raise-syntax-error 
               'recursive-contract
               "type must be one of #:impersonator, #:chaperone, or #:flat"
               stx
               type)]))
    #`(#,maker '#,name (λ () #,arg) '#,local-name
               'uninitialized-non-cyclic-first-order
               'uninitialized-rec-proj-field))
  (syntax-case stx ()
    [(_ arg type)
     (keyword? (syntax-e #'type))
     (do-recursive-contract #'arg #'type #'(recursive-contract arg type))]
    [(_ arg)
     (do-recursive-contract #'arg #'#:impersonator #'(recursive-contract arg))]))

(define (force-recursive-contract ctc)
  (define current (recursive-contract-ctc ctc))
  (when (or (symbol? current) (not current))
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
    (define cm-key (box #f))
    (define orig-projection (contract-projection forced-ctc))
    (define ((wrapper-projection blame) val)
      (cond
        [(continuation-mark-set-first #f cm-key)
         =>
         (λ (ht)
           (cond
             [(hash-ref ht val #f)
              (raise-blame-error blame val '(given: "a value with a cycle"))]
             [else
              (hash-set! ht val #t)
              ((orig-projection blame) val)]))]
        [else
         (with-continuation-mark cm-key (make-hasheq)
           ((orig-projection blame) val))]))
    (define orig-first-order (contract-first-order forced-ctc))
    (define (wrapper-first-order val)
      (cond
        [(continuation-mark-set-first #f cm-key)
         =>
         (λ (ht)
           (cond
             [(hash-ref ht val #f) #f]
             [else
              (hash-set! ht val #t)
              (orig-first-order val)]))]
        [else
         (with-continuation-mark cm-key (make-hasheq)
           (orig-first-order val))]))
    (set-recursive-contract-ctc! ctc forced-ctc)
    (set-recursive-contract-non-cyclic-projection! ctc wrapper-projection)
    (set-recursive-contract-non-cyclic-first-order! ctc wrapper-first-order)
    (set-recursive-contract-name! ctc (append `(recursive-contract ,(contract-name forced-ctc))
                                              (cddr old-name)))))

(define ((recursive-contract-projection ctc) blame)
  (force-recursive-contract ctc)
  (define f (recursive-contract-non-cyclic-projection ctc))
  (define blame-known (blame-add-context blame #f))
  (λ (val)
    ((f blame-known) val)))

(define (recursive-contract-stronger this that)
  (and (recursive-contract? that)
       (procedure-closure-contents-eq? (recursive-contract-thunk this)
                                       (recursive-contract-thunk that))))

(define ((recursive-contract-first-order ctc) val)
  (force-recursive-contract ctc)
  ((recursive-contract-non-cyclic-first-order ctc) val))

(struct recursive-contract ([name #:mutable]
                            thunk
                            [ctc #:mutable]
                            [non-cyclic-first-order #:mutable]
                            [non-cyclic-projection #:mutable]))

(struct flat-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))
(struct chaperone-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))
(struct impersonator-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:projection recursive-contract-projection
   #:stronger recursive-contract-stronger))
