#lang racket/base

(provide contract
         (rename-out [-recursive-contract recursive-contract])
         current-contract-region
         invariant-assertion
         (for-syntax lifted-key add-lifted-property))

(require (for-syntax racket/base syntax/name syntax/srcloc)
         racket/stxparam
         syntax/srcloc
         syntax/location
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "generate.rkt")

(begin-for-syntax
 (define lifted-key (gensym 'contract:lifted))
 ;; syntax? -> syntax?
 ;; tells clients that the expression is a lifted application
 (define (add-lifted-property stx)
   (syntax-property stx lifted-key #t)))

(define-for-syntax lifted-ccrs (make-hasheq))

(define-syntax-parameter current-contract-region
  (λ (stx)
     (if (eq? (syntax-local-context) 'expression)
         (let* ([ctxt (syntax-local-lift-context)]
                [id (hash-ref lifted-ccrs ctxt #f)])
           (with-syntax ([id (or id
                                 (let ([id (syntax-local-lift-expression 
                                            (add-lifted-property
                                             (syntax/loc stx (quote-module-name))))])
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
    (define clnp (contract-late-neg-projection c))
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
                  
                  (if clnp #f neg)
                  #t))
    (cond
      [clnp (with-contract-continuation-mark
             (cons blame neg)
             ((clnp blame) v neg))]
      [else (with-contract-continuation-mark
             blame
             (((contract-projection c) blame) v))])))

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
  (define (do-recursive-contract arg type name list-contract?)
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
    #`(#,maker '#,name (λ () #,arg) '#,local-name #,list-contract?))
  (syntax-case stx ()
    [(_ arg type #:list-contract?)
     (keyword? (syntax-e #'type))
     (do-recursive-contract #'arg #'type #'(recursive-contract arg type) #t)]
    [(_ arg #:list-contract?)
     (do-recursive-contract #'arg #'#:impersonator #'(recursive-contract arg) #t)]
    [(_ arg type)
     (keyword? (syntax-e #'type))
     (do-recursive-contract #'arg #'type #'(recursive-contract arg type) #f)]
    [(_ arg)
     (do-recursive-contract #'arg #'#:impersonator #'(recursive-contract arg) #f)]))

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
     (when (recursive-contract-list-contract? ctc)
       (unless (list-contract? forced-ctc)
         (raise-argument-error 'recursive-contract "list-contract?" forced-ctc)))
     (set-recursive-contract-ctc! ctc forced-ctc)
     (set-recursive-contract-name! ctc (append `(recursive-contract ,(contract-name forced-ctc))
                                               (cddr old-name)))
     forced-ctc]
    [else current]))

(define (recursive-contract-late-neg-projection ctc)
  (cond
    [(recursive-contract-list-contract? ctc)
     (λ (blame)
       (define r-ctc (force-recursive-contract ctc))
       (define f (get/build-late-neg-projection r-ctc))
       (define blame-known (blame-add-context blame #f))
       (λ (val neg-party)
         (unless (list? val)
           (raise-blame-error blame-known #:missing-party neg-party
                              val
                              '(expected: "list?" given: "~e")
                              val))
         ((f blame-known) val neg-party)))]
    [else
     (λ (blame)
       (define r-ctc (force-recursive-contract ctc))
       (define f (get/build-late-neg-projection r-ctc))
       (define blame-known (blame-add-context blame #f))
       (λ (val neg-party)
         ((f blame-known) val neg-party)))]))
  
(define (recursive-contract-stronger this that) (equal? this that))

(define ((recursive-contract-first-order ctc) val)
  (cond
    [(contract-first-order-okay-to-give-up?) #t]
    [else (contract-first-order-try-less-hard
           (contract-first-order-passes? (force-recursive-contract ctc)
                                         val))]))

(define (recursive-contract-generate ctc)
  (λ (fuel)
    (cond
      [(zero? fuel) #f]
      [else
       (force-recursive-contract ctc)
       (contract-random-generate/choose (recursive-contract-ctc ctc) (- fuel 1))])))

(struct recursive-contract ([name #:mutable] [thunk #:mutable] [ctc #:mutable] list-contract?)
  #:property prop:recursive-contract (λ (this)
                                       (force-recursive-contract this)
                                       (recursive-contract-ctc this)))

(struct flat-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection recursive-contract-late-neg-projection
   #:stronger recursive-contract-stronger
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
(struct chaperone-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection recursive-contract-late-neg-projection
   #:stronger recursive-contract-stronger
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
(struct impersonator-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection recursive-contract-late-neg-projection
   #:stronger recursive-contract-stronger
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
