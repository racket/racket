#lang racket/base

(provide contract
         make-apply-contract
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

  (let ([l (syntax->list stx)])
    (when l
      (for ([thing (in-list (cdr (syntax->list stx)))])
        (when (keyword? (syntax-e thing))
          (unless (equal? (syntax-e thing) '#:limit-context)
            (raise-syntax-error 'contract
                                (format "did not expect keyword ~a" (syntax-e thing))
                                stx
                                thing))))))
  
  (syntax-case stx ()
    [(_ c v pos neg #:limit-context limit-context-expression)
     (with-syntax ([name (syntax-local-infer-name stx)])
       (syntax/loc stx
         (apply-contract c v pos neg 'name
                         (build-source-location #f)
                         limit-context-expression)))]
    [(_ c v pos neg name loc)
     (syntax/loc stx
       (apply-contract c v pos neg name loc #f))]
    [(_ c v pos neg)
     (with-syntax ([name (syntax-local-infer-name stx)])
       (syntax/loc stx
         (apply-contract c v pos neg 'name
                         (build-source-location #f)
                         #f)))]))

(define (apply-contract c v pos neg name loc context-limit)
  ((make-apply-contract c pos neg name loc context-limit) v))

(define (make-apply-contract c pos neg name loc context-limit [backwards? #f])
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
                  #t
                  #:context-limit context-limit))
    (define ccm-value (if clnp (cons blame neg) blame))
    (define-syntax-rule (with-ccm e)
      (with-contract-continuation-mark ccm-value e))
    (cond
      [clnp
       (define proj (with-ccm (clnp blame)))
       (lambda (v) (with-ccm (proj v neg)))]
      [else
       (define proj (with-ccm ((contract-projection c) blame)))
       (lambda (v) (with-ccm (proj v)))])))

(define-syntax (invariant-assertion stx)
  (syntax-case stx ()
    [(_ ctc e)
     (quasisyntax/loc stx
       (contract ctc e
                 invariant-assertion-party invariant-assertion-party
                 '#,(syntax-local-infer-name stx)
                 '#,(build-source-location-vector #'ctc)))]))

(define-syntax (-recursive-contract stx)
  (define (parse-type/kwds arg type kwds)
    (define list-contract? #f)
    (define extra-delay? #f)
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
    (let loop ([kwds kwds])
      (syntax-case kwds ()
        [() (void)]
        [(kwd . rest)
         (unless (keyword? (syntax-e #'kwd))
           (raise-syntax-error 'recursive-contract
                               "expected either #:list-contract? or #:extra-delay"
                               stx
                               (car kwds)))
         (case (syntax-e #'kwd)
           [(#:list-contract?)
            (when list-contract?
              (raise-syntax-error 'recursive-contract
                                  "#:list-contract? keyword appeared twice"
                                  stx
                                  list-contract?
                                  (list #'kwd)))
            (set! list-contract? #'kwd)
            (loop #'rest)]
           [(#:extra-delay)
            (when extra-delay?
              (raise-syntax-error 'recursive-contract
                                  "#:extra-delay keyword appeared twice"
                                  stx
                                  extra-delay?
                                  (list #'kwd)))
            (set! extra-delay? #'kwd)
            (loop #'rest)]
           [(#:impersonator)
            (raise-syntax-error 'recursive-contract
                                "#:impersonator keyword must appear right after the expression (if at all)"
                                stx
                                (list #'kwd))]
           [(#:chaperone)
            (raise-syntax-error 'recursive-contract
                                "#:chaperone keyword must appear right after the expression"
                                stx
                                (list #'kwd))]
           [(#:flat)
            (raise-syntax-error 'recursive-contract
                                "#:impersonator keyword must appear right after the expression"
                                stx
                                (list #'kwd))]
           [else
            (raise-syntax-error 
             'recursive-contract
             "type must be one of #:impersonator, #:chaperone, or #:flat"
             stx
             type)])]))
    #`(#,maker '#,stx
               (λ () #,arg)
               '#,(syntax-local-infer-name stx)
               'recursive-contract-val->lnp-not-yet-initialized
               #,(if list-contract? #'#t #'#f)
               #,@(if (equal? (syntax-e type) '#:flat)
                      (list (if extra-delay? #'#t #'#f))
                      '())))
  (syntax-case stx ()
    [(_ arg) (parse-type/kwds #'arg #'#:impersonator '())]
    [(_ arg #:list-contract? . more)
     (parse-type/kwds #'arg
                      #'#:impersonator
                      (syntax-case stx ()
                        [(_ arg . more) #'more]))]
    [(_ arg #:extra-delay . more)
     (parse-type/kwds #'arg
                      #'#:impersonator
                      (syntax-case stx ()
                        [(_ arg . more) #'more]))]
    [(_ arg type . more) (parse-type/kwds #'arg #'type #'more)]))
    
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
     (set-recursive-contract-blame->val-np->val! ctc (make-blame->val-np->val ctc))
     (when (and (pair? old-name) (pair? (cdr old-name)))
       ;; this guard will be #f when we are forcing this contract
       ;; in a nested which (which can make the `cddr` below fail)
       ;; in this case, there should be a pending `force-recursive-contract`
       ;; that will do the actual updating of the name to the right thing
       (set-recursive-contract-name! ctc (append `(recursive-contract ,(contract-name forced-ctc))
                                                 (cddr old-name))))
     forced-ctc]
    [else current]))

(define (make-blame->val-np->val ctc)
  (define list-check? (recursive-contract-list-contract? ctc))
  (define blame-accepting-func-cell (make-thread-cell #f #t))
  (define (do-list-check val neg-party blame-known)
    (when list-check?
      (unless (list? val)
        (raise-blame-error blame-known #:missing-party neg-party
                           val
                           '(expected: "list?" given: "~e")
                           val))))
  (λ (blame)
    (cond
      [(thread-cell-ref blame-accepting-func-cell)
       =>
       (λ (blame-accepting-func) (blame-accepting-func blame))]
      [else
       (define r-ctc (force-recursive-contract ctc))
       (define f (get/build-late-neg-projection r-ctc))
       (define val-neg-party-acceptor (make-thread-cell #f #t))
       (λ (val neg-party)
         (cond
           [(thread-cell-ref val-neg-party-acceptor)
            =>
            (λ (f) (f val neg-party))]
           [else
            (thread-cell-set! blame-accepting-func-cell
                              (λ (blame)
                                (λ (val neg-party)
                                  ((thread-cell-ref val-neg-party-acceptor) val neg-party))))
            (do-list-check val neg-party blame)
            (define f-of-blame 'f-of-blame-not-yet-set)
            (thread-cell-set! val-neg-party-acceptor
                              (λ (val neg-party)
                                (do-list-check val neg-party blame)
                                (f-of-blame val neg-party)))
            (set! f-of-blame (f blame))
            (f-of-blame val neg-party)]))])))

(define (recursive-contract-late-neg-projection ctc)
  (λ (blame)
    (force-recursive-contract ctc)
    ((recursive-contract-blame->val-np->val ctc) blame)))

(define (flat-recursive-contract-late-neg-projection ctc)
  (cond
    [(flat-recursive-contract-extra-delay? ctc)
     (cond
       [(recursive-contract-list-contract? ctc)
        (λ (blame)
          (λ (val neg-party)
            (define r-ctc (force-recursive-contract ctc))
            (define f (get/build-late-neg-projection r-ctc))
            (define blame-known (blame-add-context blame #f))
            (unless (list? val)
              (raise-blame-error blame-known #:missing-party neg-party
                                 val
                                 '(expected: "list?" given: "~e")
                                 val))
            ((f blame-known) val neg-party)))]
       [else
        (λ (blame)
          (λ (val neg-party)
            (define r-ctc (force-recursive-contract ctc))
            (define f (get/build-late-neg-projection r-ctc))
            (define blame-known (blame-add-context blame #f))
            ((f blame-known) val neg-party)))])]
    [else (recursive-contract-late-neg-projection ctc)]))
  
(define (recursive-contract-equivalent this that) (equal? this that))

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

(struct recursive-contract ([name #:mutable]
                            thunk
                            [ctc #:mutable]
                            [blame->val-np->val #:mutable]
                            list-contract?)
  #:property prop:recursive-contract (λ (this)
                                       (force-recursive-contract this)
                                       (recursive-contract-ctc this)))

(struct flat-recursive-contract recursive-contract (extra-delay?)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection flat-recursive-contract-late-neg-projection
   #:stronger recursive-contract-equivalent
   #:equivalent recursive-contract-equivalent
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
(struct chaperone-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection recursive-contract-late-neg-projection
   #:stronger recursive-contract-equivalent
   #:equivalent recursive-contract-equivalent
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
(struct impersonator-recursive-contract recursive-contract ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name recursive-contract-name
   #:first-order recursive-contract-first-order
   #:late-neg-projection recursive-contract-late-neg-projection
   #:stronger recursive-contract-equivalent
   #:equivalent recursive-contract-equivalent
   #:generate recursive-contract-generate
   #:list-contract? recursive-contract-list-contract?))
