#lang racket/base

(require (for-syntax racket/base)
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt")

(provide (rename-out [wrap-hash/c hash/c]))

(define-syntax (wrap-hash/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx hash/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list stx) null))]
    [(h/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'ctc)])
       (define (convert-args args)
         (let loop ([args args]
                    [new-args null]
                    [neg-ctc? #t])
           (cond
             [(null? args) (reverse new-args)]
             [(keyword? (syntax-e (car args)))
              (if (null? (cdr args))
                  (reverse (cons (car args) new-args))
                  (loop (cddr args)
                        (list* (cadr args) (car args) new-args)
                        neg-ctc?))]
             [neg-ctc?
              (loop (cdr args)
                    (cons (syntax-property 
                           (car args)
                           'racket/contract:negative-position
                           this-one)
                          new-args)
                    #f)]
             [else
              (append (reverse new-args)
                      (cons (syntax-property
                             (car args)
                             'racket/contract:positive-position
                             this-one)
                            (cdr args)))])))
       (with-syntax ([(new-arg ...) (convert-args args)]
                     [app (datum->syntax stx '#%app)])
         (syntax-property
          (syntax/loc stx
            (app hash/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'h/c) null))))]))

(define (hash/c dom rng #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (unless (member immutable '(#t #f dont-care))
    (raise-argument-error 'hash/c
                          "(or/c #t #f 'dont-care) for the #:immutable argument"
                          immutable))
  (define dom-ctc (if flat?
                      (coerce-flat-contract 'hash/c dom)
                      (coerce-chaperone-contract 'hash/c dom)))
  (define rng-ctc (if flat?
                      (coerce-flat-contract 'hash/c rng)
                      (coerce-contract 'hash/c rng)))
  (cond
    [(or flat?
         (and (eq? immutable #t)
              (flat-contract? dom-ctc)
              (flat-contract? rng-ctc)))
     (make-flat-hash/c dom-ctc rng-ctc immutable)]
    [(chaperone-contract? rng-ctc)
     (make-chaperone-hash/c dom-ctc rng-ctc immutable)]
    [else
     (make-impersonator-hash/c dom-ctc rng-ctc immutable)]))

(define (check-hash/c ctc val blame) 
  (define dom-ctc (base-hash/c-dom ctc))
  (define rng-ctc (base-hash/c-rng ctc))
  (define immutable (base-hash/c-immutable ctc))
  (define flat? (flat-hash/c? ctc))
  (unless (hash? val)
    (raise-blame-error blame val '(expected "a hash" given: "~e") val))
  (when (and (not flat?)
             (not (flat-contract? dom-ctc))
             (not (hash-equal? val)))
    (raise-blame-error
     blame val
     '(expected "equal?-based hash table due to higher-order domain contract" given: "~e")
     val))
  (case immutable
    [(#t) 
     (unless (immutable? val) 
       (raise-blame-error blame val
                          '(expected "an immutable hash" given: "~e") val))]
    [(#f)
     (when (immutable? val)
       (raise-blame-error blame val
                          '(expected "a mutable hash" given: "~e") val))]
    [(dont-care) (void)]))

(define (hash/c-first-order ctc)
  (define dom-ctc (base-hash/c-dom ctc))
  (define rng-ctc (base-hash/c-rng ctc))
  (define immutable (base-hash/c-immutable ctc))
  (define flat? (flat-hash/c? ctc))
  (λ (val)
    (and (hash? val)
         (or flat?
             (flat-contract? dom-ctc)
             (hash-equal? val))
         (case immutable
           [(#t) (immutable? val)]
           [(#f) (not (immutable? val))]
           [else #t])
         (for/and ([(k v) (in-hash val)])
           (and (contract-first-order-passes? dom-ctc k)
                (contract-first-order-passes? rng-ctc v))))))

(define (hash/c-name ctc)
  (apply 
   build-compound-type-name
   'hash/c (base-hash/c-dom ctc) (base-hash/c-rng ctc)
   (append
    (if (and (flat-hash/c? ctc)
             (not (eq? (base-hash/c-immutable ctc) #t)))
        (list '#:flat? #t)
        null)
    (case (base-hash/c-immutable ctc)
      [(dont-care) null]
      [(#t)
       (list '#:immutable #t)]
      [(#f)
       (list '#:immutable #f)]))))

(define-struct base-hash/c (dom rng immutable))

(define (hash/c-stronger this that)
  (define this-dom (base-hash/c-dom this))
  (define this-rng (base-hash/c-rng this))
  (define this-immutable (base-hash/c-immutable this))
  (cond
    [(base-hash/c? that)
     (define that-dom (base-hash/c-dom that))
     (define that-rng (base-hash/c-rng that))
     (define that-immutable (base-hash/c-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (and (contract-stronger? this-dom that-dom)
             (contract-stronger? this-rng that-rng))]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (and (contract-stronger? this-dom that-dom)
             (contract-stronger? that-dom this-dom)
             (contract-stronger? this-rng that-rng)
             (contract-stronger? that-rng this-rng))]
       [else #f])]
    [else #f]))

(define-struct (flat-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:stronger hash/c-stronger
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (check-hash/c ctc val blame)
         (define dom-proj ((contract-projection (base-hash/c-dom ctc)) 
                           (blame-add-context blame "the keys of")))
         (define rng-proj ((contract-projection (base-hash/c-rng ctc))
                           (blame-add-context blame "the values of")))
         (for ([(k v) (in-hash val)])
           (dom-proj k)
           (rng-proj v))
         val)))))

(define (ho-projection hash-wrapper)
  (λ (ctc)
    (let ([dom-proc (contract-projection (base-hash/c-dom ctc))]
          [rng-proc (contract-projection (base-hash/c-rng ctc))]
          [immutable (base-hash/c-immutable ctc)])
      (λ (blame)
        (define pos-dom-proj (dom-proc (blame-add-context blame "the keys of")))
        (define neg-dom-proj (dom-proc (blame-add-context blame "the keys of" #:swap? #t)))
        (define pos-rng-proj (rng-proc (blame-add-context blame "the values of")))
        (define neg-rng-proj (rng-proc (blame-add-context blame "the values of" #:swap? #t)))
        (λ (val)
          (check-hash/c ctc val blame)
          (if (and (immutable? val) (not (chaperone? val)))
              (let ([hash-maker
                     (cond
                       [(hash-equal? val) make-immutable-hash]
                       [(hash-eqv? val) make-immutable-hasheqv]
                       [(hash-eq? val) make-immutable-hasheq])])
                (hash-maker
                 (for/list ([(k v) (in-hash val)])
                   (cons (pos-dom-proj k)
                         (pos-rng-proj v)))))
              (hash-wrapper
               val
               (λ (h k)
                 (values (neg-dom-proj k)
                         (λ (h k v)
                           (pos-rng-proj v))))
               (λ (h k v)
                 (values (neg-dom-proj k)
                         (neg-rng-proj v)))
               (λ (h k)
                 (neg-dom-proj k))
               (λ (h k)
                 (pos-dom-proj k))
               impersonator-prop:contracted ctc
               impersonator-prop:blame blame)))))))

(define-struct (chaperone-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:stronger hash/c-stronger
   #:projection (ho-projection chaperone-hash)))

(define-struct (impersonator-hash/c base-hash/c) ()
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name hash/c-name
   #:first-order hash/c-first-order
   #:stronger hash/c-stronger
   #:projection (ho-projection impersonate-hash)))
