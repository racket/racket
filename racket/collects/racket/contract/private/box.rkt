#lang racket/base

(require (for-syntax racket/base)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt")

(provide box-immutable/c 
         (rename-out [wrap-box/c box/c]))

(define/subexpression-pos-prop (box-immutable/c elem)
  (box/c elem #:immutable #t))

(define-struct base-box/c (content-w content-r immutable))

(define (check-box/c ctc val blame)
  (define immutable (base-box/c-immutable ctc))
  (unless (box? val)
    (raise-blame-error blame val '(expected "a box" given: "~e") val))
  (case immutable
    [(#t)
     (unless (immutable? val)
       (raise-blame-error blame val '(expected "an immutable box" given: "~e") val))]
    [(#f)
     (when (immutable? val)
       (raise-blame-error blame val '(expected "a mutable box" given: "~e") val))]
    [(dont-care) (void)]))

(define (check-box/c-np ctc val blame)
  (define immutable (base-box/c-immutable ctc))
  (cond
    [(box? val) 
     (case immutable
       [(#t)
        (cond
          [(immutable? val) #f]
          [else
           (λ (neg-party)
             (raise-blame-error blame #:missing-party neg-party
                                val '(expected "an immutable box" given: "~e") val))])]
       [(#f)
        (cond
          [(immutable? val) #F]
          [else
           (λ (neg-party)
             (raise-blame-error blame #:missing-party neg-party
                                val '(expected "a mutable box" given: "~e") val))])]
       [(dont-care) #f])]
    [else
     (λ (neg-party)
       (raise-blame-error blame #:missing-party neg-party
                          val '(expected "a box" given: "~e") val))]))

(define (box/c-first-order ctc)
  (define elem-r-ctc (base-box/c-content-r ctc))
  (define immutable (base-box/c-immutable ctc))
  (λ (val)
    (and (box? val)
         (case immutable
           [(#t) (immutable? val)]
           [(#f) (not (immutable? val))]
           [(dont-care) #t])
         (contract-first-order-passes? elem-r-ctc (unbox val)))))

(define (box/c-name ctc)
  (let ([elem-w-name (contract-name (base-box/c-content-w ctc))]
        [elem-r-name (contract-name (base-box/c-content-r ctc))]
        [immutable (base-box/c-immutable ctc)]
        [flat? (flat-box/c? ctc)])
    (apply build-compound-type-name
           'box/c
           elem-w-name
           (append
            (if (not (equal? elem-w-name elem-r-name))
                (list elem-r-name)
                null)
            (if (and flat? (eq? immutable #t))
                (list '#:immutable #t)
                (append
                 (if (not (eq? immutable 'dont-care))
                     (list '#:immutable immutable)
                     null)
                 (if flat?
                     (list '#:flat? #t)
                     null)))))))

(define (add-box-context blame)
  (blame-add-context blame "the content of"))

(define (box/c-stronger this that)
  (define this-content-w (base-box/c-content-w this))
  (define this-content-r (base-box/c-content-r this))
  (define this-immutable (base-box/c-immutable this))
  (cond
    [(base-box/c? that)
     (define that-content-w (base-box/c-content-w that))
     (define that-content-r (base-box/c-content-r that))
     (define that-immutable (base-box/c-immutable that))
     (cond
       [(and (equal? this-immutable #t)
             (equal? that-immutable #t))
        (contract-struct-stronger? this-content-r that-content-r)]
       [(or (equal? that-immutable 'dont-care)
            (equal? this-immutable that-immutable))
        (and (contract-struct-stronger? this-content-r that-content-r)
             (contract-struct-stronger? that-content-w this-content-w))]
       [else #f])]
    [else #f]))


(define-struct (flat-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:late-neg-projection
   (λ (ctc)
     (define content-ctc (get/build-late-neg-projection (base-box/c-content-w ctc)))
     (λ (blame)
       (define box-blame (add-box-context blame))
       (define late-neg-proj (content-ctc box-blame))
       (λ (val neg-party)
         (define fail-proc (check-box/c-np ctc val box-blame))
         (cond
           [fail-proc (fail-proc neg-party)]
           [else
            (late-neg-proj (unbox val) neg-party)
            val]))))))

(define (ho-late-neg-projection chaperone/impersonate-box)
  (λ (ctc)
    (define elem-w-ctc (base-box/c-content-w ctc))
    (define elem-r-ctc (base-box/c-content-r ctc))
    (define immutable (base-box/c-immutable ctc))
    (define w-vfp (get/build-late-neg-projection elem-w-ctc))
    (define r-vfp (get/build-late-neg-projection elem-r-ctc))
    (λ (blame)
      (define box-blame (add-box-context blame))
      (define pos-elem-r-proj (r-vfp box-blame))
      (define neg-elem-w-proj (w-vfp (blame-swap box-blame)))
      (λ (val neg-party)
        (cond
          [(check-box/c-np ctc val blame)
           =>
           (λ (f) (f neg-party))]
          [else
           (if (and (immutable? val) (not (chaperone? val)))
               (box-immutable (pos-elem-r-proj (unbox val) neg-party))
               (chaperone/impersonate-box 
                val
                (λ (b v) (pos-elem-r-proj v neg-party))
                (λ (b v) (neg-elem-w-proj v neg-party))
                impersonator-prop:contracted ctc
                impersonator-prop:blame (blame-add-missing-party blame neg-party)))])))))

(define-struct (chaperone-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:late-neg-projection (ho-late-neg-projection chaperone-box)))

(define-struct (impersonator-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:late-neg-projection (ho-late-neg-projection impersonate-box)))

(define-syntax (wrap-box/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx box/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(b/c arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'ctc)])
       (define (convert-args args)
         (let loop ([args args]
                    [new-args null])
           (cond
             [(null? args) (reverse new-args)]
             [(keyword? (syntax-e (car args)))
              (if (null? (cdr args))
                  (reverse (cons (car args) new-args))
                  (loop (cddr args)
                        (list* (cadr args) (car args) new-args)))]
             [else (append (reverse new-args)
                           (cons (syntax-property
                                  (syntax-property
                                   (car args)
                                   'racket/contract:negative-position
                                   this-one)
                                  'racket/contract:positive-position
                                  this-one)
                                 (cdr args)))])))
       (with-syntax ([(new-arg ...) (convert-args args)]
                     [app (datum->syntax stx '#%app)])
         (syntax-property
          (syntax/loc stx
            (app box/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'b/c) null))))]))

(define (box/c elem-w [elem-r elem-w] #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (let ([ctc-w (if flat?
                   (coerce-flat-contract 'box/c elem-w)
                   (coerce-contract 'box/c elem-w))]
        [ctc-r (if flat?
                   (coerce-flat-contract 'box/c elem-r)
                   (coerce-contract 'box/c elem-w))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (flat-contract? ctc-r)))
       (make-flat-box/c ctc-w ctc-r immutable)]
      [(and (chaperone-contract? ctc-w)
            (chaperone-contract? ctc-r))
       (make-chaperone-box/c ctc-w ctc-r immutable)]
      [else
       (make-impersonator-box/c ctc-w ctc-r immutable)])))

