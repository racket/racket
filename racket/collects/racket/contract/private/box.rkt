#lang racket/base

(require (for-syntax racket/base)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt")

(provide box-immutable/c box/c)

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
        (if (and (eq? this-content-r this-content-w)
                 (eq? that-content-r that-content-w))
            ;; if the original box/c didn't specify a separate read and write
            ;; contract, we end up in this case
            (contract-struct-equivalent? this-content-r that-content-r)
            (and (contract-struct-stronger? this-content-r that-content-r)
                 (contract-struct-stronger? that-content-w this-content-w)))]
       [else #f])]
    [else #f]))

(define (box/c-equivalent this that)
  (cond
    [(base-box/c? that)
     (define this-content-w (base-box/c-content-w this))
     (define this-content-r (base-box/c-content-r this))
     (define this-immutable (base-box/c-immutable this))
     (define that-content-w (base-box/c-content-w that))
     (define that-content-r (base-box/c-content-r that))
     (define that-immutable (base-box/c-immutable that))
     (and (equal? this-immutable that-immutable)
          (cond
            [(or (equal? this-immutable 'immutable)
                 (and (eq? this-content-r this-content-w)
                      (eq? that-content-r that-content-w)))
             (contract-struct-equivalent? this-content-r that-content-r)]
            [else
             (and (contract-struct-equivalent? this-content-r that-content-r)
                  (contract-struct-equivalent? that-content-w this-content-w))]))]
    [else #f]))

(define-struct (flat-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:trusted trust-me
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:equivalent box/c-equivalent
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
      (define-values (filled? maybe-pos-elem-r-proj maybe-neg-elem-w-proj)
        (contract-pos/neg-doubling (r-vfp box-blame)
                                   (w-vfp (blame-swap box-blame))))
      (define (make-val-np/proc pos-elem-r-proj neg-elem-w-proj)
        (λ (val neg-party)
          (define blame+neg-party (cons blame neg-party))
          (cond
            [(check-box/c-np ctc val blame)
             =>
             (λ (f) (f neg-party))]
            [else
             (if (and (immutable? val) (not (chaperone? val)))
                 (box-immutable (pos-elem-r-proj (unbox val) neg-party))
                 (chaperone/impersonate-box 
                  val
                  (λ (b v)
                    (with-contract-continuation-mark
                        blame+neg-party
                      (pos-elem-r-proj v neg-party)))
                  (λ (b v)
                    (with-contract-continuation-mark
                        blame+neg-party
                      (neg-elem-w-proj v neg-party)))
                  impersonator-prop:contracted ctc
                  impersonator-prop:blame (blame-add-missing-party blame neg-party)))])))
      (cond
        [filled?
         (make-val-np/proc maybe-pos-elem-r-proj maybe-neg-elem-w-proj)]
        [else
         (define tc (make-thread-cell #f))
         (λ (val neg-party)
           (cond
             [(thread-cell-ref tc)
              =>
              (λ (f) (f val neg-party))]
             [else
              (define proc (make-val-np/proc (maybe-pos-elem-r-proj) (maybe-neg-elem-w-proj)))
              (thread-cell-set! tc proc)
              (proc val neg-party)]))]))))

(define-struct (chaperone-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:equivalent box/c-equivalent
   #:late-neg-projection (ho-late-neg-projection chaperone-box)))

(define-struct (impersonator-box/c base-box/c) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:trusted trust-me
   #:name box/c-name
   #:first-order box/c-first-order
   #:stronger box/c-stronger
   #:equivalent box/c-equivalent
   #:late-neg-projection (ho-late-neg-projection impersonate-box)))

(define-syntax (box/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx box/c/proc)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(b/c arg ...)
     (let ([this-one (gensym 'ctc)])
       (define (add-properties args)
         (let loop ([args args])
           (syntax-case args ()
             [() '()]
             [(kwd arg . more)
              (keyword? (syntax-e #'kwd))
              (list* #'kwd (add-property #'arg) (loop #'more))]
             [(arg . more)
              (cons (add-property #'arg) (loop #'more))])))
       (define (add-property arg)
         (syntax-property
          (syntax-property
           arg
           'racket/contract:negative-position
           this-one)
          'racket/contract:positive-position
          this-one))
       (with-syntax ([app (datum->syntax stx '#%app)])
         (syntax-property
          (quasisyntax/loc stx
            (app box/c/proc #,@(add-properties #'(arg ...))))
          'racket/contract:contract
          (vector this-one (list #'b/c) null))))]))

(define (box/c/proc elem-w [elem-r elem-w] #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (define ctc-w
    (if flat?
        (coerce-flat-contract 'box/c elem-w)
        (coerce-contract 'box/c elem-w)))
  (define ctc-r
    (if flat?
        (coerce-flat-contract 'box/c elem-r)
        (coerce-contract 'box/c elem-r)))
  (cond
    [(or flat?
         (and (equal? immutable #t)
              (flat-contract? ctc-r)))
     (make-flat-box/c ctc-w ctc-r immutable)]
    [(and (chaperone-contract? ctc-w)
          (chaperone-contract? ctc-r))
     (make-chaperone-box/c ctc-w ctc-r immutable)]
    [else
     (make-impersonator-box/c ctc-w ctc-r immutable)]))
