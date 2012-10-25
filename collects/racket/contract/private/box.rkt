#lang racket/base

(require (for-syntax racket/base)
         "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "misc.rkt")

(provide box-immutable/c 
         (rename-out [wrap-box/c box/c]))

(define/subexpression-pos-prop (box-immutable/c elem)
  (box/c elem #:immutable #t))

(define-struct base-box/c (content immutable))

(define (check-box/c ctc val blame)
  (define elem-ctc (base-box/c-content ctc))
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

(define (box/c-first-order ctc)
  (define elem-ctc (base-box/c-content ctc))
  (define immutable (base-box/c-immutable ctc))
  (λ (val)
    (and (box? val)
         (case immutable
           [(#t) (immutable? val)]
           [(#f) (not (immutable? val))]
           [(dont-care) #t])
         (contract-first-order-passes? elem-ctc (unbox val)))))

(define (box/c-name ctc)
  (let ([elem-name (contract-name (base-box/c-content ctc))]
        [immutable (base-box/c-immutable ctc)]
        [flat? (flat-box/c? ctc)])
    (apply build-compound-type-name
           'box/c
           elem-name
           (if (and flat? (eq? immutable #t))
               (list '#:immutable #t)
               (append
                (if (not (eq? immutable 'dont-care))
                    (list '#:immutable immutable)
                    null)
                (if flat?
                    (list '#:flat? #t)
                    null))))))

(define-struct (flat-box/c base-box/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         (check-box/c ctc val blame)
         (((contract-projection (base-box/c-content ctc)) blame) (unbox val))
         val)))))

(define (ho-projection box-wrapper)
  (λ (ctc)
    (let ([elem-ctc (base-box/c-content ctc)]
          [immutable (base-box/c-immutable ctc)])
      (λ (blame)
        (let ([pos-elem-proj ((contract-projection elem-ctc) blame)]
              [neg-elem-proj ((contract-projection elem-ctc) (blame-swap blame))])
          (λ (val)
            (check-box/c ctc val blame)
            (if (and (immutable? val) (not (chaperone? val)))
                (box-immutable (pos-elem-proj (unbox val)))
                (box-wrapper val
                             (λ (b v) (pos-elem-proj v))
                             (λ (b v) (neg-elem-proj v))
                             impersonator-prop:contracted ctc))))))))

(define-struct (chaperone-box/c base-box/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection (ho-projection chaperone-box)))

(define-struct (impersonator-box/c base-box/c) ()
  #:property prop:contract
  (build-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection (ho-projection impersonate-box)))

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
                                  (car args)
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

(define (box/c elem #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (let ([ctc (if flat?
                 (coerce-flat-contract 'box/c elem)
                 (coerce-contract 'box/c elem))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (flat-contract? ctc)))
       (make-flat-box/c ctc immutable)]
      [(chaperone-contract? ctc)
       (make-chaperone-box/c ctc immutable)]
      [else
       (make-impersonator-box/c ctc immutable)])))

