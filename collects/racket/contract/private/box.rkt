#lang racket/base

(require (for-syntax racket/base)
         "guts.rkt")

(provide box-immutable/c 
         (rename-out [wrap-box/c box/c]))

(define-syntax (*-immutable/c stx)
  (syntax-case stx ()
    [(_ predicate? constructor (arb? selectors ...) type-name name)
     #'(*-immutable/c predicate? constructor (arb? selectors ...) type-name name #t)]
    [(_ predicate? constructor (arb? selectors ...) type-name name test-immutable?)
     (and (eq? #f (syntax->datum (syntax arb?)))
          (boolean? (syntax->datum #'test-immutable?)))
     (let ([test-immutable? (syntax->datum #'test-immutable?)])
       (with-syntax ([(params ...) (generate-temporaries (syntax (selectors ...)))]
                     [(p-apps ...) (generate-temporaries (syntax (selectors ...)))]
                     [(ctc-x ...) (generate-temporaries (syntax (selectors ...)))]
                     [(procs ...) (generate-temporaries (syntax (selectors ...)))]
                     [(selector-names ...) (generate-temporaries (syntax (selectors ...)))])
         #`(let ([predicate?-name predicate?]
                 [constructor-name constructor]
                 [selector-names selectors] ...)
             (λ (params ...)
               (let ([ctc-x (coerce-contract 'name params)] ...)
                 (if (and (flat-contract? ctc-x) ...)
                     (let ([p-apps (flat-contract-predicate ctc-x)] ...)
                       (build-flat-contract
                        `(name ,(contract-name ctc-x) ...)
                        (lambda (x)
                          (and (predicate?-name x)
                               (p-apps (selector-names x)) 
                               ...))))
                     (let ([procs (contract-projection ctc-x)] ...)
                       (make-contract
                        #:name (build-compound-type-name 'name ctc-x ...)
                        #:projection
                        (λ (blame)
                          (let ([p-apps (procs blame)] ...)
                            (λ (v)
                              (if #,(if test-immutable?
                                        #'(and (predicate?-name v)
                                               (immutable? v))
                                        #'(predicate?-name v))
                                  (constructor-name (p-apps (selector-names v)) ...)
                                  (raise-blame-error
                                   blame
                                   v
                                   #,(if test-immutable?
                                         "expected immutable <~a>, given: ~e"
                                         "expected <~a>, given: ~e")
                                   'type-name
                                   v)))))))))))))]
    [(_ predicate? constructor (arb? selector) correct-size type-name name)
     (eq? #t (syntax->datum (syntax arb?)))
     (syntax
      (let ([predicate?-name predicate?]
            [constructor-name constructor]
            [selector-name selector])
        (λ params
          (let ([ctcs (map (λ (param) (coerce-contract 'name param)) params)])
            (let ([procs (map contract-projection ctcs)])
              (make-contract
               #:name (apply build-compound-type-name 'name ctcs)
               #:projection
               (λ (blame)
                 (let ([p-apps (map (λ (proc) (proc blame)) procs)]
                       [count (length params)])
                   (λ (v)
                     (if (and (immutable? v)
                              (predicate?-name v)
                              (correct-size count v))
                         (apply constructor-name 
                                (let loop ([p-apps p-apps]
                                           [i 0])
                                  (cond
                                    [(null? p-apps) null]
                                    [else (let ([p-app (car p-apps)])
                                            (cons (p-app (selector-name v i))
                                                  (loop (cdr p-apps) (+ i 1))))])))
                         (raise-blame-error
                          blame
                          v
                          "expected <~a>, given: ~e"
                          'type-name
                          v)))))))))))]))

(define box-immutable/c (*-immutable/c box? box-immutable (#f unbox) immutable-box box-immutable/c))

(define-struct box/c (content immutable))

(define (box/c-first-order ctc)
  (let ([elem-ctc (box/c-content ctc)]
        [immutable (box/c-immutable ctc)]
        [flat? (flat-box/c? ctc)])
    (λ (val #:blame [blame #f])
      (let/ec return
        (define (fail . args)
          (if blame
              (apply raise-blame-error blame val args)
              (return #f)))
        (unless (box? val)
          (fail "expected a box, got ~a" val))
        (case immutable
          [(#t)
           (unless (immutable? val)
             (fail "expected an immutable box, got ~a" val))]
          [(#f)
           (when (immutable? val)
             (fail "expected a mutable box, got ~a" val))]
          [(dont-care) (void)])
        (when (or flat? (and (immutable? val) (not blame)))
          (if blame
              (begin (((contract-projection elem-ctc) blame) (unbox val))
                     (void))
              (unless (contract-first-order-passes? elem-ctc (unbox val))
                (fail))))))))

(define (box/c-name ctc)
  (let ([elem-name (contract-name (box/c-content ctc))]
        [immutable (box/c-immutable ctc)]
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

(define-struct (flat-box/c box/c) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection
   (λ (ctc)
     (λ (blame)
       (λ (val)
         ((box/c-first-order ctc) val #:blame blame)
         val)))))

(define (ho-projection box-wrapper)
  (λ (ctc)
    (let ([elem-ctc (box/c-content ctc)]
          [immutable (box/c-immutable ctc)])
      (λ (blame)
        (let ([pos-elem-proj ((contract-projection elem-ctc) blame)]
              [neg-elem-proj ((contract-projection elem-ctc) (blame-swap blame))])
          (λ (val)
            ((box/c-first-order ctc) val #:blame blame)
            (if (immutable? val)
                (box-immutable (pos-elem-proj (unbox val)))
                (box-wrapper val
                             (λ (b v) (pos-elem-proj v))
                             (λ (b v) (neg-elem-proj v))))))))))

(define-struct (chaperone-box/c box/c) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection (ho-projection chaperone-box)))

(define-struct (proxy-box/c box/c) ()
  #:property prop:contract
  (build-contract-property
   #:name box/c-name
   #:first-order box/c-first-order
   #:projection (ho-projection proxy-box)))

(define-syntax (wrap-box/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx build-box/c)
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
            (app build-box/c new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'b/c) null))))]))

(define (build-box/c elem #:immutable [immutable 'dont-care] #:flat? [flat? #f])
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
       (make-proxy-box/c ctc immutable)])))

