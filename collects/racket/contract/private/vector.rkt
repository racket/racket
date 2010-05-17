#lang racket/base

(require (for-syntax racket/base)
         "guts.ss")

(provide vector/c (rename-out [wrap-vectorof vectorof])
         vector-immutable/c vector-immutableof)

(define-struct vectorof (elem immutable))

(define (vectorof-name c)
  (let ([immutable (vectorof-immutable c)])
    (apply build-compound-type-name 'vectorof 
           (contract-name (vectorof-elem c))
           (append
            (if (and (flat-vectorof? c)
                     (not (eq? immutable #t)))
                (list '#:flat? #t)
                null)
            (if (not (eq? immutable 'dont-care))
                (list '#:immutable immutable)
                null)))))

(define (vectorof-first-order c) 
  (let ([elem-ctc (vectorof-elem c)]
        [immutable (vectorof-immutable c)]
        [flat? (flat-vectorof? c)])
    (λ (val #:blame [blame #f])
      (let/ec return
        (define (fail . args)
          (if blame
              (apply raise-blame-error blame val args)
              (return #f)))
        (unless (vector? val)
          (fail "expected a vector, got ~a" val))
        (cond
          [(eq? immutable #t)
           (unless (immutable? val)
             (fail "expected an immutable vector, got ~a" val))]
          [(eq? immutable #f)
           (when (immutable? val)
             (fail "expected an mutable vector, got ~a" val))]
          [else (void)])
        (when (or flat? (and (immutable? val) (not blame)))
          (if blame
              (let ([elem-proj ((contract-projection elem-ctc) blame)])
                (for ([e (in-vector val)])
                  (elem-proj e)))
              (for ([e (in-vector val)])
                (unless (contract-first-order-passes? elem-ctc e)
                  (fail)))))
        #t))))

(define-struct (flat-vectorof vectorof) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection 
   (λ (ctc) 
     (λ (blame) 
       (λ (val)
         ((vectorof-first-order ctc) val #:blame blame)
         val)))))

(define (vectorof-ho-projection vector-wrapper)
  (λ (ctc)
     (let ([elem-ctc (vectorof-elem ctc)]
           [immutable (vectorof-immutable ctc)])
       (λ (blame)
         (let ([elem-pos-proj ((contract-projection elem-ctc) blame)]
               [elem-neg-proj ((contract-projection elem-ctc) (blame-swap blame))])
           (λ (val)
             ((vectorof-first-order ctc) val #:blame blame)
             (if (immutable? val)
                 (apply vector-immutable
                        (for/list ([e (in-vector val)])
                          (elem-pos-proj e)))
                 (vector-wrapper
                  val
                  (λ (vec i val)
                    (elem-pos-proj val))
                  (λ (vec i val)
                    (elem-neg-proj val))))))))))

(define-struct (chaperone-vectorof vectorof) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection chaperone-vector)))

(define-struct (proxy-vectorof vectorof) ()
  #:property prop:contract
  (build-contract-property
   #:name vectorof-name
   #:first-order vectorof-first-order
   #:projection (vectorof-ho-projection proxy-vector)))

(define-syntax (wrap-vectorof stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx build-vectorof)
      'racket/contract:contract
      (vector (gensym 'ctc) (list #'x) null))]
    [(vecof arg ...)
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
            (app build-vectorof new-arg ...))
          'racket/contract:contract
          (vector this-one (list #'vecof) null))))]))

(define (build-vectorof c #:immutable [immutable 'dont-care] #:flat? [flat? #f])
  (let ([ctc (if flat?
                 (coerce-flat-contract 'vectorof c)
                 (coerce-contract 'vectorof c))])
    (cond
      [(or flat?
           (and (eq? immutable #t)
                (flat-contract? ctc)))
       (make-flat-vectorof ctc immutable)]
      [(chaperone-contract? ctc)
       (make-chaperone-vectorof ctc immutable)]
      [else
       (make-proxy-vectorof ctc immutable)])))

(define/subexpression-pos-prop (vector-immutableof c)
  (build-vectorof c #:immutable #t))

(define/subexpression-pos-prop (vector/c . args)
  (let* ([ctcs (coerce-flat-contracts 'vector/c args)]
         [largs (length args)]
         [procs (map flat-contract-predicate ctcs)])
    (build-flat-contract
     (apply build-compound-type-name 'vector/c ctcs)
     (λ (v)
       (and (vector? v)
            (= (vector-length v) largs)
            (andmap (λ (p? x) (p? x))
                    procs
                    (vector->list v)))))))

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

(define vector-immutable/c (*-immutable/c vector?
                                          vector-immutable
                                          (#t (λ (v i) (vector-ref v i)))
                                          (λ (n v) (= n (vector-length v)))
                                          immutable-vector
                                          vector-immutable/c))
