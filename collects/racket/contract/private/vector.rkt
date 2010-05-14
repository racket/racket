#lang racket/base

(require (for-syntax racket/base)
         "guts.ss")

(provide vector/c vectorof vector-immutable/c vector-immutableof)

(define-syntax (*-immutableof stx)
  (syntax-case stx ()
    [(_ predicate? fill testmap type-name name)
     (identifier? (syntax predicate?))
     (syntax
      (let ([fill-name fill])
        (λ (input)
          (let ([ctc (coerce-contract 'name input)])
            (if (flat-contract? ctc)
                (let ([content-pred? (flat-contract-predicate ctc)])
                  (build-flat-contract
                   `(name ,(contract-name ctc))
                   (lambda (x) (and (predicate? x) (testmap content-pred? x)))))
                (let ([proj (contract-projection ctc)])
                  (make-contract
                   #:name (build-compound-type-name 'name ctc)
                   #:projection
                   (λ (blame)
                     (let ([p-app (proj blame)])
                       (λ (val)
                         (unless (predicate? val)
                           (raise-blame-error
                            blame
                            val
                            "expected <~a>, given: ~e"
                            'type-name
                            val))
                         (fill-name p-app val))))
                   #:first-order predicate?)))))))]))

(define/final-prop (immutable-vector? val) (and (immutable? val) (vector? val)))

(define vector-immutableof
  (*-immutableof immutable-vector?
                 (λ (f v) (apply vector-immutable (map f (vector->list v))))
                 (λ (f v) (andmap f (vector->list v)))
                 immutable-vector
                 vector-immutableof))

(define/subexpression-pos-prop (vectorof p)
  (let* ([ctc (coerce-flat-contract 'vectorof p)]
         [pred (flat-contract-predicate ctc)])
    (build-flat-contract
     (build-compound-type-name 'vectorof ctc)
     (λ (v)
       (and (vector? v)
            (andmap pred (vector->list v)))))))

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
