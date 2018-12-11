#lang racket/base
(require (for-syntax racket/base
                     racket/pretty)
         racket/stxparam
         "fixnum.rkt")

;; The `define-constructor` form implements a basic object system
;; where any `(define (id ....) ....)` or `(define id (case-lambda
;; ....)` in the constructor body is treated as a method that is
;; converted to accept a vector of free variables.
;;
;; A `lambda` or `case-lambda` is any other position can be wrapped in
;; `method` to convert it, too. Note that the caller of such methods
;; must know to pass along the self vector somehow. The `self`
;; identifier is bound to the self vector.
;;
;; Constraints:
;;
;;   Macros are not expanded to recognize definitions; use only
;;   `define` or `define-fixnum` for definitions.
;;
;;   Only `set!` any variables within a method.
;;
;;   A named method cannot be used as a value unless it is wrapped
;;   with `method`; calls are automatcally converted to pass along the
;;   self vector, while the `method` escape obliges a called of the
;;   result to pass along the self vector.

(provide define-constructor
         method
         self)

(define-syntax-parameter self
  (lambda (stx)
    (raise-syntax-error #f "misuse outside of a constructor" stx)))

(define-syntax-parameter current-constructor-fields #f)
(define-syntax-parameter current-constructor-methods #f)

(define-for-syntax (get-current-constructor-fields)
  (syntax-parameter-value #'current-constructor-fields))
(define-for-syntax (get-current-constructor-methods)
  (syntax-parameter-value #'current-constructor-methods))

(define-for-syntax (maybe-lift mode e)
  (syntax-case mode ()
    [#:lift (syntax-local-lift-expression e)]
    [#:no-lift e]))

(define-syntax (method stx)
  (syntax-case stx (lambda case-lambda)
    [(_ id)
     (identifier? #'id)
     #'(id #:method)]
    [(_ rhs)
     #'(method #:lift rhs)]
    [(_ lift-mode (lambda (arg ...) body ...))
     (with-syntax ([fields (get-current-constructor-fields)]
                   [methods (get-current-constructor-methods)]
                   [(_ _ inside) stx])
       (maybe-lift
        #'lift-mode
        #`(lambda (this arg ...)
            (let-methods
             this methods inside
             (let-fields
              this 0 fields inside
              body ...)))))]
    [(_ lift-mode (case-lambda [(arg ...) body ...] ...))
     (with-syntax ([fields (get-current-constructor-fields)]
                   [methods (get-current-constructor-methods)]
                   [(_ _ inside) stx])
       (maybe-lift
        #'lift-mode
        #`(case-lambda
            [(this arg ...)
             (let-methods
              this methods inside
              (let-fields
               this 0 fields inside
               body ...))]
            ...)))]))

(define-for-syntax (make-method-transformer self-id id)
  (lambda (stx)
    (syntax-case stx ()
      [(_ #:method) id]
      [(_ arg ...) (quasisyntax/loc stx
                     (#,id #,self-id arg ...))])))

(define-syntax (define-constructor stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ... last-body)
     (andmap identifier? (syntax->list #'(arg ...)))
     (let ()
       (define fields
         (apply append
                (syntax->list #'(arg ...))
                (for/list ([body (in-list (syntax->list #'(body ...)))])
                  (syntax-case body (define case-lambda define-values define-fixnum)
                    [(define (id . _) . _)
                     null]
                    [(define id (case-lambda . _))
                     null]
                    [(define id _)
                     (list #'id)]
                    [(define-values (id ...) _)
                     (syntax->list #'(id ...))]
                    [(define-fixnum id _)
                     (list #'(capture-fixnum id))]
                    [else
                     null]))))
       (define methods
         (apply
          append
          (for/list ([body (in-list (syntax->list #'(body ...)))])
            (syntax-case body (define define-values define-fixnum case-lambda)
              [(define (id . args) . bodys)
               (list (cons #'id (generate-temporaries #'(id))))]
              [(define id (case-lambda . clauses))
               (list (cons #'id (generate-temporaries #'(id))))]
              [_ null]))))
       (define (find-method-name id)
         (for/or ([pr (in-list methods)])
           (and (free-identifier=? id (car pr))
                (cadr pr))))
       (define num-args (length (syntax->list #'(arg ...))))
       (define-values (method-defns other-bodys count)
         (for/fold ([method-defns null] [other-bodys null] [n num-args]) ([body (in-list (syntax->list #'(body ...)))])
           (syntax-case body (define define-values define-fixnum case-lambda)
             [(define (id . args) . bodys)
              (let ([re (lambda (s) (datum->syntax body (syntax-e s) body body))]
                    [tmp-id (find-method-name #'id)])
                (values (cons #`(define #,tmp-id
                                  (syntax-parameterize ([current-constructor-fields (quote-syntax #,fields)]
                                                        [current-constructor-methods (quote-syntax #,methods)])
                                    (method #:no-lift #,(re #'(lambda args . bodys)))))
                              method-defns)
                        other-bodys
                        n))]
             [(define id (case-lambda . clauses))
              (let ([re (lambda (s) (datum->syntax body (syntax-e s) body body))]
                    [tmp-id (find-method-name #'id)])
                (values (cons #`(define #,tmp-id
                                  (syntax-parameterize ([current-constructor-fields (quote-syntax #,fields)]
                                                        [current-constructor-methods (quote-syntax #,methods)])
                                    (method #:no-lift #,(re #'(case-lambda clauses)))))
                              method-defns)
                        other-bodys
                        n))]
             [(define id _)
              (values method-defns
                      (list* #`(vector*-set! self-vec #,n id)
                             body
                             other-bodys)
                      (add1 n))]
             [(define-values (id ...) _)
              (values method-defns
                      (list* #`(begin #,@(for/list ([id (in-list (syntax->list #'(id ...)))]
                                                    [n (in-naturals n)])
                                           #`(vector*-set! self-vec #,n #,id)))
                             body
                             other-bodys)
                      (+ n (length (syntax->list #'(id ...)))))]
             [(define-fixnum id _)
              (values method-defns
                      (list* #`(vector*-set! self-vec #,n (capture-fixnum id))
                             body
                             other-bodys)
                      (add1 n))]
             [else
              (values method-defns
                      (cons body other-bodys)
                      n)])))
       (with-syntax ([inside stx]
                     [count #`#,count]
                     [fields fields]
                     [methods methods]
                     [(init-arg ...) (for/list ([arg (in-list (syntax->list #'(arg ...)))]
                                                [i (in-naturals)])
                                       #`(vector*-set! self-vec #,i #,arg))]
                     [(other-body ...) (reverse other-bodys)]
                     [(method-defn ...) (reverse method-defns)])
         #'(begin
             method-defn ...
             (define (name arg ...)
               (define self-vec (make-vector count))
               init-arg ...
               (syntax-parameterize ([current-constructor-fields (quote-syntax fields)]
                                     [current-constructor-methods (quote-syntax methods)]
                                     [self (lambda (stx) #'self-vec)])
                 (let-methods
                  self-vec methods inside
                  (let ()
                    other-body ...
                    last-body)))))))]
    [(_ (name . formals) body ... last-body)
     (with-syntax ([(arg ...) (let loop ([formals #'formals])
                                (syntax-case formals ()
                                  [() null]
                                  [id
                                   (identifier? #'id)
                                   (list #'id)]
                                  [(kw . formals)
                                   (keyword? (syntax-e #'kw))
                                   (loop #'formals)]
                                  [([id _] . formals)
                                   (cons #'id (loop #'formals))]
                                  [(id . formals)
                                   (cons #'id (loop #'formals))]))])
       #`(begin
           #,(datum->syntax
              stx
              (syntax-e #'(define-constructor (constructor arg ...)
                            body ... last-body))
              stx
              stx)
           (define (name . formals)
             (constructor arg ...))))]))

(define-syntax (let-fields stx)
  (syntax-case stx (capture-fixnum)
    [(_ self-id n () ctx body ...)
     #'(let () body ...)]
    [(_ self-id n ((capture-fixnum id) . captureds) ctx . bodys)
     (with-syntax ([id (datum->syntax #'ctx (syntax-e #'id))]
                   [n+1 #`#,(+ (syntax-e #'n) 1)])
       #'(let-syntax ([id (make-fixnum-transformer #'(vector*-ref self-id n))])
           (let-fields self-id n+1 captureds ctx . bodys)))]
    [(_ self-id n (id . captureds) ctx . bodys)
     (with-syntax ([id (datum->syntax #'ctx (syntax-e #'id))]
                   [n+1 #`#,(+ (syntax-e #'n) 1)])
       #'(let-syntax ([id (make-set!-transformer
                           (lambda (stx)
                             (syntax-case stx (set!)
                               [(set! _ r) #'(vector*-set! self-id n r)]
                               [(_ arg (... ...)) #'((vector*-ref self-id n) arg (... ...))]
                               [_ #'(vector*-ref self-id n)])))])
           (let-fields self-id n+1 captureds ctx . bodys)))]))

(define-syntax (let-methods stx)
  (syntax-case stx ()
    [(_ self-id () ctx body ...)
     #'(let () body ...)]
    [(_ self-id ((id tmp-id) . methods) ctx . bodys)
     (with-syntax ([id (datum->syntax #'ctx (syntax-e #'id))])
       #'(let-syntax ([id (make-method-transformer #'self-id #'tmp-id)])
           (let-methods self-id methods ctx . bodys)))]))

;; ----------------------------------------

(module+ test
  (define-constructor (c x y)
    (define a 12)
    (define-fixnum z 120)
    (define (f)
      (set! z 130)
      (set! y 8)
      (list a x (g)))
    (define (g)
      (list y z))
    (values (method f)
            self))

  (define-values (f f-self) (c 1 2))
  (f f-self))

