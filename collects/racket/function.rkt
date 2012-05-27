#lang racket/base

(require (for-syntax racket/base syntax/name))

(provide identity const thunk thunk* negate curry curryr)

(define (identity x) x)

(define (const c)
  (define (const . _) c)
  (make-keyword-procedure const const))

(define-syntax (thunk stx)
  (syntax-case stx ()
    [(_ body0 body ...) (syntax/loc stx (lambda () body0 body ...))]))

(define-syntax (thunk* stx)
  (syntax-case stx ()
    [(_ body0 body ...)
     (with-syntax ([proc (syntax-property
                          (syntax/loc stx
                            ;; optimize 0- and 1-argument cases
                            (case-lambda [() body0 body ...]
                                         [(x) (th)] [xs (th)]))
                          'inferred-name (syntax-local-infer-name stx))])
       (syntax/loc stx
         (letrec ([th proc])
           (make-keyword-procedure (lambda (_1 _2 . _3) (th)) proc))))]))

(define (negate f)
  (unless (procedure? f) (raise-argument-error 'negate "procedure?" f))
  (let-values ([(arity) (procedure-arity f)] [(_ kwds) (procedure-keywords f)])
    (case (and (null? kwds) arity) ; optimize some simple cases
      [(0) (lambda () (not (f)))]
      [(1) (lambda (x) (not (f x)))]
      [(2) (lambda (x y) (not (f x y)))]
      [else (compose1 not f)]))) ; keyworded or more args => just compose

(define (make-curry right?)
  ;; The real code is here
  (define (curry* f args kws kvs)
    (unless (procedure? f)
      (raise-argument-error (if right? 'curryr 'curry) "procedure?" f))
    (let* ([arity (procedure-arity f)]
           [max-arity (cond [(integer? arity) arity]
                            [(arity-at-least? arity) #f]
                            [(ormap arity-at-least? arity) #f]
                            [else (apply max arity)])]
           [n (length args)])
      (define (loop args n)
        (cond
          [(procedure-arity-includes? f n)
           (if (null? kws) (apply f args) (keyword-apply f kws kvs args))]
          [(and max-arity (n . > . max-arity))
           (apply raise-arity-error f arity args)]
          [else
           (letrec [(curried
                     (case-lambda
                       [() curried] ; return itself on zero arguments
                       [more (loop (if right?
                                     (append more args) (append args more))
                                   (+ n (length more)))]))]
             curried)]))
      ;; take at least one step if we can continue (there is a higher arity)
      (if (equal? n max-arity)
        (if (null? kws) (apply f args) (keyword-apply f kws kvs args))
        (letrec ([curried
                  (lambda more
                    (let ([args (if right?
                                  (append more args) (append args more))])
                      (loop args (+ n (length more)))))])
          curried))))
  ;; curry is itself curried -- if we get args then they're the first step
  (define curry
    (case-lambda [(f) (define (curried . args) (curry* f args '() '()))
                      curried]
                 [(f . args) (curry* f args '() '())]))
  (make-keyword-procedure (lambda (kws kvs f . args) (curry* f args kws kvs))
                          curry))

(define curry  (make-curry #f))
(define curryr (make-curry #t))
