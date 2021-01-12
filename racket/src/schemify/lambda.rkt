#lang racket/base
(require "wrap.rkt"
         "match.rkt")

(provide lambda?
         extract-lambda
         lambda-arity-mask)

;; ----------------------------------------

;; Recognize forms that produce plain procedures; expression can be
;; pre- or post-schemify
(define (lambda? v #:simple? [simple? #f])
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(letrec-values ([(,id) ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(let ([,id ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(letrec* ([,id ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(let-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(letrec-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(begin ,body) (lambda? body #:simple? simple?)]
    [`(begin . ,bodys) (and (not simple?)
                            (let loop ([bodys bodys])
                              (if (null? (cdr bodys))
                                  (lambda? (car bodys) #:simple? simple?)
                                  (loop (cdr bodys)))))]
    [`(values ,body) (lambda? body #:simple? simple?)]
    [`,_ #f]))

(define (let-lambda? id rhs body #:simple? simple?)
  (or (and (wrap-eq? id body) (lambda? rhs #:simple? simple?))
      (and (not simple?)
           (lambda? body #:simple? simple?))))

;; Extract procedure from a form on which `lambda?` produces true
(define (extract-lambda v)
  (match v
    [`(lambda . ,_) (values v #t)]
    [`(case-lambda . ,_) (values v #t)]
    [`(let-values ([(,id) ,rhs]) ,body) (extract-let-lambda #f id rhs body)]
    [`(letrec-values ([(,id) ,rhs]) ,body) (extract-let-lambda #t id rhs body)]
    [`(let ([,id ,rhs]) ,body) (extract-let-lambda #f id rhs body)]
    [`(letrec* ([,id ,rhs]) ,body) (extract-let-lambda #t id rhs body)]
    [`(let-values ,_ ,body) (extract-lambda* body)]
    [`(letrec-values ,_ ,body) (extract-lambda* body)]
    [`(let ,_ ,body) (extract-lambda* body)]
    [`(letrec* ,_ ,body) (extract-lambda* body)]
    [`(begin ,body) (extract-lambda body)]
    [`(begin . ,bodys) (let loop ([bodys bodys])
                         (if (null? (cdr bodys))
                             (extract-lambda* (car bodys))
                             (loop (cdr bodys))))]
    [`(values ,body) (extract-lambda body)]))

(define (extract-let-lambda rec? id rhs body)
  (if (wrap-eq? id body)
      (if rec?
          (extract-lambda* rhs)
          (extract-lambda rhs))
      (extract-lambda* body)))

(define (extract-lambda* v)
  (define-values (lam inlinable?) (extract-lambda v))
  (values lam #f))

(define (lambda-arity-mask v)
  (match v
    [`(lambda ,args . ,_) (args-arity-mask args)]
    [`(case-lambda [,argss . ,_] ...)
     (for/fold ([mask 0]) ([args (in-list argss)])
       (bitwise-ior mask (args-arity-mask args)))]))

(define (args-arity-mask args)
  (cond
    [(wrap-null? args) 1]
    [(wrap-pair? args)
     (arithmetic-shift (args-arity-mask (wrap-cdr args)) 1)]
    [else -1]))
