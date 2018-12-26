#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "quoted.rkt")

;; The `linklet-bigger-than?` function is practically an S-expression
;; counter, but it parses expressions properly so it can stop at
;; `quote`.

(provide linklet-bigger-than?)

(define (linklet-bigger-than? e size serializable?)

  (define (leftover-size e size)
    (cond
      [(size . <= . 0) 0]
      [else
       (match e
         [`(begin . ,body)
          (body-leftover-size body (sub1 size))]
         [`(define-values ,_ ,rhs)
          (leftover-size rhs (sub1 size))]
         [`(lambda ,_ . ,body)
          (body-leftover-size body (sub1 size))]
         [`(case-lambda [,_ . ,bodys] ...)
          (body-leftover-size bodys (sub1 size))]
         [`(let-values ([,_ ,rhss] ...)
             . ,body)
          (body-leftover-size (cons rhss body) (sub1 size))]
         [`(letrec-values ([,_ ,rhss] ...)
             . ,body)
          (body-leftover-size (cons rhss body) (sub1 size))]
         [`(if ,tst ,thn ,els)
          (leftover-size els (leftover-size thn (leftover-size tst (sub1 size))))]
         [`(with-continuation-mark ,key ,val ,body)
          (leftover-size body (leftover-size val (leftover-size key (sub1 size))))]
         [`(begin0 . ,body)
          (body-leftover-size body (sub1 size))]
         [`(quote ,v) (if (and serializable?
                               (lift-quoted? v #f #t))
                          ;; pessimistically assume that full
                          ;; strcuture must be lifted for
                          ;; serialization:
                          (s-expr-leftover-size v size)
                          (sub1 size))]
         [`(set! ,id ,rhs) (leftover-size rhs (sub1 size))]
         [`(#%variable-reference . ,_) (sub1 size)]
         [`(,_ . ,_) (body-leftover-size e size)]
         [`,_ (sub1 size)])]))

  (define (body-leftover-size body size)
    (for/fold ([size size]) ([e (in-wrap-list body)]
                             #:break (size . <= . 0))
      (leftover-size e size)))

  (define (s-expr-leftover-size v size)
    (cond
      [(size . <= . 0) 0]
      [(pair? v) (s-expr-leftover-size
                  (cdr v)
                  (s-expr-leftover-size (car v) (sub1 size)))]
      [(box? v) (s-expr-leftover-size (unbox v) (sub1 size))]
      [(vector? v) (for/fold ([size (sub1 size)]) ([v (in-vector v)]
                                                   #:break (size . <= . 0))
                     (s-expr-leftover-size v size))]
      [(prefab-struct-key v)
       (s-expr-leftover-size (struct->vector v) size)]
      [(hash? v)
       (for/fold ([size (sub1 size)]) ([(k v) (in-hash v)]
                                       #:break (size . <= . 0))
         (s-expr-leftover-size v (s-expr-leftover-size k size)))]
      [else (sub1 size)]))

  (match e
    [`(linklet ,_ ,_ . ,body)
     ((body-leftover-size body size) . <= . 0)]))
