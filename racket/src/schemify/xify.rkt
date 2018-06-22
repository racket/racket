#lang racket/base
(require "wrap.rkt"
         "match.rkt")

;; Given a closed (except for primitives) `lambda` or
;; `case-lambda` form `e`, replace every local variable
;; with x<n> for the smallest number <n> that will work.

(provide xify)

(define (xify e)
  (define (xify e env)
    (reannotate
     e
     (match e
       [`(lambda ,ids . ,body)
        (define-values (new-ids new-env) (xify-ids ids env))
        `(lambda ,new-ids . ,(xify-body body new-env))]
       [`(case-lambda ,clauses ...)
        `(case-lambda . ,(for/list ([clause (in-list clauses)])
                           (cdr (xify (cons 'lambda clause) env))))]
       [`(let ([,ids ,rhss] ...) . ,body)
        (xify-let 'let ids rhss body env)]
       [`(letrec ([,ids ,rhss] ...) . ,body)
        (xify-let 'letrec ids rhss body env)]
       [`(letrec* ([,ids ,rhss] ...) . ,body)
        (xify-let 'letrec* ids rhss body env)]
       [`(quote ,v) e]
       ;; Although this next group could be covered by `xify-body`,
       ;; they seem common enough to handle faster as special cases
       [`(begin . ,body)
        `(begin . ,(xify-body body env))]
       [`(if ,tst ,thn ,els)
        `(if ,(xify tst env) ,(xify thn env) ,(xify els env))]
       [`(with-continuation-mark ,key ,val ,body)
        `(with-continuation-mark ,(xify key env) ,(xify val env) ,(xify body env))]
       [`(set! ,id ,rhs)
        `(set! ,(xify id env) ,(xify rhs env))]
       ;; Catch-all for other forms, which we can treat like applications
       [`(,_ . ,_) (xify-body e env)]
       [`,v
        (define u-v (unwrap v))
        (cond
          [(symbol? u-v)
           (define x (hash-ref env u-v #f))
           (if x
               (reannotate v x)
               v)]
          [else v])])))

  (define (xify-body es env)
    (for/list ([e (in-wrap-list es)])
      (xify e env)))

  (define (xify-let form ids rhss body env)
    (define-values (new-ids new-env) (xify-ids ids env))
    `(,form ,(for/list ([new-id (in-list new-ids)]
                        [rhs (in-list rhss)])
               `[,new-id ,(xify rhs (if (eq? form 'let) env new-env))])
            . ,(xify-body body new-env)))

  (define (xify-ids ids env)
    (cond
      [(pair? ids)
       (define u-id (unwrap (car ids)))
       (define x (or (hash-ref env u-id #f)
                     (string->symbol (string-append "x" (number->string (hash-count env))))))
       (define-values (rest-xs rest-env) (xify-ids (cdr ids) (hash-set env u-id x)))
       (values (cons x rest-xs) rest-env)]
      [(null? ids) (values '() env)]
      [else
       (define-values (xs new-env) (xify-ids (list ids) env))
       (values (car xs) new-env)]))

  (xify e #hasheq()))

(module+ test
  (define-syntax-rule (test a b)
    (let ([v a])
      (unless (equal? v b) (error 'test "failed: ~s => ~e" 'a v))))
  
  (test (xify '(let ([apple 1]) apple))
        '(let ([x0 1]) x0))
  (test (xify '(let ([apple 1] [banana 2]) apple))
        '(let ([x0 1] [x1 2]) x0))
  (test (xify '(let ([apple 1]
                     [banana 2])
                 (let ([apple 1]
                       [banana 2])
                   apple)))
        '(let ([x0 1]
               [x1 2])
           (let ([x0 1]
                 [x1 2])
             x0)))
  (test (xify '(+ (let ([apple 1]) apple)
                  (let ([banana 2]) banana)))
        '(+ (let ([x0 1]) x0)
            (let ([x0 2]) x0)))
  (test (xify '(lambda (a b c)
                 (list c b a)))
        '(lambda (x0 x1 x2)
           (list x2 x1 x0)))
  (test (xify '(case-lambda
                 [(a b c) (list c b a)]
                 [(x . y) (list x y)]))
        '(case-lambda
           [(x0 x1 x2)
            (list x2 x1 x0)]
           [(x0 . x1) (list x0 x1)]))
  (test (xify '(lambda (a b c)
                 (if a
                     (begin b c)
                     (with-continuation-mark a b c))
                 (set! a b)
                 (list 'a 'b 'c 1 2 3)
                 (#%app a b c)))
        '(lambda (x0 x1 x2)
           (if x0
               (begin x1 x2)
               (with-continuation-mark x0 x1 x2))
           (set! x0 x1)
           (list 'a 'b 'c 1 2 3)
           (#%app x0 x1 x2))))
