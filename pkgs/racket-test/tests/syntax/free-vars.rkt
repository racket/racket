#lang racket
(require syntax/free-vars
         rackunit)

(parameterize ([current-namespace (make-base-namespace)])
  (define (check stx)
    (syntax-case (expand stx) (quote)
      [(let-vals bindings (quote free) body) 
       (unless (andmap free-identifier=?
                       (syntax->list #'free) 
                       (free-vars #'body))
         (error "wrong answer: ~e" stx))]))
  (check #'(let ([x 1]) 
             '(x)
             x))
  (check #'(let ([x 1]
                 [y 2])
             '(x y)
             (x y)))
  (check #'(let ([x 1]
                 [y 2])
             '(y x)
             (y x)))
  (check #'(let ([x 1]
                 [y 2])
             '(x y)
             (let-syntax ([ex (syntax-rules () [(foo) x])])
               (list x y (ex)))))
  (check #'(let ([x 1]
                 [y 2])
             '(x y)
             (let-syntax ([ex (syntax-rules () [(foo) x])])
               (list (ex) y x))))
  (check #'(let ([x 1]
                 [y 2])
             '(x)
             (let-syntax ([ex (syntax-rules () [(foo) x])])
               (lambda (z) (ex)))))
  (check #'(let ([x 1]
                 [y 2])
             '(x)
             (let ([y 3])
               (list x y)))))

(check-equal? (free-vars (expand #'(+ 1 2)))
              '())
(check-pred (lambda (x) (free-identifier=? x #'+))
            (first (free-vars (expand #'(+ 1 2))
                              #:module-bound? #t)))
