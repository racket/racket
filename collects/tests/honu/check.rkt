#lang at-exp racket/base

(require racket/sandbox
         racket/port
         rackunit
         honu/core/read)

(define honu-eval (make-parameter (lambda args (error 'honu-eval "set the honu evaluator"))))
(define (make-honu-evaluator)
  (call-with-trusted-sandbox-configuration
    (lambda ()
      (make-evaluator 'honu))))

(define (honu input)
  (with-input-from-string input
    (lambda ()
      ((honu-eval) (honu-read-syntax)))))

(define input string-append)

(define-syntax-rule (honu-tests checks ...)
  (parameterize ([honu-eval (make-honu-evaluator)]) checks ...))

(honu-tests
  (check-equal? (honu @input{1}) 1)
  (check-equal? (honu @input{5}) 5)
  (check-equal? (honu @input{1 + 1}) (+ 1 1))
  (check-equal? (honu @input{1 + 2 * 3}) (+ 1 (* 2 3)))
  (check-equal? (honu @input{3 * 2 + 1}) (+ (* 3 2) 1))
  (check-equal? (honu @input{1 + 4 ^ 2 * 3}) (+ 1 (* (expt 4 2) 3)))
  (check-equal? (honu @input{1 + 4 ^ 3 ^ 2}) (+ 1 (expt 4 (expt 3 2))))
  (check-equal? (honu @input{4 ^ 3 ^ 2 + 1}) (+ (expt 4 (expt 3 2)) 1))
  
  (check-equal? (honu @input{
                      var n = 5
                      cond
                      n < 10: 'x1,
                      n > 10: 'x2
                    })
             'x1)

  (check-equal? (honu @input{
                     if (2 > 1)
                       1
                     else
                     0
                   })
             1)

  (check-equal? (honu @input{[x + 1: x = [1, 2, 3]]}) '(2 3 4))
  (check-equal? (honu @input{[x + y: x = [1, 2, 3], y = [4, 5, 6]]}) '(5 7 9))
  )

(honu-tests
  (check-equal? (honu @input{function foo(x){
                            x * 2
                          }
                          foo(5)
                          })
             10))
