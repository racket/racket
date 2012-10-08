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

(define (honu . input)
  (with-input-from-string (apply string-append input)
    (lambda ()
      ((honu-eval) (honu-read-syntax)))))

(define-syntax-rule (honu-tests checks ...)
  (parameterize ([honu-eval (make-honu-evaluator)]) checks ...))

(honu-tests
  (check-equal? @honu{1} 1)
  (check-equal? @honu{5} 5)
  (check-equal? @honu{1 + 1} (+ 1 1))
  (check-equal? @honu{1 + 2 * 3} (+ 1 (* 2 3)))
  (check-equal? @honu{3 * 2 + 1} (+ (* 3 2) 1))
  (check-equal? @honu{1 + 4 ^ 2 * 3} (+ 1 (* (expt 4 2) 3)))
  (check-equal? @honu{1 + 4 ^ 3 ^ 2} (+ 1 (expt 4 (expt 3 2))))
  (check-equal? @honu{4 ^ 3 ^ 2 + 1} (+ (expt 4 (expt 3 2)) 1))
  
  (check-equal? @honu{
                      var n = 5
                      cond
                      n < 10: 'x1,
                      n > 10: 'x2
                }
             'x1)

  (check-equal? @honu{
                     if (2 > 1)
                       1
                     else
                     0
                }
             1)

  (check-equal? @honu{
                  var x = 12
                  x *= 2
                  x -= 5
                  x
                }
                (- (* 12 2) 5))

  (check-equal? @honu{[x + 1: x = [1, 2, 3]]} '(2 3 4))
  (check-equal? @honu{[x + y: x = [1, 2, 3], y = [4, 5, 6]]} '(5 7 9))
  )

(honu-tests
  (check-equal? @honu{function foo(x){
                            x * 2
                          }
                          foo(5)
                          }
             10)

  (check-equal? @honu{
                  unary_operator u1 4 function(x){ x * 2}
                  u1 8
                }

                (* 8 2))

  (check-equal? @honu{
                  unary_operator u1 4 function(x){ x * 2}
                  2 + u1 8 * 3
                }

                (+ 2 (* 3 (* 8 2))))

  (check-equal? @honu{
                  binary_operator b1 4 'left function(left, right){ left + right * 2}
                  2 b1 4
                }

                (+ 2 (* 2 4)))
  )
