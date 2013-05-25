#lang scheme
(require net/cookie
         tests/eli-tester)

(define (set-when-true fn val)
  (if val
      (λ (c) (fn c val))
      (λ (c) c)))

(define (make-cookie name val)
  ((lambda (x)
     ((set-when-true cookie:add-comment #f)
      x))
   (set-cookie name val)))

(test
 (cookie? (make-cookie "name" "value")))
