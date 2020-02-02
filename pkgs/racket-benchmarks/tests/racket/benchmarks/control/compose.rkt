#lang racket/base
(require racket/include)

(include "config.rktl")

;; ----------------------------------------

'capture
(times
 (call-with-continuation-prompt
  (lambda ()
    (let loop ([i M] [k #f])
      (unless (zero? i)
        (loop (- i 1)
              (call-with-composable-continuation
               (lambda (k) k))))))))

'compose
(times
 (let ([one #f])
   (call-with-continuation-prompt
    (lambda ()
      (call-with-composable-continuation
       (lambda (k) (set! one k)))
      1))
   (let loop ([i M])
     (unless (zero? i)
       (loop (- i (one)))))))
