#lang racket/base
(require racket/include)

(include "config.rktl")

'----------------------------------------

;; Key not found
'none-loop
(times
 (let loop ([i N] [a #f])
   (if (zero? i)
       a
       (call-with-immediate-continuation-mark
        'key
        (lambda (a)
          (loop (sub1 i) a))))))

;; Key found
'some-loop
(times
 (with-continuation-mark
  'key 'val
  (let loop ([i N] [a #f])
    (if (zero? i)
        a
        (call-with-immediate-continuation-mark
         'key
         (lambda (a)
           (loop (sub1 i) a)))))))
