#lang racket/base
(require racket/place)

;; Try to catch a resource leak starting and finishing places.

(for ([i (in-range 256)])
  (printf "~s\n" i)
  (map place-wait
       (for/list ([i 4])
         (dynamic-place ''#%kernel 'list))))

(module+ test
  (module config info
    (define timeout 600)))
