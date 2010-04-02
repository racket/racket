#lang racket
(require scheme/enter
         scheme/help
         "private/runtime.ss")

;; Set the printer:
(current-print (let ([pretty-printer
                      (lambda (v)
                        (unless (void? v)
                          (pretty-print v)))])
                 pretty-printer))

(provide (all-from-out racket
                       scheme/enter
                       scheme/help))
