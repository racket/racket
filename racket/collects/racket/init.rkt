#lang racket
(require racket/enter
         racket/help)

;; Set the printer:
(current-print (let ([pretty-printer
                      (lambda (v)
                        (unless (void? v)
                          (pretty-print v)))])
                 pretty-printer))

(provide (all-from-out racket
                       racket/enter
                       racket/help))
