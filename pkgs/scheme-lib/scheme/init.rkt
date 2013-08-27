#lang scheme

(require "enter.rkt"
         "help.rkt")

;; Set the printer:
(current-print (let ([pretty-printer
                      (lambda (v)
                        (unless (void? v)
                          (pretty-print v)))])
                 pretty-printer))

(provide (all-from-out scheme
                       "enter.rkt"
                       "help.rkt"))
