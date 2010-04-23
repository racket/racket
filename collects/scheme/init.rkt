#lang scheme

(require "enter.ss"
         "help.ss")

;; Set the printer:
(current-print (let ([pretty-printer
                      (lambda (v)
                        (unless (void? v)
                          (pretty-print v)))])
                 pretty-printer))

(provide (all-from-out scheme
                       "enter.ss"
                       "help.ss"))


