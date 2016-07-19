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

;; Set the default replt to XREPL
(when (collection-file-path "main.rkt" "xrepl"
                            #:fail (lambda _ #f))
  (dynamic-require 'xrepl #f))
