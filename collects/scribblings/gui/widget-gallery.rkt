#lang racket/base
(require racket/string
         "common.rkt")

(provide showcase-widget)

(define-syntax-rule (showcase-widget widget code ...)
  (begin
    (racketlink widget
                (image #:suffixes
                       (list ".png")
                       (string-append "image/"
                                      (string-trim (symbol->string 'widget)
                                                   "%"
                                                   #:left? #f))))
    (racketblock code ...)))
