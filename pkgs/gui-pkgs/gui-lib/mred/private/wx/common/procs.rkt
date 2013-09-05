#lang racket/base
(require "../../syntax.rkt")

(provide
 label->plain-label)

(define/top (label->plain-label [string? s])
  (regexp-replace* #rx"&(.)" 
                   (regexp-replace 
                    #rx" *[(]&.[)] *"
                    (regexp-replace #rx"\t.*$" s "") 
                    "")
                   "\\1"))

