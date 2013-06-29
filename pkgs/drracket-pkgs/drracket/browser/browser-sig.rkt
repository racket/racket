#lang racket
(require racket/unit
         "private/sig.rkt")

(provide browser^)

(define-signature browser^
  ((open hyper^)
   (open html-export^)
   (open bullet-export^)))
