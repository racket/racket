#lang racket/base
(require web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (send/back
   (response/xexpr
    `(html (head (title "Current Directory Page"))
           (body
            (h1 "Current Directory Page")
            (p "The current directory is: " (em ,(path->string (current-directory)))))))))
