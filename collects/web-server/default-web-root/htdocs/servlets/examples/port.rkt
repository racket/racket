#lang racket/base
(require web-server/servlet
         racket/list)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  (response
   200 #"Okay" (current-seconds) #"text/html" empty
   (λ (op)
     (display #<<END
<html><body><p>Hello, Web!</p></body></html>
END
              op))))
