#lang web-server/insta

(define (start req)
  (response 200 #"Okay" (current-seconds)
            #"text/html" empty
            (λ (op) (write-bytes #"PONG" op))))
