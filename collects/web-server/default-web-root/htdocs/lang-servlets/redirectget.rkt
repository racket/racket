#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

(define (start request)
  (define request 
    (send/suspend
     (λ (url)
       `(html
         (head)
         (body
          (form ((action ,url))
                (input ((type "submit") (value "submit")))))))))
  (redirect/get)
  (send/suspend
   (λ (_)
     `(html "bye"))))