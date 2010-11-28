#lang web-server
(provide interface-version start) 
(define interface-version 'stateless)

(define softie
  (soft-state
   (printf "Doing a long computation...\n")
   (sleep 1)
   5))

(define (start req)
  (soft-state-ref softie)
  (printf "Done\n")
  (start
   (send/suspend
    (lambda (k-url)
      (response/xexpr
       `(html (body (a ([href ,k-url]) "Done"))))))))
