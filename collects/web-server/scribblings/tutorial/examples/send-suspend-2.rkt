#lang web-server/insta

(define (start request)
  (show-counter 0))

;; show-counter: number -> html-response
(define (show-counter n)
  (send/suspend/dispatch
   (lambda (make-url)
     `(html (head (title "Counting example"))
            (body 
             (a ((href ,(make-url 
                         (lambda (request)
                           (show-counter (+ n 1))))))
                ,(number->string n)))))))
