#lang web-server/insta

;; start: request -> response
(define (start request)
  (send/suspend/dispatch
   (lambda (make-url)
     (response/xexpr
      `(html 
        (body 
         (a ((href ,(make-url link-1))) "Link 1")
         (a ((href ,(make-url link-2))) "Link 2")))))))


;; link-1: request -> response
(define (link-1 request)
  (response/xexpr
   "This is link-1"))


;; link-2: request -> response
(define (link-2 request)
  (response/xexpr
   "This is link-2"))
