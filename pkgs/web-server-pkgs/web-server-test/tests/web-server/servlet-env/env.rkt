#lang racket
(require web-server/servlet
         web-server/servlet-env)

; request-number : str -> num
(define (request-number which-number)
  (string->number
   (extract-binding/single
    'number
    (request-bindings (send/suspend (build-request-page which-number))))))

; build-request-page : str -> str -> response
(define (build-request-page which-number)
  (lambda (k-url)
    `(html (head (title "Enter a Number to Add"))
           (body ([bgcolor "white"])
                 (form ([action ,k-url] [method "post"])
                       "Enter the " ,which-number " number to add: "
                       (input ([type "text"] [name "number"] [value ""]))
                       (input ([type "submit"] [name "enter"] [value "Enter"])))))))
(define (start request)
  `(html (head (title "Sum"))
         (body ([bgcolor "white"])
               (p "The sum is "
                  ,(number->string (+ (request-number "first") (request-number "second")))))))

(serve/servlet start
               #:servlet-path "/")

(module test racket/base)
