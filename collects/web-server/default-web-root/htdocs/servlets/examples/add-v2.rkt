#lang racket/base
(require web-server/servlet
         web-server/managers/timeouts)
(provide (all-defined-out))
(define interface-version 'v2)
(define manager
  (create-timeout-manager
   (lambda _ (response/xexpr `(html (body "Expired"))))
   360 360))

; request-number : str -> num
(define (request-number which-number)
  (string->number
   (extract-binding/single
    'number
    (request-bindings (send/suspend (build-request-page which-number))))))

; build-request-page : str -> str -> response
(define (build-request-page which-number)
  (lambda (k-url)
    (response/xexpr
     `(html (head (title "Enter a Number to Add"))
            (body ([bgcolor "white"])
                  (form ([action ,k-url] [method "post"])
                        "Enter the " ,which-number " number to add: "
                        (input ([type "text"] [name "number"] [value ""]))
                        (input ([type "submit"] [name "enter"] [value "Enter"]))))))))

(define (start initial-request)
  (response/xexpr
   `(html (head (title "Sum"))
          (body ([bgcolor "white"])
                (p "The answer is "
                   ,(number->string (+ (request-number "first") (request-number "second"))))))))
