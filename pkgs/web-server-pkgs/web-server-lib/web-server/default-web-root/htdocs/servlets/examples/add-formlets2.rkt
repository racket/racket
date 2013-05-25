#lang racket/base
(require web-server/servlet
         web-server/formlets)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

; request-number : str -> num
(define (request-number which-number)
  (send/formlet
   (formlet
    (#%# "Enter the " ,which-number " number to add: "
         ,{input-int . => . the-number}
         (input ([type "submit"] [name "enter"] [value "Enter"])))
    the-number)
   #:method
   "POST"
   #:wrap
   (lambda (f-expr)
     `(html (head (title "Enter a Number to Add"))
            (body ([bgcolor "white"])
                  ,f-expr)))))

(define (start initial-request)
  (response/xexpr
   `(html (head (title "Sum"))
          (body ([bgcolor "white"])
                (p "The answer is "
                   ,(number->string (+ (request-number "first") (request-number "second"))))))))
