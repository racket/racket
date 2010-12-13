#lang racket/base
(require web-server/servlet
         web-server/page)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (request-number which-number)
  (let/ec esc
    (page
     (response/xexpr
      `(html (head (title "Enter a Number to Add"))
             (body ([bgcolor "white"])
                   (form ([action ,(embed/url
                                    (lambda/page ()
                                                 (esc
                                                  (string->number
                                                   (get-binding 'number)))))]
                          [method "post"])
                         "Enter the " ,which-number " number to add: "
                         (input ([type "text"] [name "number"] [value ""]))
                         (input ([type "submit"] [name "enter"] [value "Enter"])))))))))

(define/page (start)
  (response/xexpr
   `(html (head (title "Sum"))
          (body ([bgcolor "white"])
                (p "The answer is "
                   ,(number->string (+ (request-number "first") (request-number "second"))))))))
