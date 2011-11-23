#lang racket/base
(require web-server/http
         net/url)
(define interface-version 'stateless)
(provide start interface-version)

(define (start req)
  (let* ([uri (request-uri req)]
         [qry (url-query uri)])
    (cond
      [(assoc 'second qry)
       => (lambda (a-pair)
            (response/xexpr
             `(html (head (title "Answer Page"))
                    (body
                     (h1 "Answer Page")
                     (p ,(format "The answer is: ~a"
                                 (+ (string->number (cdr a-pair))
                                    (string->number (cdr (assoc 'first qry))))))))))]
      [(assoc 'first qry)
       => (lambda (a-pair)
            (response/xexpr
             `(html (head (title "Second Page"))
                    (body
                     (h1 "Second Page")
                     (form ([action ,(url->string uri)]
                            [method "get"] [enctype "application/x-www-form-urlencoded"])
                           "Enter the second number to add: "
                           (input ([type "hidden"] [name "first"] [value ,(cdr a-pair)]))
                           (input ([type "text"] [name "second"] [value ""]))
                           (input ([type "submit"] [name "enter"] [value "Enter"])))))))]
      [else
       (response/xexpr
        `(html (head (title "Hello"))
               (body
                (h1 "Hello World!")
                (form ([action ,(url->string uri)]
                       [method "get"] [enctype "application/x-www-form-urlencoded"])
                      "Enter the first number to add: "
                      (input ([type "text"] [name "first"] [value ""]))
                      (input ([type "submit"] [name "enter"] [value "Enter"]))))))])))
