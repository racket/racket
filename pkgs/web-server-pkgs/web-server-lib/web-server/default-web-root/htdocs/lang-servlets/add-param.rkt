#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

(define msg (make-parameter "unknown"))
(define printf void)

(define (gn)
  (printf "gn ~a\n" (msg))
  (let* ([req
          (send/suspend/url
           (lambda (k-url)
             (printf "ssu ~S\n" (msg))
             (response/xexpr
              `(html (head (title ,(format "Get ~a number" (msg))))
                     (body
                      (form ([action ,(url->string k-url)]
                             [method "post"]
                             [enctype "application/x-www-form-urlencoded"])
                            ,(format "Enter the ~a number to add: " (msg))
                            (input ([type "text"] [name "number"] [value ""]))
                            (input ([type "submit"]))))))))]
         [num (string->number
               (bytes->string/utf-8
                (binding:form-value
                 (bindings-assq #"number" 
                                (request-bindings/raw req)))))])
    (printf "gn ~a ~a\n" (msg) num)
    num))

(define (start initial-request)
  (printf "after s-s\n")
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Final Page")
           (p ,(format "The answer is ~a"
                       (+ (parameterize ([msg "first"])
                            (gn))
                          (parameterize ([msg "second"])
                            (gn)))))))))
