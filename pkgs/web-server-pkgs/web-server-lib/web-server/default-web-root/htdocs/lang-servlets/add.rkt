#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

(define printf void)

;; get-number-from-user: string -> number
;; ask the user for a number
(define (get-number msg)
  (printf "gn ~a\n" msg)
  (let* ([req
          (send/suspend/url
           (lambda (k-url)
             (printf "ssu\n")
             (response/xexpr
              `(html (head (title ,(format "Get ~a number" msg)))
                     (body
                      (form ([action ,(url->string k-url)]
                             [method "post"]
                             [enctype "application/x-www-form-urlencoded"])
                            ,(format "Enter the ~a number to add: " msg)
                            (input ([type "text"] [name "number"] [value ""]))
                            (input ([type "submit"]))))))))]
         [num (string->number
               (bytes->string/utf-8
                (binding:form-value
                 (bindings-assq #"number" 
                                (request-bindings/raw req)))))])
    (printf "gn ~a ~a\n" msg num)
    num))

(define (start initial-request)
  (printf "after s-s\n")
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Final Page")
           (p ,(format "The answer is ~a"
                       (+ (get-number "first") (get-number "second"))))))))
