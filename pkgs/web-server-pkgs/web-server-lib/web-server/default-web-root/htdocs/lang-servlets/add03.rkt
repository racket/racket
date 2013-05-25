#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

;; get-number-from-user: string -> number
;; ask the user for a number
(define (gn msg)
  (let ([req
         (send/suspend/hidden
          (lambda (ses-url k-hidden)
            (response/xexpr
             `(html (head (title ,(format "Get ~a number" msg)))
                    (body
                     (form ([action ,(url->string ses-url)]
                            [method "post"]
                            [enctype "application/x-www-form-urlencoded"])
                           ,(format "Enter the ~a number to add: " msg)
                           (input ([type "text"] [name "number"] [value ""]))
                           (input ([type "submit"]))
                           ,k-hidden))))))])
    (string->number
     (bytes->string/utf-8
      (binding:form-value
       (bindings-assq #"number" 
                      (request-bindings/raw req)))))))

(define (start initial-request)
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Final Page")
           (p ,(format "The answer is ~a"
                       (+ (gn "first") (gn "second"))))))))
