#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

(define softie
  (soft-state
   "submit"))

;; get-number-from-user: string -> number
;; ask the user for a number
(define (gn msg)
  (let ([req
         (send/suspend/url
          (lambda (k-url)
            (response/xexpr
             `(html (head (title ,(format "Get ~a number" msg)))
                    (body
                     (form ([action ,(url->string k-url)]
                            [method "get"]
                            [enctype "application/x-www-form-urlencoded"])
                           ,(format "Enter the ~a number to add: " msg)
                           (input ([type "text"] [name "number"] [value ""]))
                           (input ([type ,(soft-state-ref softie)]))))))))])
    (string->number
     (cdr (assoc 'number (url-query (request-uri req)))))))

(define (start initial-request)
  (response/xexpr
   `(html (head (title "Final Page"))
          (body
           (h1 "Final Page")
           (p ,(format "The answer is ~a"
                       (+ (gn "first") (gn "second"))))))))
