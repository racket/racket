(module add02 "../web-interaction.ss"
  (require (lib "url.ss" "net")
           (lib "request.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server"))
  
  ;; get-number-from-user: string -> number
  ;; ask the user for a number
  (define (gn msg)
    (let ([req
           (send/suspend
            (lambda (k-url)
              `(hmtl (head (title ,(format "Get ~a number" msg)))
                     (body
                      (form ([action ,(url->string k-url)]
                             [method "get"]
                             [enctype "application/x-www-form-urlencoded"])
                            ,(format "Enter the ~a number to add: " msg)
                            (input ([type "text"] [name "number"] [value ""]))
                            (input ([type "submit"])))))))])
      (string->number
       (cdr (assoc 'number (url-query (request-uri req)))))))
  
  (let ([initial-request (start-servlet)])
    `(html (head (title "Final Page"))
           (body
            (h1 "Final Page")
            (p ,(format "The answer is ~a"
                        (+ (gn "first") (gn "second"))))))))