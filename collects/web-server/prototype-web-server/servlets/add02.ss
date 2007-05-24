(module add02 (lib "lang.ss" "web-server" "prototype-web-server")
  (require (lib "url.ss" "net")
           (lib "request-structs.ss" "web-server"))
  (provide start)
  
  ;; XXX This demonstrates that if we hide the K in a query, it will be overridden.
  
  ;; get-number-from-user: string -> number
  ;; ask the user for a number
  (define (gn msg)
    (let ([req
           (send/suspend/url
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
  
  (define (start initial-request)
    `(html (head (title "Final Page"))
           (body
            (h1 "Final Page")
            (p ,(format "The answer is ~a"
                        (+ (gn "first") (gn "second"))))))))