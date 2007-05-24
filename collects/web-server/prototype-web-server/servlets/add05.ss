(module add05 (lib "persistent-web-interaction.ss" "web-server" "prototype-web-server")
  (require (lib "url.ss" "net")
           (lib "servlet-helpers.ss" "web-server" "private"))
  
  ;; get-number-from-user: string -> number
  ;; ask the user for a number
  (define (gn msg)
    (extract-proc/url
     (send/suspend/url
      (lambda (k-url)
        `(hmtl (head (title ,(format "Get ~a number" msg)))
               (body
                (form ([action ,(url->string 
                                 (embed-proc/url 
                                  k-url
                                  (lambda (req)
                                    (string->number
                                     (extract-binding/single
                                      'number
                                      (request-bindings req))))))]
                       [method "post"]
                       [enctype "application/x-www-form-urlencoded"])
                      ,(format "Enter the ~a number to add: " msg)
                      (input ([type "text"] [name "number"] [value ""]))
                      (input ([type "submit"])))))))))  
  
  (let ([initial-request (start-servlet)])
    `(html (head (title "Final Page"))
           (body
            (h1 "Final Page")
            (p ,(format "The answer is ~a" (+ (gn "first") (gn "second"))))))))