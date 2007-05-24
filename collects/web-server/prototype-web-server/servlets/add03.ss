(module add03 (lib "lang.ss" "web-server" "prototype-web-server")
  (require (lib "url.ss" "net")
           (lib "servlet-helpers.ss" "web-server" "private"))
  (provide start)
  
  ;; get-number-from-user: string -> number
  ;; ask the user for a number
  (define (gn msg)
    (let ([req
           (send/suspend/hidden
            (lambda (ses-url k-hidden)
              `(hmtl (head (title ,(format "Get ~a number" msg)))
                     (body
                      (form ([action ,(url->string ses-url)]
                             [method "post"]
                             [enctype "application/x-www-form-urlencoded"])
                            ,(format "Enter the ~a number to add: " msg)
                            (input ([type "text"] [name "number"] [value ""]))
                            (input ([type "submit"]))
                            ,k-hidden)))))])
      (string->number
       (extract-binding/single
        'number
        (request-bindings req)))))
  
  (define (start initial-request)
    `(html (head (title "Final Page"))
           (body
            (h1 "Final Page")
            (p ,(format "The answer is ~a"
                        (+ (gn "first") (gn "second"))))))))