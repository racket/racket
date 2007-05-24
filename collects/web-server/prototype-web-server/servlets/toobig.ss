(module toobig (lib "lang.ss" "web-server" "prototype-web-server")
  (require (lib "url.ss" "net")
           (lib "servlet-helpers.ss" "web-server" "private"))
  (provide start)
  
  (define (get-n)
    (let ([req
           (send/suspend/url
            (lambda (k-url)
              `(html (head (title "How many bytes?"))
                     (body
                      (form ([action ,(url->string k-url)]
                             [method "POST"]
                             [enctype "application/x-www-form-urlencoded"])
                            "How many bytes? (Try 1024)"
                            (input ([type "text"] [name "number"] [value ""]))
                            (input ([type "submit"])))))))])
      (string->number
       (extract-binding/single
        `number
        (request-bindings req)))))
  
  (define (get-bytes)
    (let* ([the-bytes
            (make-bytes (get-n) (char->integer #\!))]
           [req
            (send/suspend/url
             (lambda (k-url)
               `(html (head (title "How are these bytes?"))
                      (body
                       (h3 ,(bytes->string/utf-8 the-bytes))
                       (a ([href ,(url->string k-url)]) "OK!")))))])
      the-bytes))
  
  (define (start initial-request)
    `(html (head (title "You got here!"))
           (body
            (h1 ,(bytes->string/utf-8 (get-bytes)))))))