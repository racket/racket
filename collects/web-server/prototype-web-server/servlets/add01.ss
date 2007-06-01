(module add01 mzscheme
  (require (lib "request-structs.ss" "web-server" "private")
           (lib "url.ss" "net"))
  (provide start)

  (define (start req)
    (let* ([uri (request-uri req)]
           [qry (url-query uri)])
      (cond
        [(assoc 'second qry)
         => (lambda (a-pair)
              `(html (head (title "Answer Page"))
                     (body
                      (h1 "Answer Page")
                      (p ,(format "The answer is: ~a"
                                  (+ (string->number (cdr a-pair))
                                     (string->number (cdr (assoc 'first qry)))))))))]
        [(assoc 'first qry)
         => (lambda (a-pair)
              `(html (head (title "Second Page"))
                     (body
                      (h1 "Second Page")
                      (form ([action ,(url->string uri)]
                             [method "get"] [enctype "application/x-www-form-urlencoded"])
                         "Enter the second number to add: "
                         (input ([type "hidden"] [name "first"] [value ,(cdr a-pair)]))
                         (input ([type "text"] [name "second"] [value ""]))
                         (input ([type "submit"] [name "enter"] [value "Enter"]))))))]
        [else
         `(html (head (title "Hello"))
                (body
                 (h1 "Hello World!")
                 (form ([action ,(url->string uri)]
                        [method "get"] [enctype "application/x-www-form-urlencoded"])
                         "Enter the first number to add: "
                         (input ([type "text"] [name "first"] [value ""]))
                         (input ([type "submit"] [name "enter"] [value "Enter"])))))]))))