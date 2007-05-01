(module form mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "pretty.ss"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start _)
    (define req
      (send/suspend
       (lambda (k-url)
         `(html 
           (body
            (form ([action ,k-url] [method "POST"])
                  (input ([type "checkbox"] [name "checkbox"]))
                  (input ([type "text"] [name "text"]))
                  (input ([type "submit"] [name "submit"]))))))))
    (define binds
      (request-bindings req))
    (define sport
      (open-output-string))
    (define pp
      (parameterize ([current-output-port sport])
        (pretty-print binds)))
    `(html
      (body
       (pre
        ,(get-output-string sport))))))
