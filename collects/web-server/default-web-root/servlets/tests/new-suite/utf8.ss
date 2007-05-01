(module utf8 mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  (define interface-version 'v1)
  (define timeout 60)
  (define (start initial-request)
    (extract-binding/single
     'name
     (request-bindings
      (send/suspend
       (lambda (k-url)
         `(html
           (body
            (form ([action ,k-url])
                  (input ([type "text"] [name "name"]))
                  (input ([type "submit"])))))))))))
