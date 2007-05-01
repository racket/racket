(module fupload mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "plt-match.ss"))
  (provide (all-defined))
  
  (define timeout 60)
  (define interface-version 'v1)
  (define (start initial-request)
    (send/suspend/callback
     `(html (head)
            (body
             (form ([action
                     ,(lambda (request)
                        (define b (request-bindings/raw request))
                        (match (bindings-assq #"file" b)
                          [(struct binding:file (_ filename _))
                           `(html 
                             (body
                              ,(bytes->string/utf-8 filename)))]))]
                    [method "post"]
                    [enctype "multipart/form-data"])
                   (h3 "Submit for an Assignment")
                   (input ([name "file"] [type "file"] [size "30"]))
                   (input ([type "submit"] [value "Submit"]))))))))
