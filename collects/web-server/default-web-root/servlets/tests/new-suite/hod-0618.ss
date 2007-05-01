(module hod-0618 mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout 30)
  (define (start _)
    `(html
      (body
       ,(extract-binding/single 
         'test:case
         (request-bindings
          (send/suspend
           (lambda (k-url)
             `(html
               (body
                (form ([method "POST"]
                       [action ,k-url])
                      (input ([type "text"] [name "test:case"]))
                      (input ([type "submit"])))))))))))))
