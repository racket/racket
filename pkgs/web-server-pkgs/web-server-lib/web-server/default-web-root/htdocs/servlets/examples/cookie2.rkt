#lang web-server/insta
(require net/url)

(define (start req)
  (define cookies (request-cookies req))
  (define id-cookie
    (findf (lambda (c)
             (string=? "id" (client-cookie-name c)))
           cookies))
  (define who
    (if id-cookie
        (client-cookie-value id-cookie)
        #f))
  (define new-req
    (send/suspend
     (lambda (k-url)
       (response/xexpr
        `(html (head (title "Hello!"))
               (body (h1 "Hello " ,(if who who "<unknown>"))
                     (form ([action ,k-url])
                           (input ([name "who"])))))))))
  (define binds
    (request-bindings/raw new-req))
  (match (bindings-assq #"who" binds)
    [(? binding:form? b)
     (define new-who 
       (bytes->string/utf-8 (binding:form-value b)))
     (redirect-to (url->string (request-uri req))
                  see-other
                  #:headers
                  (list
                   (cookie->header (make-cookie "id" new-who))))]
    [else
     (redirect-to
      (url->string (request-uri req))
      see-other)]))
