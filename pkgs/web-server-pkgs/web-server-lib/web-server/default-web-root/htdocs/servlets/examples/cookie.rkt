#lang web-server/insta
(require net/url)

(define (start req)
  (define cookies (request-cookies req))
  (define id-cookie
    (findf (lambda (c)
             (string=? "id" (client-cookie-name c)))
           cookies))
  (if id-cookie
      (hello (client-cookie-value id-cookie))
      (redirect-to
       (url->string (request-uri req))
       see-other
       #:headers
       (list
        (cookie->header (make-cookie "id" "joseph"))))))

(define (hello who)
  (response/xexpr
   `(html (head (title "Hello!"))
          (body
           (h1 "Hello "
               ,who)))))

