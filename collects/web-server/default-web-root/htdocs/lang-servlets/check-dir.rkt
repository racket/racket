#lang web-server
(define interface-version 'stateless)
(provide start interface-version)

(define (directory-page n)
  (send/suspend/url
   (lambda (k-url)
     (response/xexpr
      `(html (head (title ,(format "Page ~a" n)))
             (body
              (h1 ,(format "Page ~a" n))
              (h2 ,(format "The current directory: ~a" (current-directory)))
              (p "Click " (a ([href ,(url->string k-url)]) "here") " to continue."))))))
  (directory-page (add1 n)))

(define (start initial-request)
  (directory-page 1))
