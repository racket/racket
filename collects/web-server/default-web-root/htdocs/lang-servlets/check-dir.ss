(module check-dir (lib "lang.ss" "web-server")
  (require (lib "url.ss" "net"))
  (provide start)
  
  (define (directory-page n)
    (send/suspend/url
     (lambda (k-url)
       `(html (head (title ,(format "Page ~a" n)))
              (body
               (h1 ,(format "Page ~a" n))
               (h2 ,(format "The current directory: ~a" (current-directory)))
               (p "Click " (a ([href ,(url->string k-url)]) "here") " to continue.")))))
    (directory-page (add1 n)))
  
  (define (start initial-request)
    (directory-page 1)))