(module adjust mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (adjust-timeout! 1)
    (send/suspend
     (lambda (k-url)
       `(html (head (title "Hello"))
              (body (a ([href ,k-url])
                       "Link")))))
    `(html (body "Should not happen"))))