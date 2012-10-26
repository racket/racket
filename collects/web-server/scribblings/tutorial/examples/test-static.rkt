#lang web-server/insta
(define (start request)
  (response/xexpr
   '(html (head (title "Testing")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))          
          (body (h1 "This is a header")
                (p "This is " (span ((class "hot")) "hot") ".")))))
(static-files-path "htdocs")
