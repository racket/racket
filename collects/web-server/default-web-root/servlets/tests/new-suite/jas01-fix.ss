(module jas01-fix mzscheme
  (require (lib "servlet.ss" "web-server")
           "jas01-fix-param.ss")

  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)

  ; start : request -> response
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html (head (title "Servlet Parameter Test"))
           (body (h1 "Servlet Parameter Test")
                 ,(number->string (get-time))))))
