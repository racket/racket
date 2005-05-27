;; Non-incremental servlet with no arguments on the URL, in text/html, with URL
;; path.
(module test2 mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      `(html
         (head
           (title "Title"))
         (body
           (h1 "Title")
           (p "Current path: " ,(path->string (current-directory)))))))
  )
