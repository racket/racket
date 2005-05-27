;; Non-incremental servlet with arguments on the URL, in text/html, no URL
;; path.
(module test4 mzscheme
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
           ,@(map
               (lambda (binding)
                 (list 'p (symbol->string (car binding)) (cdr binding)))
               (request-bindings req))))))
  )
