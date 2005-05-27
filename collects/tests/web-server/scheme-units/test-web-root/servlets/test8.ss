;; Non-incremental servlet with arguments on the URL, in text/plain, with URL
;; path.
(module test8 mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      (cons "text/plain"
            (cons
              (path->string (current-directory))
              (map
                (lambda (binding)
                  (string-append (symbol->string (car binding)) (cdr binding)))
                (request-bindings req))))))
  )
