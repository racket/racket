;; Non-incremental servlet no arguments on the URL, in text/plain, with URL
;; path.
(module test7 mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      (list "text/plain"
            (path->string (current-directory)))))
  )
