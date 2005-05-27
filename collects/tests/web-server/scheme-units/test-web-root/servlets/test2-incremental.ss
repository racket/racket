;; Incremental servlet with no arguments on the URL, in text/html, with URL
;; path.
(module test2-incremental mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      (make-html-response/incremental
        (lambda (output-chunked)
          (output-chunked "<html><head><title>")
          (output-chunked "Title")
          (sleep 4)
          (output-chunked "</title></head><body><h1>Title</h1>")
          (output-chunked "<p>Current path: ")
          (sleep 4)
          (output-chunked (path->string (current-directory)))
          (output-chunked "</p></body></html>")))))
  )
