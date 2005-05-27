;; Incremental servlet with no arguments on the URL, in text/html, no URL
;; path.
(module test1-incremental mzscheme
  (require (lib "servlet.ss" "web-server")
           )

  (provide start timeout interface-version)

  (define timeout 1)
  (define interface-version 'v1)

  (define (start req)
    (send/finish
      (make-html-response/incremental
        (lambda (output-chunk)
          (output-chunk "<html><head><title>")
          (output-chunk "Title")
          (sleep 4)
          (output-chunk "</title></head></html>")))))
  )

