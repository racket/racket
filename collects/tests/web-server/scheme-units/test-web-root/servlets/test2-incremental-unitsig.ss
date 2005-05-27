;; Incremental servlet with no arguments on the URL, in text/html, with URL
;; path.
(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         )
(unit/sig ()
  (import servlet^)
(let ((cd (path->string (current-directory))))
  (send/finish
    (make-html-response/incremental
      (lambda (output-chunked)
        (output-chunked "<html><head><title>")
        (output-chunked "Title")
        (sleep 1)
        (output-chunked "</title></head><body><h1>Title</h1>")
        (output-chunked "<p>Current path: ")
        (sleep 1)
        (output-chunked cd)
        (output-chunked "</p></body></html>")))))
  )

