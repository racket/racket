(require (lib "servlet-sig.ss" "web-server")
         (lib "unit.ss"))

(unit
  (import servlet^)
  (export)
  
  (send/finish
   (make-html-response/incremental
    (lambda (output-chunk)
      (output-chunk "<html><head><title>"
                    "my-title</title></head>\n")
      (output-chunk "<body><p>The first paragraph</p>\n")
      (sleep 4)
      (output-chunk "<p>The second paragraph</p></body></html>\n")))))
