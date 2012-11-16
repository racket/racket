#lang scheme/base

(require launcher compiler/embed)
(provide post-installer)

(define (post-installer path)
  (define variants (available-mzscheme-variants))
  (for ([v (in-list variants)])
    (parameterize ([current-launcher-variant v])
      (create-embedding-executable
       (mzscheme-program-launcher-path "MzScheme")
       #:variant v
       #:cmdline '("-I" "scheme/init")
       #:launcher? #t
       #:aux '((framework-root . #f)
               (dll-dir . #f)
               (relative? . #t))))))
