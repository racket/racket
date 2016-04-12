#lang racket/base
(require setup/dirs
         racket/file
         compiler/embed
         launcher)

(provide installer)

(define (installer path coll user? no-main?)
  (unless (or user? no-main?)
    (do-installer #f (find-config-tethered-console-bin-dir)))
  (do-installer #t (find-addon-tethered-console-bin-dir)))

(define (do-installer user? dir)
  (when dir
    (make-directory* dir)
    (define variants (available-racket-variants))
    (for ([v (in-list variants)])
      (parameterize ([current-launcher-variant v])
        (create-embedding-executable
         (racket-program-launcher-path "Racket" #:user? user? #:tethered? #t)
         #:variant v
         #:cmdline (append
                    (list "-X" (path->string (find-collects-dir))
                          "-G" (path->string (find-config-dir)))
                    (if user?
                        (list "-A" (path->string (find-system-path 'addon-dir)))
                        null))
         #:launcher? #t
         #:aux `((relative? . #f)))))))
