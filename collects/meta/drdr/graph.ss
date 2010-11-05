#lang scheme
(require scheme/system
         "config.ss"
         "path-utils.ss"
         "dirstruct.ss")

(define rebaser
  (rebase-path (plt-data-directory) "/data"))

(define (main filename)
  (define prefix
    (path-timing-png-prefix filename))
  (system*/exit-code
   (path->string
    (build-path (plt-directory) "plt" "bin" "mred-text"))
   "-t"
   (path->string (build-path (drdr-directory) "graphs" "build-graph.ss"))
   "--"
   "-l" (string-append "http://drdr.racket-lang.org/~a/" (path->string* filename)) ; XXX
   "--image-loc" "/graph-images/"
   (path->string (path-timing-log filename))
   (path->string prefix)
   (path->string (rebaser prefix))
   (path->string (path-timing-html filename))))

(provide main)