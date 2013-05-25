#lang racket/base
(require racket/cmdline
         racket/file
         net/url)

(command-line
 #:args
 ()
 (void))

(define src-dir "pkgs")
(define dest-dir (build-path "build" "local"))

(define catalog-dir (build-path dest-dir "catalog" "pkg"))
(make-directory* catalog-dir)

(define found (make-hash))

;; Recur through directory tree, and treat each directory
;; that has an "info.rkt" file as a package (and don't recur
;; further into the package)
(let loop ([src-dir src-dir])
  (for ([f (in-list (directory-list src-dir))])
    (define src-f (build-path src-dir f))
    (cond
     [(file-exists? (build-path src-f "info.rkt"))
      (when (hash-ref found f #f)
        (error 'pack-local 
               "found packages multiple times: ~a and ~a"
               (hash-ref found f)
               src-f))
      (hash-set! found f src-f)
      (call-with-output-file*
       (build-path catalog-dir f)
       #:exists 'truncate
       (lambda (o)
         (write (hash 'source (path->string (path->directory-path src-f))
                      'checksum "0"
                      'name (path->string f)
                      'author "plt@racket-lang.org"
                      'description "library"
                      'tags '()
                      'dependencies '()
                      'modules '())
                o)
         (newline o)))]
     [(directory-exists? src-f)
      (loop src-f)])))
