#lang racket/base
(require pkg
         pkg/lib
         racket/format
         net/url
         racket/set
         racket/file
         openssl/sha1
         racket/cmdline)

(define create-mode 'built)

(command-line
 #:once-each
 [("--mode") mode "Create package archives for <mode>"
  (set! create-mode (string->symbol mode))]
 #:args ()
 (void))

(define build-dir "build")
(define dest-dir (build-path build-dir (~a create-mode)))
(define native-dir (build-path build-dir "native" "pkgs"))
(define pkg-dest-dir (path->complete-path (build-path dest-dir "pkgs")))
(define catalog-dir (build-path dest-dir "catalog" "pkg"))
(make-directory* pkg-dest-dir)
(make-directory* catalog-dir)

(for ([pkg (in-list (installed-pkg-names))])
  (define native-zip (build-path native-dir (path-add-suffix pkg ".zip")))
  (unless (file-exists? native-zip)
    (define dest-zip (build-path pkg-dest-dir (~a pkg ".zip")))
    (pkg-create 'zip pkg
                #:source 'name
                #:dest pkg-dest-dir
                #:mode create-mode)
    (call-with-output-file*
     (build-path catalog-dir pkg)
     #:exists 'truncate
     (lambda (o)
       (write (hash 'source (path->string dest-zip)
                    'checksum (call-with-input-file* dest-zip sha1)
                    'name pkg
                    'author "plt@racket-lang.org"
                    'description "library"
                    'tags '()
                    'dependencies '()
                    'modules '())
              o)
       (newline o)))))
