#lang racket/base
(require racket/cmdline
         racket/file
         net/url
         "download-page.rkt"
         (only-in "config.rkt" extract-options))

(define build-dir (build-path "build"))
(define dest-dir (build-path build-dir "site"))

(define built-dir (build-path build-dir "built"))
(define native-dir (build-path build-dir "native"))

(define installers-dir (build-path "installers"))
(define pkgs-dir (build-path "pkgs"))
(define catalog-dir (build-path "catalog"))

(define-values (config-file config-mode)
  (command-line
   #:args
   (config-file config-mode)
   (values config-file config-mode)))

(define config (extract-options config-file config-mode))

(define (copy dir [build-dir build-dir])
  (make-directory* dest-dir)
  (printf "Copying ~a\n" (build-path build-dir dir))
  (copy-directory/files (build-path build-dir dir)
                        (build-path dest-dir dir)
                        #:keep-modify-seconds? #t))

(delete-directory/files dest-dir #:must-exist? #f)

(define (build-catalog built-dir)
  (printf "Building catalog from ~a\n" built-dir)
  (let ([c-dir (build-path built-dir "pkgs")]
        [d-dir (build-path dest-dir "pkgs")])
    (make-directory* d-dir)
    (for ([f (directory-list c-dir)])
      (define c (build-path c-dir f))
      (define d (build-path d-dir f))
      (copy-file c d)
      (file-or-directory-modify-seconds d (file-or-directory-modify-seconds c))))
  (let ([c-dir (build-path built-dir catalog-dir "pkg")]
        [d-dir (build-path dest-dir catalog-dir "pkg")])
    (make-directory* d-dir)
    (define base-url (string->url (hash-ref config '#:dist-base-url)))
    (for ([f (in-list (directory-list c-dir))])
      (define ht (call-with-input-file* (build-path c-dir f) read))
      (define new-ht
        (hash-set ht 'source (url->string
                              (combine-url/relative
                               base-url
                               (path->string 
                                (build-path
                                 "pkgs"
                                 (path-add-suffix f #".zip")))))))
      (call-with-output-file* 
       (build-path d-dir f)
       (lambda (o)
         (write new-ht o)
         (newline o))))))

(build-catalog built-dir)
(build-catalog native-dir)

(copy installers-dir)

(make-download-page (build-path build-dir
                                installers-dir
                                "table.rktd")
                    #:installers-url "installers/"
                    #:dest (build-path dest-dir
                                       "index.html")
                    #:git-clone (current-directory))
