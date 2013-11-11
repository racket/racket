#lang racket/base
(require racket/cmdline
         racket/file
         net/url
         "download-page.rkt"
         (only-in "config.rkt" extract-options))

(define build-dir (build-path "build"))

(define built-dir (build-path build-dir "built"))
(define native-dir (build-path build-dir "native"))
(define docs-dir (build-path build-dir "docs"))

(define installers-dir (build-path "installers"))
(define pkgs-dir (build-path "pkgs"))
(define catalog-dir (build-path "catalog"))
(define doc-dir (build-path "doc"))
(define pdf-doc-dir (build-path "pdf-doc"))
(define log-dir (build-path "log"))

(define-values (config-file config-mode)
  (command-line
   #:args
   (config-file config-mode)
   (values config-file config-mode)))

(define config (extract-options config-file config-mode))

(define dest-dir (hash-ref config
                           '#:site-dest
                           (build-path build-dir "site")))

(printf "Assembling site as ~a\n" dest-dir)

(define (copy dir [build-dir build-dir])
  (make-directory* (let-values ([(base name dir?) (split-path dir)])
                     (if (path? base)
                         (build-path dest-dir base)
                         dest-dir)))
  (printf "Copying ~a\n" (build-path build-dir dir))
  (copy-directory/files (build-path build-dir dir)
                        (build-path dest-dir dir)
                        #:keep-modify-seconds? #t))

(delete-directory/files dest-dir #:must-exist? #f)

(define (build-catalog built-dir)
  (printf "Building catalog from ~a\n" built-dir)
  (let ([c-dir (build-path built-dir pkgs-dir)]
        [d-dir (build-path dest-dir pkgs-dir)])
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
                                 pkgs-dir
                                 (path-add-suffix f #".zip")))))))
      (call-with-output-file* 
       (build-path d-dir f)
       (lambda (o)
         (write new-ht o)
         (newline o))))))

(build-catalog built-dir)
(build-catalog native-dir)
(let ([l (directory-list (build-path dest-dir catalog-dir "pkg"))])
  ;; Write list of packages:
  (define sl (map path-element->string l))
  (call-with-output-file*
   (build-path dest-dir catalog-dir "pkgs")
   (lambda (o)
     (write sl o)
     (newline o)))
  ;; Write hash table of package details:
  (define dht
    (for/hash ([f (in-list l)])
      (values (path-element->string f)
              (call-with-input-file*
               (build-path dest-dir catalog-dir "pkg" f)
               read))))
  (call-with-output-file*
   (build-path dest-dir catalog-dir "pkgs-all")
   (lambda (o)
     (write dht o)
     (newline o))))

(copy log-dir)

(copy installers-dir)

(define doc-path (build-path docs-dir doc-dir))
(when (directory-exists? doc-path)
  (copy doc-dir docs-dir))
(define pdf-doc-path (build-path build-dir pdf-doc-dir))
(when (directory-exists? pdf-doc-path)
  (copy pdf-doc-dir))
(copy "stamp.txt")
(copy (build-path "origin" "collects.tgz"))

(make-download-page (build-path build-dir
                                installers-dir
                                "table.rktd")
                    #:installers-url "installers/"
                    #:log-dir-url "log/"
                    #:docs-url (and (directory-exists? doc-path)
                                    "doc/index.html")
                    #:pdf-docs-url (and (directory-exists? pdf-doc-path)
                                        "pdf-doc/")
                    #:dest (build-path dest-dir
                                       "index.html")
                    #:help-table (hash-ref config '#:site-help (hash))
                    #:git-clone (current-directory))
