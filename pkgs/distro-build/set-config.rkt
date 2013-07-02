#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         (only-in "config.rkt" extract-options)
         "url-options.rkt")

(define-values (dest-config-file config-file config-mode default-doc-search default-catalogs)
  (command-line
   #:args
   (dest-config-file config-file config-mode doc-search . catalog)
   (values dest-config-file config-file config-mode doc-search catalog)))

(define config (extract-options config-file config-mode))

(define doc-search (choose-doc-search config default-doc-search))

(define catalogs (choose-catalogs config default-catalogs))

(define orig
  (if (file-exists? dest-config-file)
      (call-with-input-file* dest-config-file read)
      (hash)))

(let* ([table orig]
       [table
        (if (equal? doc-search "")
            table
            (hash-set table 'doc-search-url doc-search))]
       [table (if (equal? catalogs '(""))
                  table
                  (hash-set table 'catalogs 
                            (for/list ([c (in-list catalogs)])
                              (if (equal? c "")
                                  #f
                                  c))))])
  (unless (equal? table orig)
    (make-directory* (path-only dest-config-file))
    (call-with-output-file dest-config-file
      #:exists 'truncate
      (lambda (o)
        (write table o)
        (newline o)))))
