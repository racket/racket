#lang racket/base
(require racket/cmdline
         racket/file
         racket/path)

(provide set-config)

(module test racket/base)

(module+ main
  (command-line
   #:args
   (dest-config-file install-name build-stamp
                     doc-search . catalog)
   (set-config dest-config-file
               install-name build-stamp
               doc-search catalog)))

(define (set-config dest-config-file 
                    install-name build-stamp 
                    doc-search catalogs)
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
                                    c))))]
         [table (if (equal? install-name "")
                    table
                    (hash-set table 'installation-name install-name))]
         [table (hash-set table 'build-stamp build-stamp)])
    (unless (equal? table orig)
      (make-directory* (path-only dest-config-file))
      (call-with-output-file dest-config-file
        #:exists 'truncate
        (lambda (o)
          (write table o)
          (newline o))))))
