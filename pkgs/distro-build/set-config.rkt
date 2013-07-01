#lang racket/base
(require racket/cmdline
         racket/file
         racket/path)

(define-values (config-file doc-search catalogs)
  (command-line
   #:args
   (config-file doc-search . catalog)
   (values config-file doc-search catalog)))

(define orig
  (if (file-exists? config-file)
      (call-with-input-file* config-file read)
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
    (make-directory* (path-only config-file))
    (call-with-output-file config-file
      #:exists 'truncate
      (lambda (o)
        (write table o)
        (newline o)))))
