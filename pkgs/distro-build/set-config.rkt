#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         (only-in "config.rkt" extract-options)
         "url-options.rkt")

(define-values (dest-config-file config-file config-mode 
                                 install-name build-stamp 
                                 default-doc-search default-catalogs)
  (command-line
   #:args
   (dest-config-file config-file config-mode 
                     install-name build-stamp
                     doc-search . catalog)
   (values dest-config-file config-file config-mode 
           install-name build-stamp
           doc-search catalog)))

(define config (if (equal? config-file "")
                   (hash)
                   (extract-options config-file config-mode)))

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
        (newline o)))))
