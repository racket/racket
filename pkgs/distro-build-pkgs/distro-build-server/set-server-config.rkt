#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         (only-in "config.rkt" extract-options)
         "url-options.rkt"
         distro-build/set-config)

(module test racket/base)

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

(set-config dest-config-file
            install-name build-stamp
            doc-search catalogs)
