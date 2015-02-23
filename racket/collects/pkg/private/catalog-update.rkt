#lang racket/base
(require racket/format
         racket/list
         net/url
         (prefix-in db: "../db.rkt")
         "params.rkt"
         "catalog.rkt"
         "content.rkt"
         "config.rkt"
         "print.rkt"
         "desc.rkt")

(provide pkg-catalog-update-local)

(define (pkg-catalog-update-local #:catalogs [catalogs (pkg-config-catalogs)]
                                  #:set-catalogs? [set-catalogs? #t]
                                  #:catalog-file [catalog-file (db:current-pkg-catalog-file)]
                                  #:quiet? [quiet? #f]
                                  #:consult-packages? [consult-packages? #f]
                                  #:skip-download-failures? [skip-download-failures? #f])
  (parameterize ([db:current-pkg-catalog-file catalog-file])
    (define current-catalogs (db:get-catalogs))
    (cond
     [set-catalogs?
      (unless (equal? catalogs current-catalogs)
        (db:set-catalogs! catalogs))]
     [else
      (unless (for/and ([catalog (in-list catalogs)])
                (member catalog current-catalogs))
        (error 'pkg-catalog-update-local
               (~a "given catalog list is not a superset of recorded catalogs\n"
                   "  given: ~s\n"
                   "  recorded: ~s")
               catalogs
               current-catalogs))])

    (for ([catalog (in-list catalogs)])
      (unless quiet?
        (printf/flush "Updating from ~a\n" catalog))
      (parameterize ([current-pkg-catalogs (list (string->url catalog))])
        (define details (for/hash ([(name ht) (get-all-pkg-details-from-catalogs)])
                          (values name (select-info-version ht))))
        (db:call-with-pkgs-transaction
         (lambda ()
           ;; set packages:
           (db:set-pkgs! catalog (for/list ([(name ht) (in-hash details)])
                                   (db:pkg name
                                           catalog
                                           (hash-ref ht 'author "")
                                           (hash-ref ht 'source "")
                                           (hash-ref ht 'checksum "")
                                           (hash-ref ht 'description ""))))
           ;; Add available module and dependency info:
           (for/list ([(name ht) (in-hash details)])
             (define checksum (hash-ref ht 'checksum ""))
             (define mods (hash-ref ht 'modules #f))
             (when mods
               (db:set-pkg-modules! name catalog checksum mods))
             (define tags (hash-ref ht 'tags #f))
             (when tags
               (db:set-pkg-tags! name catalog tags))
             (define deps (hash-ref ht 'dependencies #f))
             (when deps
               (db:set-pkg-dependencies! name catalog checksum deps)))))
        (when consult-packages?
          ;; If module information isn't available for a package, download
          ;; the package to fill in that information:
          (define need-modules (db:get-pkgs-without-modules #:catalog catalog))
          (for ([(pkg) (in-list need-modules)])
            (define name (db:pkg-name pkg))
            (define ht (hash-ref details name))
            (define source (hash-ref ht 'source))
            (unless quiet?
              (printf/flush "Downloading ~s\n" source))
            (define-values (checksum modules deps)
              (get-pkg-content (pkg-desc source
                                         #f
                                         name
                                         (hash-ref ht 'checksum #f)
                                         #f)))
            (db:call-with-pkgs-transaction
             (lambda ()
               (db:set-pkg-modules! name catalog checksum modules)
               (db:set-pkg-dependencies! name catalog checksum deps)))))))))
