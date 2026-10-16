#lang racket/base
(require "catalog.rkt"
         "../path.rkt"
         "pkg-db.rkt"
         "print.rkt")

(provide pkg-outdated)

;; Formats a checksum for display: the first 8 characters, or the
;; whole string if shorter; #f (linked/local packages) becomes "-".
(define (short-checksum checksum)
  (cond
    [(not checksum) "-"]
    [(> (string-length checksum) 8)
     (string-append (substring checksum 0 8) "...")]
    [else checksum]))

;; Returns the sorted names of packages whose installed checksum
;; differs from the one reported by the catalog. Packages with no
;; installed checksum (linked or local) and names absent from the
;; catalog are not considered.
(define (outdated-package-names installed-checksums catalog-details)
  (sort
   (filter values
           (for/list ([(name checksum) (in-hash installed-checksums)])
             (define details (hash-ref catalog-details name #f))
             (define catalog-checksum (and details (hash-ref details 'checksum #f)))
             (and checksum
                  catalog-checksum
                  (not (equal? checksum catalog-checksum))
                  name)))
   string<?))

(define (pkg-outdated names)
  (define installed (installed-pkg-table))
  (define selected
    (if (null? names)
        installed
        (for/hash ([(name info) (in-hash installed)]
                   #:when (member name names))
          (values name info))))
  (for ([name (in-list names)]
        #:unless (hash-has-key? installed name))
    (printf "~a is not installed.\n" name))
  (cond
    [(zero? (hash-count selected))
     (when (null? names)
       (printf "No packages installed.\n"))]
    [else
     (define checksums
       (for/hash ([(name info) (in-hash selected)])
         (values name (pkg-info-checksum info))))
     (define catalog-details (get-all-pkg-details-from-catalogs))
     (define outdated (outdated-package-names checksums catalog-details))
     (cond
       [(null? outdated)
        (printf "All installed packages are up to date.\n")]
       [else
        (printf "The following installed packages are not up to date:\n")
        (for ([name (in-list outdated)])
          (define details (hash-ref catalog-details name))
          (printf "  ~a  ~a -> ~a\n"
                  name
                  (short-checksum (hash-ref checksums name))
                  (short-checksum (hash-ref details 'checksum #f))))
        (printf "~a of ~a installed package(s) not up to date.\n"
                (length outdated) (hash-count selected))])]))
