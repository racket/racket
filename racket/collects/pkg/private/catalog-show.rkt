#lang racket/base
(require racket/format
         "../name.rkt"
         "catalog.rkt"
         "dep.rkt"
         "path.rkt"
         "print.rkt")

(provide pkg-catalog-show)

(define (pkg-catalog-show names 
                        #:all? [all? #f]
                        #:only-names? [only-names? #f]
                        #:modules? [modules? #f])
  (for ([name (in-list names)])
    (define-values (parsed-name type)
      (package-source->name+type name #f))
    (unless (eq? type 'name)
      (pkg-error (~a "incorrect syntax for a package name\n"
                     "  given: ~a")
                 name)))
  
  (cond
   [only-names?
    (define all-names (if all?
                          (get-all-pkg-names-from-catalogs)
                          names))
    (for ([name (in-list all-names)])
      (unless all?
        ;; Make sure it's available:
        (get-pkg-details-from-catalogs name))
      (printf "~a\n" name))]
   [else
    (define all-details (and all?
                             (get-all-pkg-details-from-catalogs)))
    (for ([name (in-list (if all?
                             (sort (hash-keys all-details) string<?)
                             names))]
          [position (in-naturals)])
      (define details (select-info-version
                       (if all?
                           (hash-ref all-details name)
                           (get-pkg-details-from-catalogs name))))
      (unless (zero? position) (newline))
      (printf "Package name: ~a\n" name)
      (for ([key '(author source checksum tags description)])
        (define v (hash-ref details key #f))
        (when v
          (printf " ~a: ~a\n"
                  (string-titlecase (symbol->string key))
                  (if (list? v)
                      (apply ~a #:separator ", " v)
                      v))))
      (for ([key '(dependencies)])
        (define v (hash-ref details key null))
        (unless (null? v)
          (printf " Dependencies:\n")
          (for ([dep (in-list v)])
            (define vers (dependency->version dep))
            (define plat (dependency-lookup '#:platform dep))
            (printf "  ~a~a~a\n"
                    (dependency->name dep)
                    (if vers
                        (format " version ~a" vers)
                        "")
                    (if plat
                        (format " on platform ~v" plat)
                        "")))))
      (when modules?
        (printf " Modules:")
        (for/fold ([col 72]) ([mod (in-list (hash-ref details 'modules null))])
          (define pretty-mod (pretty-module-path mod))
          (define mod-str (~a " " (~s pretty-mod)))
          (define new-col (if ((+ col (string-length mod-str)) . > . 72)
                              (begin
                                (printf "\n ")
                                0)
                              col))
          (display mod-str)
          (+ new-col (string-length mod-str)))
        (newline)))]))

