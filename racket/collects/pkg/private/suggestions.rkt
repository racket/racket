#lang racket/base
(require racket/set
         racket/path
         setup/dirs
         syntax/modcollapse
         (prefix-in db: "../db.rkt"))

(provide pkg-catalog-suggestions-for-module)

(define (choose-catalog-file)
  (define default (db:current-pkg-catalog-file))
  (if (file-exists? default)
      default
      (let ([installation (build-path (find-share-dir) "pkgs" (file-name-from-path default))])
        (if (file-exists? installation)
            installation
            default))))

(define (pkg-catalog-suggestions-for-module module-path
                                            #:catalog-file [catalog-file (choose-catalog-file)])
  (if (file-exists? catalog-file)
      (parameterize ([db:current-pkg-catalog-file catalog-file])
        (let* ([mod (collapse-module-path 
                     module-path
                     (lambda () (build-path (current-directory) "dummy.rkt")))]
               [pkgs (db:get-module-pkgs mod)]
               [more-pkgs (let ([rx:reader #rx"/lang/reader[.]rkt$"])
                            (if (and (pair? mod)
                                     (eq? (car mod) 'lib)
                                     (regexp-match rx:reader (cadr mod)))
                                (db:get-module-pkgs `(lib ,(regexp-replace rx:reader (cadr mod) "/main.rkt")))
                                null))])
          (sort (set->list
                 (list->set
                  (map db:pkg-name (append pkgs more-pkgs)))) 
                string<?)))
      null))


