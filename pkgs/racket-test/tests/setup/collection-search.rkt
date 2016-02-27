#lang racket
(require setup/collection-search
         compiler/compilation-path
         syntax/modcollapse
         rackunit
         pkg/lib
         pkg/path
         setup/getinfo
         racket/format
         setup/collection-name)

(define (check-search-finds-one mp)
  (printf "try ~s\n" mp)
  (define norm-mp (collapse-module-path mp 'racket))
  (check-equal? (list
                 (resolved-module-path-name
                  (module-path-index-resolve
                   (module-path-index-join mp #f))))
                (collection-search norm-mp
                                   #:init null
                                   #:combine (lambda (r n)
                                               (if (or (file-exists? n)
                                                       (file-exists? (get-compilation-bytecode-file n)))
                                                   (cons n r)
                                                   r)))))

(for-each check-search-finds-one 
          (list 'racket
                'racket/base
                'compiler/compilation-path
                'tests/setup/collection-search))

;; Try to find a module in every installed collection:
(define cache (make-hash))
(for ([s (get-all-pkg-scopes)])
  (for ([(name info) (in-hash (installed-pkg-table #:scope s))])
    (define dir (pkg-directory name #:cache cache))
    (define info (get-info/full dir))
    (define collection (and info
                            (info 'collection (lambda () 'use-pkg-name))))
    (define-values (coll coll-dir)
      (cond
       [(eq? collection 'multi)
        (let loop ([l (directory-list dir)])
          (cond
           [(null? l) (values #f #f)]
           [(and (collection-name-element? (path->string (car l)))
                 (directory-exists? (build-path dir (car l))))
            (values (car l) (build-path dir (car l)))]
           [else (loop (cdr l))]))]
       [(string? collection)
        (values collection dir)]
       [else
        (values name dir)]))
    (when coll
      (define file (for/first ([l (directory-list coll-dir)]
                               #:when (and (regexp-match? #rx"[.]rkt$" l)
                                           (not (equal? (path->string l) "info.rkt"))))
                     l))
      (when file
        (check-search-finds-one `(lib ,(~a coll "/" file)))))))
