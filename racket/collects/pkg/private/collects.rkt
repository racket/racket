#lang racket/base
(require setup/pack
         racket/set
         "dep.rkt"
         "pkg-db.rkt"
         "get-info.rkt"
         "metadata.rkt"
         "print.rkt")

;; Package collection and dependency information needed for
;; installation and removal of packages.

(provide package-collections
         package-collection-directories
         package-dependencies
         get-setup-collects)

(define (package-collections pkg-dir metadata-ns)
  (for/list ([d (directory-list pkg-dir)]
             #:when (directory-exists? (build-path pkg-dir d))
             #:when (std-filter d))
    d))

(define (package-collection-directories pkg-dir metadata-ns)
  (for/list ([c (in-list (package-collections pkg-dir metadata-ns))])
    (build-path pkg-dir c)))

(define ((package-dependencies metadata-ns db all-platforms?
                               #:only-implies? [only-implies? #f]) 
         pkg-name)
  (define pkg-dir (pkg-directory* pkg-name #:db db))
  (define deps
    (map dependency->name 
         (let ([l (get-all-deps metadata-ns pkg-dir)])
           (if all-platforms?
               l
               (filter dependency-this-platform? l)))))
  (if only-implies?
      (let ([implies (list->set (get-all-implies metadata-ns pkg-dir deps))])
        (filter (lambda (dep)
                  (set-member? implies dep))
                deps))
      deps))

(define (maybe-append lists)
  (and (for/and ([v (in-list lists)]) (not (eq? v 'all)))
       (apply append lists)))

(define (get-setup-collects pkg-names db metadata-ns)
  (maybe-append
   (for/list ([pkg-name (in-list pkg-names)])
     (define pkg-dir (pkg-directory* pkg-name #:db db))
     (define single-collect
       (and pkg-dir
            (pkg-single-collection pkg-dir #:name pkg-name #:namespace metadata-ns)))
     (or (and (not pkg-dir) null)
         (and single-collect (list single-collect))
         (get-metadata metadata-ns pkg-dir
                       'setup-collects (lambda () (package-collections
                                                   pkg-dir
                                                   metadata-ns))
                       #:checker (lambda (v)
                                   (unless (or (eq? v 'all)
                                               (and (list? v)
                                                    (for ([c (in-list v)])
                                                      (or (path-string? c)
                                                          (and (list? c)
                                                               (pair? c)
                                                               (andmap path-string? c))))))
                                     (pkg-error "bad 'setup-collects value\n  value: ~e"
                                                v))))))))
