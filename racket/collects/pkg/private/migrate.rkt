#lang racket/base
(require racket/match
         net/url
         "../path.rkt"
         "config.rkt"
         "lock.rkt"
         "pkg-db.rkt"
         "desc.rkt"
         "params.rkt"
         "install.rkt"
         "repo-path.rkt"
         "dirs.rkt")

(provide pkg-migrate)

(define (pkg-migrate from-version
                     #:all-platforms? [all-platforms? #f]
                     #:force? [force? #f]
                     #:quiet? [quiet? #f]
                     #:from-command-line? [from-command-line? #f]
                     #:ignore-checksums? [ignore-checksums? #f]
                     #:strict-doc-conflicts? [strict-doc-conflicts? #f]
                     #:use-cache? [use-cache? #t]
                     #:dep-behavior [dep-behavior #f]
                     #:strip [strip-mode #f]
                     #:force-strip? [force-strip? #f])
  (define from-db
    (parameterize ([current-pkg-scope-version from-version])
      (installed-pkg-table #:scope 'user)))
  (define installed-dir
    (parameterize ([current-pkg-scope 'user])
      (pkg-installed-dir)))
  (define (path->complete-string p)
    (path->string (path->complete-path p installed-dir)))
  (define to-install
    (sort
     (for/list ([(name info) (in-hash from-db)]
                #:unless (pkg-info-auto? info))
       (define-values (source type dir)
         (match (pkg-info-orig-pkg info)
           [(list* 'catalog name _) (values name 'name #f)]
           [(list 'url url) (values url #f #f)]
           [(list 'link path) (values (path->complete-string path) 'link #f)]
           [(list 'static-link path) (values (path->complete-string path) 'static-link #f)]
           [(list 'clone path url) (values url 'clone (enclosing-path-for-repo
                                                       url
                                                       (path->complete-path path installed-dir)))]))
       (pkg-desc source type name #f #f dir))
     string<?
     #:key pkg-desc-name))
  (unless quiet?
    (cond
     [(null? to-install)
      (printf "No packages from ~s to install\n" from-version)]
     [else
      (printf "Packages to install:\n")
      (for ([d (in-list to-install)])
        (define t (pkg-desc-type d))
        (define n (pkg-desc-name d))
        (case t
          [(name) (printf "  ~a\n" n)]
          [(link static-link)
           (printf "  ~a ~aed from ~a\n" n t (pkg-desc-source d))]
          [else
           (printf "  ~a from ~a\n" n (pkg-desc-source d))]))]))
  (if (null? to-install)
      'skip
      (begin0
       (pkg-install to-install
                    #:all-platforms? all-platforms?
                    #:force? force?
                    #:ignore-checksums? ignore-checksums?
                    #:strict-doc-conflicts? strict-doc-conflicts?
                    #:use-cache? use-cache?
                    #:skip-installed? #t
                    #:dep-behavior (or dep-behavior 'search-auto)
                    #:quiet? quiet? 
                    #:from-command-line? from-command-line?
                    #:strip strip-mode
                    #:force-strip? force-strip?)
       (unless quiet?
         (printf "Packages migrated\n")))))
