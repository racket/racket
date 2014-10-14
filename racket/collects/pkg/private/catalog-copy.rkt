#lang racket/base
(require racket/format
         racket/file
         net/url
         (prefix-in db: "../db.rkt")
         "config.rkt"
         "catalog.rkt"
         "print.rkt"
         "params.rkt")

(provide pkg-catalog-copy
         src->url-or-path
         url-or-path->url-string)

(define (src->url-or-path src)
  (cond
   [(path? src) (path->complete-path src)]
   [(regexp-match? #rx"^https?://" src)
    (string->url src)]
   [(regexp-match? #rx"^file://" src)
    (url->path (string->url src))]
   [(regexp-match? #rx"^[a-zA-Z]*://" src)
    (pkg-error (~a "unrecognized URL scheme for a catalog\n"
                   "  URL: ~a")
               src)]
   [else (path->complete-path src)]))

(define (url-or-path->url-string p)
  (url->string (if (url? p)
                   p
                   (path->url p))))

(define (pkg-catalog-copy srcs dest
                          #:from-config? [from-config? #f]
                          #:merge? [merge? #f]
                          #:force? [force? #f]
                          #:override? [override? #f]
                          #:relative-sources? [relative-sources? #f])
  (define src-paths
    (for/list ([src (in-list (append srcs
                                     (if from-config?
                                         (pkg-config-catalogs)
                                         null)))])
      (define src-path (src->url-or-path src))
      (when (path? src-path)
        (cond
         [(db-path? src-path)
          (void)]
         [(directory-exists? src-path)
          (void)]
         [(let-values ([(base name dir?) (split-path src-path)]) dir?)
          (void)]
         [else
          (pkg-error (~a "bad source catalog path\n"
                         "  path: ~a\n"
                         "  expected: directory or path with \".sqlite\" extension")
                     src)]))
      src-path))
  (define dest-path
    (cond
     [(path? dest) (path->complete-path dest)]
     [(regexp-match? #rx"^file://" dest)
      (url->path (string->url dest))]
     [(regexp-match? #rx"^[a-zA-Z]*://" dest)
      (pkg-error (~a "cannot copy to a non-file destination catalog\n"
                     "  given URL: ~a")
                 dest)]
     [else (path->complete-path dest)]))

  (define dest-dir
    (and relative-sources?
         (if (db-path? dest-path)
             (let-values ([(base name dir?) (split-path dest-path)])
               base)
             dest-path)))

  (unless (or force? merge?)
    (when (or (file-exists? dest-path)
              (directory-exists? dest-path)
              (link-exists? dest-path))
      (pkg-error (~a "destination exists\n"
                     "  path: ~a")
                 dest-path)))

  (define absolute-details
    (let ([src-paths (if (and merge?
                              (or (file-exists? dest-path)
                                  (directory-exists? dest-path)))
                         (if override?
                             (append src-paths
                                     (list dest-path))
                             (cons dest-path
                                   src-paths))
                         src-paths)])
      (parameterize ([current-pkg-catalogs (for/list ([src-path src-paths])
                                            (if (path? src-path)
                                                (path->url src-path)
                                                src-path))])
        (get-all-pkg-details-from-catalogs))))
  (define details
    (if relative-sources?
        (for/hash ([(k ht) (in-hash absolute-details)])
          (values k (source->relative-source dest-dir ht)))
        absolute-details))

  (when (and force? (not merge?))
    (cond
     [(file-exists? dest-path)
      (delete-file dest-path)]
     [(directory-exists? dest-path)
      (if (db-path? dest-path)
          (delete-directory/files dest-path)
          (for ([i (directory-list dest-path)])
            (delete-directory/files (build-path dest-path i))))]
     [(link-exists? dest-path)
      (delete-file dest-path)]))

  (cond
   [(db-path? dest-path)
    (define vers-details
      (for/hash ([(k v) (in-hash details)])
        (values k (select-info-version v))))
    (parameterize ([db:current-pkg-catalog-file dest-path])
      (db:set-catalogs! '("local"))
      (db:set-pkgs! "local"
                    (for/list ([(k v) (in-hash vers-details)])
                      (db:pkg k "local"
                              (hash-ref v 'author "")
                              (hash-ref v 'source "")
                              (hash-ref v 'checksum "")
                              (hash-ref v 'description ""))))
      (for ([(k v) (in-hash vers-details)])
        (define t (hash-ref v 'tags '()))
        (unless (null? t)
          (db:set-pkg-tags! k "local" t)))
      (for ([(k v) (in-hash vers-details)])
        (define mods (hash-ref v 'modules '()))
        (unless (null? mods)
          (define cs (hash-ref v 'checksum ""))
          (db:set-pkg-modules! k "local" cs mods)))
      (for ([(k v) (in-hash vers-details)])
        (define deps (hash-ref v 'dependencies '()))
        (unless (null? deps)
          (define cs (hash-ref v 'checksum ""))
          (db:set-pkg-dependencies! k "local" cs deps))))]
   [else
    (define pkg-path (build-path dest-path "pkg"))
    (make-directory* pkg-path)
    (for ([(k v) (in-hash details)])
      (call-with-output-file*
       #:exists 'truncate/replace
       (build-path pkg-path k)
       (lambda (o) (write v o))))
    (call-with-output-file*
     #:exists 'truncate/replace
     (build-path dest-path "pkgs")
     (lambda (o) (write (hash-keys details) o)))
    (call-with-output-file*
     #:exists 'truncate/replace
     (build-path dest-path "pkgs-all")
     (lambda (o) (write details o)))]))

