#lang racket/base
(require racket/set
         setup/collection-name
         setup/matching-platform
         setup/getinfo
         "../path.rkt"
         "params.rkt"
         "metadata.rkt"
         "get-info.rkt")

(provide pkg-directory->additional-installs
         directory->additional-installs
         get-additional-installed)

(define (pkg-directory->additional-installs dir pkg-name
                                            #:namespace [metadata-ns (make-metadata-namespace)]
                                            #:system-type [sys-type #f]
                                            #:system-library-subpath [sys-lib-subpath #f])
  (set->list (directory->additional-installs dir pkg-name metadata-ns
                                             #:system-type sys-type
                                             #:system-library-subpath sys-lib-subpath)))

(define (directory->additional-installs dir pkg-name metadata-ns
                                        #:system-type [sys-type #f]
                                        #:system-library-subpath [sys-lib-subpath #f])
  (define single-collect
    (pkg-single-collection dir #:name pkg-name #:namespace metadata-ns))
  (let loop ([s (set)] [f dir] [top? #t] [omits (set)])
    (cond
     [(and (directory-exists? f)
           (not (set-member? omits (simplify-path f))))
      (define i (get-pkg-info f metadata-ns))
      (define omit-paths (if i
                             (i 'compile-omit-paths (lambda () null))
                             null))
      (cond
       [(eq? omit-paths 'all)
        s]
       [else
        (define omit-files (if i
                               (i 'compile-omit-files (lambda () null))
                               null))
        (define new-s
          (if (and i (or single-collect (not top?)))
              (set-union (extract-additional-installs i sys-type sys-lib-subpath)
                         s)
              s))
        (define new-omits
          (set-union
           omits
           (for/set ([i (in-list (append omit-paths omit-files))])
             (simplify-path (build-path f i)))))
        (for/fold ([s new-s]) ([f (directory-list f #:build? #t)])
          (loop s f #f new-omits))])]
     [else s])))

(define (extract-additional-installs i sys-type sys-lib-subpath)
  (define (extract-documents i)
    (let ([s (i 'scribblings (lambda () null))])
      (for/set ([doc (in-list (if (list? s) s null))]
                #:when (and (list? doc)
                            (pair? doc)
                            (path-string? (car doc))
                            (or ((length doc) . < . 2)
                                (list? (cadr doc)))
                            (or ((length doc) . < . 4)
                                (collection-name-element? (list-ref doc 3)))))
        (define flags (if ((length doc) . < . 2)
                          null
                          (cadr doc)))
        (cond
         [(member 'main-doc-root flags) '(main-doc-root . "root")]
         [(member 'user-doc-root flags) '(user-doc-root . "root")]
         [else
          (cons 'doc
                (string-foldcase
                 (if ((length doc) . < . 4)
                     (let-values ([(base name dir?) (split-path (car doc))])
                       (path->string (path-replace-suffix name #"")))
                     (list-ref doc 3))))]))))
  (define (extract-paths i tag keys)
    (define (get k)
      (define l (i k (lambda () null)))
      (if (and (list? l) (andmap path-string? l))
          l
          null))
    (list->set (map (lambda (v) (cons tag
                                      (let-values ([(base name dir?) (split-path v)])
                                        ;; Normalize case, because some platforms
                                        ;; have case-insensitive filesystems:
                                        (string-foldcase (path->string name)))))
                    (apply
                     append
                     (for/list ([k (in-list keys)])
                       (get k))))))
  (define (extract-launchers i)
    (extract-paths i 'exe '(racket-launcher-names
                            mzscheme-launcher-names
                            gracket-launcher-names
                            mred-launcher-names)))
  (define (extract-foreign-libs i)
    (extract-paths i 'lib '(copy-foreign-libs
                            move-foreign-libs)))
  (define (extract-shared-files i)
    (extract-paths i 'share '(copy-shared-files
                              move-shared-files)))
  (define (extract-man-pages i)
    (extract-paths i 'man '(copy-man-pages
                            move-man-pages)))
  (define (this-platform? i)
    (define v (i 'install-platform (lambda () #rx"")))
    (or (not (platform-spec? v))
        (matching-platform? v
                            #:system-type sys-type
                            #:system-library-subpath sys-lib-subpath)))
  (set-union (extract-documents i)
             (extract-launchers i)
             (if (this-platform? i)
                 (set-union
                  (extract-foreign-libs i)
                  (extract-shared-files i)
                  (extract-man-pages i))
                 (set))))

(define (get-additional-installed kind skip-ht-keys ai-cache metadata-ns path-pkg-cache)
  (or (unbox ai-cache)
      (let ()
        (define skip-pkgs (list->set (hash-keys skip-ht-keys)))
        (define dirs (find-relevant-directories '(scribblings
                                                  racket-launcher-names
                                                  mzscheme-launcher-names
                                                  gracket-launcher-names
                                                  mred-launcher-names
                                                  copy-foreign-libs
                                                  move-foreign-libs
                                                  copy-shared-files
                                                  move-shared-files
                                                  copy-man-pages
                                                  move-man-pages)
                                                (if (eq? 'user (current-pkg-scope))
                                                    'all-available
                                                    'no-user)))
        (define s (for/fold ([s (set)]) ([dir (in-list dirs)])
                    (cond
                     [(set-member? skip-pkgs (path->pkg dir #:cache path-pkg-cache))
                      s]
                     [else
                      (define i (get-pkg-info dir metadata-ns))
                      (if i
                          (set-union s (extract-additional-installs i #f #f))
                          s)])))
        (set-box! ai-cache s)
        s)))

