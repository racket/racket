#lang racket/base
(require racket/cmdline
         pkg/lib
         net/url)

;; Check that links to packages in `catalog` are right.

(define catalog
  (command-line
   #:args
   (catalog)
   catalog))

(define pkgs
  (for/list ([f (in-list (directory-list (build-path catalog "pkg")))])
    (path->string f)))

(define installed
  (installed-pkg-table #:scope 'installation))

(define (installed-location pkg)
  (define info (hash-ref installed pkg))
  (define loc (pkg-info-orig-pkg info))
  (if (and (pair? loc)
           (eq? 'static-link (car loc)))
      (list 'static-link
            (simplify-path
             (path->complete-path (cadr loc)
                                  (get-pkgs-dir 'installation))))
      loc))

(define (installed-auto? pkg)
  (pkg-info-auto? (hash-ref installed pkg)))

(define (catalog-location pkg)
  (define r (call-with-input-file* (build-path catalog "pkg" pkg )
                                   read))
  `(static-link
    ,(simplify-path
      (url->path
       (combine-url/relative (path->url (path->directory-path (path->complete-path catalog)))
                             (hash-ref r 'source))))))

(define fix-pkgs
  (for/list ([pkg (in-list pkgs)]
             #:when (hash-ref installed pkg #f)
             [installed-location (in-value (installed-location pkg))]
             #:when (and (pair? installed-location)
                         (equal? 'static-link (car installed-location)))
             #:unless (equal? installed-location
                              (catalog-location pkg)))
    pkg))

(when (null? fix-pkgs)
  (printf "Package links are in sync\n"))

(unless (null? fix-pkgs)
  (define descs
    (for/list ([pkg (in-list fix-pkgs)])
      (pkg-desc pkg
                'name
                pkg
                #f
                (installed-auto? pkg))))
  (parameterize ([current-pkg-scope 'installation])
    (void (with-pkg-lock (pkg-update descs)))))
