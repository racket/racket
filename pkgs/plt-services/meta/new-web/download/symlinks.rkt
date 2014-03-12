#lang plt-web
(require "resources.rkt"
         "data.rkt"
         plt-web/indexes
         version/utils)

(provide (all-defined-out))

(define download-index-site (index-site download-site))

;; For versions 5.92 and later, redirect "installers/<version>"
;; and "docs/<version>/..." to "releases/<version>/..."
(for ([r (in-list all-releases)])
  (define v (release-version r))
  (when (version<=? "5.92" v)
    (symlink #:site download-site
             (format "../releases/~a/installers" v)
             (format "installers/~a" v))
    (symlink #:site download-site
             (format "../../releases/~a/doc" v)
             (format "docs/~a/html" v))
    (symlink #:site download-site
             (format "../../releases/~a/pdf-doc" v)
             (format "docs/~a/pdf" v))))

;; We generally expect "index.html" files to be in place in "releases"
;; and for pre-v6.0 "docs", but keep "installers", "docs", and
;; "releases" directory lists up-to-date as we add new versions:
(for ([r (in-list all-releases)])
  (define v (release-version r))
  (index-page download-index-site
              (format "docs/~a" v)
              '(("html" . dir)
                ("pdf" . dir))))
(void
 (index-page download-index-site
             "installers"
             (for/list ([r (in-list all-releases)])
               (cons (release-version r) 'dir))))
(void
 (index-page download-index-site
             "docs"
             (for/list ([r (in-list all-releases)])
               (cons (release-version r) 'dir))))
(void
 (index-page download-index-site
             "releases"
             (for/list ([r (in-list all-releases)]
                        #:when (version<=? "5.92" (release-version r)))
               (cons (release-version r) 'dir))))
