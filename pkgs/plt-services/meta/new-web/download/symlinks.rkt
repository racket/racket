#lang plt-web
(require "resources.rkt"
         "data.rkt"
         version/utils)

(provide (all-defined-out))

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
