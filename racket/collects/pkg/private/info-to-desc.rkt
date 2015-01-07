#lang racket/base
(require racket/match
         "../path.rkt"
         "desc.rkt"
         "dirs.rkt"
         "repo-path.rkt")

(provide pkg-info->desc)

(define (pkg-info->desc name info
                        #:checksum [checksum (pkg-info-checksum info)]
                        #:auto? [auto? (pkg-info-auto? info)])
  (define (to-absolute-string path)
    (path->string
     (simplify-path (path->complete-path path (pkg-installed-dir)))))
  (match (pkg-info-orig-pkg info)
    [`(clone ,path ,url-str)
     (pkg-desc url-str 'clone name
               checksum auto?
               (enclosing-path-for-repo url-str
                                        (path->complete-path path
                                                             (pkg-installed-dir))))]
    [`(catalog ,lookup-name . ,_)
     (pkg-desc lookup-name 'name name
               checksum auto?
               #f)]
    [`(url ,url-str)
     (pkg-desc url-str #f name
               checksum auto?
               #f)]
    [`(,kind ,path)
     (pkg-desc (to-absolute-string path) kind name
               checksum auto?
               #f)]))
