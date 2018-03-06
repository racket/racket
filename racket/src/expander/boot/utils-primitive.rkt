#lang racket/base
(require racket/private/config
         "../common/reflect-hash.rkt"
         (only-in "../eval/load.rkt" load/use-compiled)
         "../eval/collection.rkt")

(provide utils-primitives)

;; These functions are a small step away from `#%kernel`, and they
;; have traditionally been available as the `#%utils` module. Don't
;; use `#%utils` in `racket/base`, since that's where the actual
;; implementation sometimes is. We turn the functions into a
;; "primitive" module using this table in a bootstrapped load.

(define utils-primitives
  (reflect-hash path-string?
                normal-case-path
                path-replace-extension
                path-add-extension
                reroot-path

                path-list-string->path-list
                
                find-executable-path
                
                call-with-default-reading-parameterization
                
                collection-path
                collection-file-path
                find-library-collection-paths
                find-library-collection-links
                
                load/use-compiled

                find-main-config
                find-main-collects))
