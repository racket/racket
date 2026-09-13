#lang racket/base

(provide get-base-pkgs
         get-main-dist-pkgs)

(require racket/match
         racket/set
         pkg/lib
         setup/getinfo
         setup/dirs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-main-dist-pkgs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define base-pkgs #f)
(define main-dist-pkgs #f)
(define pkg-cache-for-pkg-directory (make-hash))

(define (get-base-pkgs)
  (unless base-pkgs
    (set! base-pkgs (find-pkgs (get-base-documentation-packages))))
  base-pkgs)

(define (get-main-dist-pkgs)
  (unless main-dist-pkgs
    (set! main-dist-pkgs (find-pkgs (get-distribution-documentation-packages)
                                    #:exclude (list->set (get-base-pkgs)))))
  main-dist-pkgs)

(define (find-pkgs root-pkg-names #:exclude [excludes (set)])
  (define result '())
  (define seen (set-copy excludes))
  (for ([root-pkg-name (in-list root-pkg-names)])
    (match (pkg-directory
            root-pkg-name
            #:cache pkg-cache-for-pkg-directory)
      [#f '()]
      [_
       (let loop ([pkg root-pkg-name])
         (unless (set-member? seen pkg)
           (set-add! seen pkg)
           (match (pkg-directory pkg #:cache pkg-cache-for-pkg-directory)
             [#f
              ;; these are platform dependent packages (like racket-win32-i386-3)
              ;; they have no deps, and if they are platform dependent,
              ;; they are not that useful (for documentation search) anyway
              (set! result (cons pkg result))]
             [dir
              (set! result (cons pkg result))
              (define get-info (get-info/full dir))
              (define direct-deps
                (for/list ([dep (extract-pkg-dependencies get-info #:build-deps? #f)])
                  (match dep
                    [(? string?) dep]
                    [(cons dep _) dep])))
              ;; we need to recur. For example, 2dtabular is in 2d-lib,
              ;; which is not a direct dep of main-distribution
              (for ([dep direct-deps])
                (loop dep))])))]))
  result)
