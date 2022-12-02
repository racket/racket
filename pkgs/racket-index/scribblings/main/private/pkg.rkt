#lang racket/base

(provide get-main-dist-pkgs
         tag->pkg)

(require racket/match
         racket/set
         pkg/lib
         setup/main-collects
         setup/getinfo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-main-dist-pkgs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAIN-DIST-NAME "main-distribution")
(define main-dist-pkgs #f)
(define pkg-cache-for-pkg-directory (make-hash))

(define (get-main-dist-pkgs)
  (unless main-dist-pkgs
    (set! main-dist-pkgs
          (match (pkg-directory MAIN-DIST-NAME
                                #:cache pkg-cache-for-pkg-directory)
            [#f '()]
            [_ (find-main-dist-pkgs)])))
  main-dist-pkgs)

(define (find-main-dist-pkgs)
  (define result '())
  (define seen (mutable-set))
  (let loop ([pkg MAIN-DIST-NAME])
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
           (loop dep))])))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag->pkg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pkg-cache-for-path->pkg (make-hash))

(define (module-path->path module-path)
  (with-handlers ([exn:missing-module? (lambda (exn) #f)])
    (resolved-module-path-name
     (module-path-index-resolve
      (module-path-index-join module-path #f)))))

(define (->pkg path)
  (cond
    [(path->pkg path #:cache pkg-cache-for-path->pkg)]
    [else (and (path->main-collects-relative path) "base")]))

(define (->path tag)
  (match tag
    [(or (list 'part (list path-string _ ...))
         (list 'idx (list 'gentag _ path-string _ ...))
         (list 'tech (list path-string _ ...)))

     (match path-string
       ;; remove /xxx.scrbl
       [(pregexp #px"^\\(lib (.*)\\)$" (list _ path))
        (module-path->path (list 'lib path))])]

    [(or (list 'def (list path _ ...))
         (list 'form (list path _ ...))
         (list 'meth (list (list path _ ...) _))
         (list 'sig-val (list path _ ...)))
     (module-path->path path)]

    [(list 'mod-path path-string)
     (module-path->path (read (open-input-string path-string)))]

    ;; HACK: xrepl weirdly forges their own tag.
    ;; Since it is a part of the main distribution, we add a hack to handle it
    [(list 'xrepl _) 'xrepl]

    [pkg
     (log-debug "search script: unrecognized tag shape: ~s" pkg)
     #f]))

(define (tag->pkg tag)
  (let loop ([path (->path tag)])
    (match path
      [(? path? path) (->pkg path)]
      ['xrepl "xrepl"]
      ['#%kernel "base"]
      ['#%foreign "base"]
      [(cons path _) (loop path)]
      [_ #f])))
