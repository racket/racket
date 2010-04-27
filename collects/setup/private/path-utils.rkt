#lang scheme/base

(require setup/dirs
         setup/main-collects
         setup/path-relativize
         unstable/dirs
         (rename-in planet/config [CACHE-DIR planet-dir]))

(provide doc-path path->name)

;; user-doc-mode can be `false-if-missing' or `never'
(define (doc-path dir name flags [user-doc-mode #f])
  (define (user-doc [sub #f])
    (and (not (eq? 'never user-doc-mode))
         (let ([d (find-user-doc-dir)])
           (and (or (not (eq? 'false-if-missing user-doc-mode))
                    (directory-exists? d))
                (if sub (build-path d sub) d)))))
  (cond [(memq 'main-doc-root flags) (find-doc-dir)]
        [(memq 'user-doc-root flags) (user-doc)]
        [(memq 'user-doc flags)      (user-doc name)]
        [(or (memq 'main-doc flags) (pair? (path->main-collects-relative dir)))
         (build-path (find-doc-dir) name)]
        [else (build-path dir "doc" name)]))

;; Similar to path->string, except when the path is relative to the
;; main collects directory, which returns a string with just the
;; relative subpath inside collects.  Used for producing less verbose
;; printouts during compilation, so the input path is usually
;; complete, otherwise it can be ambiguous, so use only when it's
;; clear from the context what path is shown.  (To be used only for
;; human-readable output.)  Generalized for any base directory and an
;; indicative prefix.
(define (path->name path #:prefix [prefix #f] #:base [find-base #f])
  (path->directory-relative-string
   path
   #:dirs (cond
           [find-base (list (cons find-base prefix))]
           [prefix (list (cons find-collects-dir prefix))]
           [else setup-relative-directories])))
