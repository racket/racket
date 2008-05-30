#lang scheme/base

(require setup/dirs
         setup/main-collects
         setup/path-relativize
         scheme/list
         (rename-in planet/config [CACHE-DIR planet-dir]))

(provide doc-path path->name)

(define (doc-path dir name flags)
  (cond [(memq 'main-doc-root flags) (find-doc-dir)]
        [(memq 'user-doc-root flags) (find-user-doc-dir)]
        [(memq 'user-doc flags)      (build-path (find-user-doc-dir) name)]
        [(or (memq 'main-doc flags)
             (pair? (path->main-collects-relative dir)))
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
(define (path->rel path find-base)
  ((if (not find-base)
     path->main-collects-relative
     (let-values ([(path->rel rel->path)
                   (make-relativize find-base 'rel 'path->rel 'rel->path)])
       path->rel))
   path))
(define (path->name path #:prefix [prefix #f] #:base [find-base #f])
  (define (try find-base prefix)
    (define rel (path->rel path find-base))
    (and (pair? rel)
         (let* ([p (cdr (append-map (lambda (p) (list #"/" p)) (cdr rel)))]
                [p (bytes->string/utf-8 (apply bytes-append p))])
           (if prefix (format "<~a>/~a" prefix p) p))))
  (define (->string) (if (string? path) path (path->string path)))
  (if (not (complete-path? path))
    (->string)
    (or (try find-base prefix)
        ;; by default (both optionals missing) try the user
        ;; collections and planet too
        (and (not (or prefix find-base))
             (or (try find-user-collects-dir 'user)
                 (try planet-dir 'planet)))
        (->string))))
