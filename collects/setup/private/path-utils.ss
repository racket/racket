#lang scheme/base

(require setup/dirs
         setup/main-collects
         scheme/list)

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
;; human-readable output.)
(define (path->name path)
  (if (not (complete-path? path))
    (if (string? path) path (path->string path))
    (let ([rel (path->main-collects-relative path)])
      (if (pair? rel)
        (bytes->string/utf-8
         (apply bytes-append
                (cdr (append-map (lambda (p) (list #"/" p)) (cdr rel)))))
        (path->string rel)))))
