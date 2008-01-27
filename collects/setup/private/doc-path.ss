#lang scheme/base

(require setup/dirs
         setup/main-collects)

(provide doc-path)

(define (doc-path dir name flags)
  (cond
   [(memq 'main-doc-root flags)
    (find-doc-dir)]
   [(memq 'user-doc-root flags)
    (find-user-doc-dir)]
   [(memq 'user-doc flags)
    (build-path (find-user-doc-dir) name)]
   [(or (memq 'main-doc flags)
        (pair? (path->main-collects-relative dir)))
    (build-path (find-doc-dir) name)]
   [else
    (build-path dir "doc" name)]))
