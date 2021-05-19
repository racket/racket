#lang racket/base

(require setup/dirs setup/main-collects)

(provide doc-path)

;; user-doc-mode can be `false-if-missing' or `never'
(define (doc-path dir name flags under-main? [user-doc-mode #f]
                  #:main? [main? #t])
  (define (user-doc [sub #f])
    (and (not (eq? 'never user-doc-mode))
         (let ([d (find-user-doc-dir)])
           (and (or (not (eq? 'false-if-missing user-doc-mode))
                    (directory-exists? d))
                (if sub (build-path d sub) d)))))
  (cond [(memq 'main-doc-root flags) (if under-main?
                                         (and main? (find-doc-dir))
                                         ;; Effectively no main doc dir:
                                         (user-doc (build-path "main" name)))]
        [(memq 'user-doc-root flags) (user-doc)]
        [(memq 'user-doc flags)      (user-doc name)]
        [(or under-main?
             (memq 'main-doc flags)
             (pair? (path->main-collects-relative dir)))
         (and main?
              ;; check for existing directory in search path (unless
              ;; 'every-main-layer) before assuming the main doc directory
              (or (and (not (memq 'every-main-layer flags))
                       (for/or ([dir (get-doc-extra-search-dirs)])
                         (define p (build-path dir name))
                         (and (directory-exists? p)
                              p)))
                  (build-path (find-doc-dir) name)))]
        [else 
         (and (not (eq? 'never user-doc-mode))
              (build-path dir "doc" name))]))
