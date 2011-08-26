#lang racket/base
(require racket/contract/base
         "private/modhelp.rkt")

(define (force-relto relto dir?)
  (cond [(path-string? relto)
         (if dir?
             (let-values ([(base n d?) (split-path relto)])
               (when d?
                 (error 'resolve-module-path-index
                        "given a directory path instead of a file path: ~e" relto))
               (if (eq? base 'relative)
                   (or (current-load-relative-directory) (current-directory))
                   base))
             relto)]
        [(pair? relto) relto]
        [(not dir?)
         (error 'resolve-module-path-index
                "can't resolve \"self\" with non-path relative-to: ~e" relto)]
        [(procedure? relto) (relto)]
        [else (current-directory)]))

(define (path-ss->rkt p)
  (let-values ([(base name dir?) (split-path p)])
    (if (regexp-match #rx"[.]ss$" (path->bytes name))
        (path-replace-suffix p #".rkt")
        p)))

(define (resolve-module-path s relto)
  ;; relto should be a complete path, #f, or procedure that returns a
  ;; complete path
  (define (get-dir) (force-relto relto #t))
  (cond [(symbol? s)
         ;; use resolver handler:
         (resolved-module-path-name
          (module-path-index-resolve
           (module-path-index-join s #f)))]
        [(string? s)
         ;; Parse Unix-style relative path string
         (path-ss->rkt
          (apply build-path (get-dir) (explode-relpath-string s)))]
        [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
         #f]
        [(or (path? s) (eq? (car s) 'file))
         (let ([p (if (path? s) s (expand-user-path (cadr s)))])
           (path-ss->rkt
            (path->complete-path
             p (let ([d (get-dir)])
                 (if (path-string? d)
                     d
                     (or (current-load-relative-directory)
                         (current-directory)))))))]
        [(or (eq? (car s) 'lib)
             (eq? (car s) 'quote)
             (eq? (car s) 'planet))
         ;; use resolver handler in this case, too:
         (resolved-module-path-name
          (module-path-index-resolve
           (module-path-index-join s #f)))]
        [else #f]))

(define (resolve-module-path-index mpi relto)
  ;; relto must be a complete path
  (let-values ([(path base) (module-path-index-split mpi)])
    (if path
        (resolve-module-path path (resolve-possible-module-path-index base relto))
        (path-ss->rkt (force-relto relto #f)))))

(define (resolve-possible-module-path-index base relto)
  (cond [(module-path-index? base)
         (resolve-module-path-index base relto)]
        [(and (resolved-module-path? base)
              (path? (resolved-module-path-name base)))
         (resolved-module-path-name base)]
        [relto relto]
        [else #f]))


(define rel-to-path-string/thunk/#f
  (or/c path-string? (-> path-string?) false/c))

(provide/contract
 [resolve-module-path (module-path-v? rel-to-path-string/thunk/#f
                                      . -> . (or/c path? symbol?))]
 [resolve-module-path-index ((or/c symbol? module-path-index?)
                             rel-to-path-string/thunk/#f
                             . -> . (or/c path? symbol?))])
