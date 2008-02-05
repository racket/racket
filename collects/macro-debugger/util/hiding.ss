
#lang scheme/base
(provide (all-defined-out))

(define (scheme-module? mpi)
  (let ([abs (find-absolute-module-path mpi)])
    (and abs
         (or (base-module-path? abs)
             (scheme-lib-module-path? abs)))))

(define (lib-module? mpi)
  (let ([abs (find-absolute-module-path mpi)])
    (and abs (lib-module-path? abs))))

(define (find-absolute-module-path mpi)
  (and (module-path-index? mpi)
       (let-values ([(path rel) (module-path-index-split mpi)])
         (cond [(and (pair? path) (memq (car path) '(quote lib planet)))
                path]
               [(symbol? path) path]
               [(string? path) (find-absolute-module-path rel)]
               [else #f]))))

(define (base-module-path? mp)
  (and (pair? mp)
       (eq? 'quote (car mp))
       (regexp-match #rx"^#%" (symbol->string (cadr mp)))))

(define (scheme-lib-module-path? mp)
  (cond [(symbol? mp)
         (scheme-collection-name? (symbol->string mp))]
        [(and (pair? mp) (eq? (car mp) 'lib))
         (cond [(string? (cadr mp)) (null? (cddr mp))
                (scheme-collection-name? (cadr mp))]
               [(symbol? (cadr mp))
                (scheme-collection-name? (symbol->string (cadr mp)))]
               [else #f])]
        [else #f]))

(define (scheme-collection-name? path)
  (or (regexp-match? #rx"^scheme(/.)?" path)
      (regexp-match? #rx"^mzscheme(/.)?" path)))

(define (lib-module-path? mp)
  (or (symbol? mp)
      (and (pair? mp) (memq (car mp) '(lib planet)))))
