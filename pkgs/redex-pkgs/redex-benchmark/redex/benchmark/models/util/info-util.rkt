#lang racket/base

(require racket/path
         racket/list
         racket/runtime-path)

(provide make-all-mods
         make-path-root)

(define-runtime-path here ".")

(define types '(grammar enum ordered))

(define type->genmod
  (hash 'grammar 'adhoc-mod
        'enum 'enum-mod
        'ordered 'ordered-mod))

(define (get-name/modpaths filename type path-root)
  (define model-name (first (regexp-split #rx"\\." (last (regexp-split #rx"/" filename)))))
  (define mod-path (string->symbol
                    (string-append
                     (symbol->string path-root)
                     "/"
                     model-name)))
  (list (first (regexp-split #rx"\\." (last (regexp-split #rx"/" filename))))
        `(submod ,mod-path generators ,(hash-ref type->genmod type))
        `(submod ,mod-path generators check-mod)))

(define (all-mods/type type the-dir name-base path-root)
  (for/list ([f (in-directory the-dir)]
             #:when (regexp-match (regexp (string-append name-base "-[0-9]+\\.rkt$")) 
                                  (path->string f)))
    (get-name/modpaths f type path-root)))

(define (make-all-mods the-dir name-base path-root)
  (apply append
         (for/list ([t (in-list types)])
           (all-mods/type t the-dir name-base path-root))))

(define (make-path-root model-dir)
  (string->symbol
   (string-append
    "redex/benchmark/models/"
    (symbol->string model-dir))))
