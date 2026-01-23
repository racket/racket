#lang racket/base
(require setup/collection-name)

(provide validate-scribblings-infos)

(define (scribblings-flag? sym)
  (memq sym '(main-doc main-doc-root user-doc-root user-doc multi-page
                       depends-all depends-all-main depends-all-user depends-family
                       no-depend-on always-run keep-style no-search
                       every-main-layer)))

(define (validate-scribblings-infos infos)
  (define (validate path [flags '()] [cat '(library)] [name #f] [out-count 1] [order-hint 0])
    (and (string? path) (relative-path? path)
         (list? flags) (andmap scribblings-flag? flags)
         (or (not name) (collection-name-element? name))
         (and (list? cat)
              (<= 1 (length cat) 3)
              (or (symbol? (car cat))
                  (string? (car cat)))
              (or (null? (cdr cat))
                  (and (real? (cadr cat))
                       (or (null? (cddr cat))
                           (let ([fam (caddr cat)])
                             (and (list? fam)
                                  (andmap string? fam)))))))
         (and (exact-positive-integer? out-count))
         (and (real? order-hint))
         (list path flags cat
               (or name (let-values ([(_1 name _2) (split-path path)])
                          (path-replace-suffix name #"")))
               out-count
               order-hint)))
  (and (list? infos)
       (let ([infos (map (lambda (i)
                           (and (list? i) (<= 1 (length i) 6)
                                (apply validate i)))
                         infos)])
         (and (not (memq #f infos)) infos))))
