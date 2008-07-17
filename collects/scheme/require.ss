#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform scheme/list
                     "private/at-syntax.ss"))

(provide matching-identifiers-in)
(define-syntax matching-identifiers-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ rx spec)
        (regexp? (syntax-e #'rx))
        (let ([rx (syntax-e #'rx)])
          (define-values [imports sources] (expand-import #'spec))
          (values (filter (lambda (i)
                            (regexp-match? rx (symbol->string
                                               (syntax-e (import-local-id i)))))
                          imports)
                  sources))]))))

(provide subtract-in)
(define-syntax subtract-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ spec specs ...)
        (let* ([subs (map (lambda (spec)
                            (let-values ([(imports srcs) (expand-import spec)])
                              imports))
                          (syntax->list #'(specs ...)))]
               [subs (map (lambda (i) (syntax-e (import-local-id i)))
                          (apply append subs))])
          (define-values [imports sources] (expand-import #'spec))
          (values (filter (lambda (i)
                            (not (memq (syntax-e (import-local-id i)) subs)))
                          imports)
                  sources))]))))

(provide filtered-in)
(define-syntax filtered-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ proc spec)
        (let ([proc (at-syntax #'proc)])
          (define-values [imports sources] (expand-import #'spec))
          (values
           (filter-map
            (lambda (i)
              (let* ([id (import-local-id i)]
                     [s1 (symbol->string (syntax-e id))]
                     [s2 (proc s1)])
                (cond [(equal? s1 s2) i]
                      [(string? s2) (make-import (datum->syntax
                                                  id (string->symbol s2) id)
                                                 (import-src-sym i)
                                                 (import-src-mod-path i)
                                                 (import-mode i)
                                                 (import-req-mode i)
                                                 (import-orig-mode i)
                                                 (import-orig-stx i))]
                      [(not s2) #f]
                      [else (error 'filtered-in "bad result: ~e" s2)])))
            imports)
           sources))]))))
