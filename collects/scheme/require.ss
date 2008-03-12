#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform))

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
