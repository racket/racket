#lang scheme/base

(require (for-syntax scheme/base scheme/provide-transform))

(provide matching-identifiers-out)
(define-syntax matching-identifiers-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ rx spec)
        (regexp? (syntax-e #'rx))
        (let ([rx (syntax-e #'rx)])
          (filter (lambda (e)
                    (regexp-match? rx (symbol->string (export-out-sym e))))
                  (expand-export #'spec modes)))]))))

#| Cute, and symmetric to subtract-in, but useless
(provide subtract-out)
(define-syntax subtract-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ spec specs ...)
        (let* ([subs (map (lambda (spec) (expand-export spec modes))
                          (syntax->list #'(specs ...)))]
               [subs (map (lambda (i) (syntax-e (export-out-sym i)))
                          (apply append subs))])
          (filter (lambda (i) (not (memq (export-out-sym i) subs)))
                  (expand-export #'spec modes)))]))))
|#
