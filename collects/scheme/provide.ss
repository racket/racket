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
