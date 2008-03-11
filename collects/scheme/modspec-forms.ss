#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform))

(provide matching-identifiers-in)
(define-syntax matching-identifiers-in
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ rx spec)
        (regexp? (syntax-e #'rx))
        (let*-values ([(rx) (syntax-e #'rx)]
                      [(imports sources) (expand-import #'spec)])
          (values
           (filter (lambda (i)
                     (regexp-match? rx (symbol->string
                                        (syntax-e (import-local-id i)))))
                   imports)
           sources))]))))
