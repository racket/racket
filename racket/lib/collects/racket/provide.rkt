#lang racket/base

(require (for-syntax racket/base racket/provide-transform racket/list
                     (only-in racket/syntax syntax-local-eval)))

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

(provide filtered-out)
(define-syntax filtered-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-case stx ()
       [(_ proc spec)
        (let ([proc (syntax-local-eval #'proc)])
          (filter-map
           (lambda (e)
             (let* ([s1 (symbol->string (export-out-sym e))]
                    [s2 (proc s1)])
               (cond [(equal? s1 s2) e]
                     [(string? s2) (make-export (export-local-id e)
                                                (string->symbol  s2)
                                                (export-mode     e)
                                                (export-protect? e)
                                                (export-orig-stx e))]
                     [(not s2) #f]
                     [else (error 'filtered-out "bad result: ~e" s2)])))
           (expand-export #'spec modes)))]))))
