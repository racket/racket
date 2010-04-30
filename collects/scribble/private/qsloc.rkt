#lang scheme/base
(require (for-syntax scheme/base))

(provide quote-syntax/loc)

(define-syntax (quote-syntax/loc stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([loc (let ([s #'id])
                          (vector (syntax-source s)
                                  (syntax-line s)
                                  (syntax-column s)
                                  (syntax-position s)
                                  (syntax-span s)))])
       #'(let ([s (*quote-syntax/loc id)])
           (datum->syntax s (syntax-e s) 'loc s)))]))

(define-syntax *quote-syntax/loc
  (syntax-rules ()
    [(_ (sub ...)) (datum->syntax #f (list (quote-syntax/loc sub) ...))]
    [(_ id) (quote-syntax id)]))

