#lang racket/base
(require setup/language-family)

(provide get-family-name)

(define (get-family-name family
                         #:who [who #f])
  (cond
    [(not family) #f]
    [else
     (define fams (sort (get-language-families)
                        (lambda (a b)
                          (define ao (hash-ref a 'order 0))
                          (define bo (hash-ref b 'order 0))
                          (cond
                            [(= ao bo) (string-ci<? (hash-ref a 'family "???")
                                                    (hash-ref b 'family "???"))]
                            [else (< bo ao)]))))
     (define-values (name found-kind)
       (for/fold ([found #f] [found-kind #f]) ([fam (in-list fams)])
         (define name (hash-ref fam 'family #f))
         (define (next) (values found found-kind))
         (cond
           [(not name) (next)]
           [(eq? found-kind 'exact) (next)]
           [(string-ci=? family name) (values name 'exact)]
           [(eq? found-kind 'start) (next)]
           [(and ((string-length family) . < . (string-length name))
                 (string-ci=? family (substring name 0 (string-length family))))
            (values name 'start)]
           [else (values found found-kind)])))
     (unless name
       (raise-user-error who
                         "unrecognzed language family: ~a"
                         family))
     name]))
