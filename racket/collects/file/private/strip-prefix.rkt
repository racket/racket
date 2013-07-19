#lang racket/base

(provide strip-prefix)

(define (strip-prefix filename strip-count)
  (if (zero? strip-count)
      filename
      (let-values ([(name count)
                    (let loop ([fn filename])
                      (define-values (base name dir?) (split-path fn))
                      (cond
                       [(eq? 'relative base)
                        (values 'same strip-count)]
                       [else
                        (define-values (res count) (loop base))
                        (if (count . <= . 1)
                            (if (eq? res 'same)
                                (values name 0)
                                (values (build-path res name) 0))
                            (values res (sub1 count)))]))])
        (if (and (zero? count)
                 (not (eq? name 'same)))
            name
            #f))))
