#lang racket/base
(require setup/language-family)

(provide get-family-name)

(define (get-family-name family
                         #:who [who #f])
  (cond
    [(not family) #f]
    [else
     (define fams (get-language-families))
     (define fam
       (for/or ([fam (in-list fams)])
          (define name (hash-ref fam 'fam #f))
          (and name
               (string-ci=? family name)
               fam)))
     (unless fam
       (raise-user-error who
                         "unrecognzed language family: ~a"
                         family))
     (hash-ref fam 'fam)]))
