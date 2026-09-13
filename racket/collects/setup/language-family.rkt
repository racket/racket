#lang racket/base
(require setup/dirs
         setup/getinfo)

(provide get-language-families)

(define (get-language-families #:user? [user? #t]
                               #:namespace [namespace #f])
  (for/list ([dir (in-list (find-relevant-directories '(language-family)
                                                      (cond
                                                        [user? 'preferred]
                                                        [else 'no-user])))]
             #:do [(define info (get-info/full dir #:namespace namespace))]
             #:when info
             #:do [(define fams (info 'language-family (lambda () null)))]
             #:when (and (list? fams) (andmap hash? fams))
             [fam (in-list fams)])
    fam))
