#lang racket/base
(require "../getinfo.rkt"
         "../collection-name.rkt")

(provide get-relevant-collections)

;; If this is called before the info-domain step, then it can
;; only find information previous registered. That can be ok for
;; `--doc-index`, for example.
(define (get-relevant-collections key get-info #:mode mode)
  (define dirs (find-relevant-directories (list key) mode))
  (for/list ([dir (in-list dirs)]
             #:do [(define info (get-info dir))]
             #:when info
             #:do [(define colls (info key (lambda () null)))]
             #:when (or (list? colls)
                        (string? colls))
             [coll (in-list (if (string? colls) (list colls) colls))]
             #:when (collection-name? coll)
             #:when (collection-path coll #:fail (lambda (x) #f)))
    coll))
