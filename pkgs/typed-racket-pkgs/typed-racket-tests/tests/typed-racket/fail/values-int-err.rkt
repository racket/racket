#;
(exn:pred (lambda (e) (not (regexp-match? "match:" e))))


#lang typed/racket/base

(: bob (-> (Values Real Real)))
(define (bob) 0)
