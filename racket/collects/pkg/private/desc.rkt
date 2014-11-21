#lang racket/base

(provide (struct-out pkg-desc)
         pkg-desc=?)

(struct pkg-desc (source type name checksum auto? extra-path))

(define (pkg-desc=? a b)
  (define (->list a)
    (list (pkg-desc-source a)
          (pkg-desc-type a)
          (pkg-desc-name a)
          (pkg-desc-checksum a)
          (pkg-desc-auto? a)
          (pkg-desc-extra-path a)))
  (equal? (->list a) (->list b)))
