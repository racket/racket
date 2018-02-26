#lang racket/base
(provide is-sep?)

(define (is-sep? c convention)
  (or (eq? c (char->integer #\/))
      (and (eq? convention 'windows)
           (eq? c (char->integer #\\)))))
