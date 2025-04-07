#lang racket/base

(module+ test
  (require
   (only-in rackunit
            check-true
            check-false)
   (only-in xml/private/xexpr-core
            non-empty-symbol?))

  (check-true (non-empty-symbol? 'x))
  (check-false (non-empty-symbol? '||))
  (check-false (non-empty-symbol? 0))
  (check-false (non-empty-symbol? (string->uninterned-symbol "")))
  (check-true (non-empty-symbol? (string->uninterned-symbol "x"))))

