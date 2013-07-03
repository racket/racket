#lang racket/base
(require string-constants)

(provide textbook-pls)

(define textbook-pls
  (list (list '("htdp-icon.gif" "icons")
              "How to Design Programs"
              (string-constant teaching-languages)
              (string-constant how-to-design-programs)
              (string-constant beginning-student))))
