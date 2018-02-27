#lang racket/base
(require "../common/inline.rkt"
         "config.rkt")

;; The reader should never use `read-char` or `peek-char`. Instead,
;; use `read-char/special` or `peek-char/special`, so that special
;; values are never treated as characters, and so that `read-syntax`
;; mode provides the source name.

(provide (struct-out special)
         read-char/special
         peek-char/special)

(struct special (value))

(define-inline (read-char/special in config [source (read-config-source config)])
  (read-char-or-special in special source))

;; Returns `(special 'special)` for any special value:
(define-inline (peek-char/special in config [skip-count 0] [source (read-config-source config)])
  (define c (peek-char-or-special in skip-count 'special source))
  (if (eq? c 'special)
      (special 'special)
      c))
