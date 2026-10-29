#lang racket/base
(require rackunit
         rackunit/docs-complete
         racket/port
         syntax/location)

;; Tests for `#:skip` in `check-docs` (issue #5622)

(module exports racket/base
  (provide undocumented-a undocumented-b)
  (define undocumented-a 1)
  (define undocumented-b 2))

(define mod (quote-module-path exports))

(define (check-docs-output skip)
  (with-output-to-string
    (λ ()
      (parameterize ([current-error-port (current-output-port)])
        (check-docs mod #:skip skip)))))

;; skipping a symbol should still report the other undocumented export
(let ([out (check-docs-output 'undocumented-a)])
  (check-regexp-match #rx"undocumented-b" out)
  (check-false (regexp-match? #rx"undocumented-a" out)))

;; skipping with a list of regexps should not raise an error
(let ([out (check-docs-output (list #rx"^undocumented-a$"))])
  (check-regexp-match #rx"undocumented-b" out)
  (check-false (regexp-match? #rx"undocumented-a" out)))

;; skipping with a list of symbols
(let ([out (check-docs-output (list 'undocumented-a))])
  (check-regexp-match #rx"undocumented-b" out)
  (check-false (regexp-match? #rx"undocumented-a" out)))
