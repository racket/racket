#lang racket/base
(require typed-scheme/typed-reader)
(provide get-info configure)

(define ((get-info arg) key default)
  (case key
    [(configure-runtime) `(#(typed-scheme/language-info configure ()))]
    [else default]))

;; options currently always empty
(define (configure options)
  (namespace-require 'racket/base)
  (eval '(begin
           (require (for-syntax typed-scheme/utils/tc-utils racket/base))
           (begin-for-syntax (set-box! typed-context? #t)))
        (current-namespace))
  (current-readtable (readtable)))
