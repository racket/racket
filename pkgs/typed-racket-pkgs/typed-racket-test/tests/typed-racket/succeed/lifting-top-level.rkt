#lang racket/load

;; Test to make sure lifting is okay at the top-level for TR
;;
;; Would be best as a unit test, but the local expansion done in
;; tests is different from the local expansion done for #%top-interaction

(require typed/racket)

(define-syntax (m stx)
  (syntax-local-lift-expression #'(string-append "foo" "bar")))
(m)

(define-syntax (n* stx)
  (syntax-local-lift-expression #'(string-append "foo" "bar")))
(define-syntax (m* stx)
  (syntax-local-lift-expression #'(n*)))
(m*)
