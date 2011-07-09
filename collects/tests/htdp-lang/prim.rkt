#lang racket/base

(define (check-bad form)
  (with-handlers ([exn:fail? (lambda (exn)
                               (define msg (exn-message exn))
                               (unless (regexp-match #rx"unbound identifier.*y$" msg)
                                 (raise exn)))])
    (expand form)
    (error 'check-bad "failed: ~v" form)))

(define (check-good form)
  (expand form))

(check-bad `(,#'module m racket/base (require lang/prim) (define-primitive x y)))
(check-bad `(,#'module m racket/base (require lang/prim) (provide-primitive y)))
(check-good `(,#'module m racket/base (require lang/prim) (provide-primitive y) (define (y z) z)))

