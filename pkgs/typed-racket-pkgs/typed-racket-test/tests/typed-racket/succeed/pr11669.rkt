#lang racket/load

(require typed/racket)

;; Test that struct: and define-struct: work at the
;; top-level.
;;
;; Test for PR 11669
(struct: Foo ([x : Integer]))
(define-struct: Bar ([y : Foo]))
(define-type Qux (U String Integer))
(struct: Quux ([qux : Qux]))
Quux-qux
Foo
make-Bar

