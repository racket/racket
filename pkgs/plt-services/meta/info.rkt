#lang info

(define name "Infrastructure code")
(define compile-omit-paths '("images/taking-screenshots/racket-widget.scm"
                             "tests/unix-installer.rkt"))
(define test-omit-paths
  '("images/mkheart.rkt"
    "pkg-index/official"
    "pkg-index/planet-compat"
    "tests/unix-installer.rkt"))


(define test-responsibles '(("props" (eli jay))
                            (all eli)))
