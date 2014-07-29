#lang info

(define name "Infrastructure code")
(define compile-omit-paths '("images/taking-screenshots/racket-widget.scm"))
(define test-omit-paths
  '("drdr"
    "drdr2"
    "images/mkheart.rkt"
    "pkg-index/official"
    "pkg-index/planet-compat"
    "pkg-push"))


(define test-responsibles '(("props" (eli jay))
                            (all eli)))
