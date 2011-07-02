#lang setup/infotab

(define name "HtDP Teachpacks")
(define compile-omit-paths
  '("hangman-world.rkt" "hangman-world-play.rkt"
    ;; TEMPORARY DISABLE THESE FILES UNTIL FIXED
    ;; "matrix.rkt" "matrix-client.rkt" "matrix-invisible.rkt"
    ;; "matrix-render-sig.rkt" "matrix-sig.rkt" "matrix-unit.rkt"
    "tests"))

(define scribblings '(("htdp.scrbl")))
