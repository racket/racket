#lang info

(define mred-launcher-libraries (list "start.rkt"))
(define mred-launcher-names (list "Slideshow"))
(define compile-omit-paths '("initial-ones.rkt" "examples"))
(define test-omit-paths '("tutorial-show.rkt"))

(define binary-keep-files '("tutorial-show.rkt"
                            "initial-ones.rkt"
                            "examples"))
