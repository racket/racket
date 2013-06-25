#lang typed/racket
(define ss '("one" "two" "three")) ; (Listof String)
(sort ss string<?)
