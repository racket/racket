#;
(exn-pred exn:fail:syntax? #rx".*Cannot infer.*Please add more type annotations.*")
#lang typed/racket
(define ss '("one" "two" "three")) ; (Listof String)
(sort ss string<?)
