#;#;
#<<END
TR opt: case-lambda-dead-branch.rkt 12:5 (x y) -- dead case-lambda branch
TR opt: case-lambda-dead-branch.rkt 6:5 (x y) -- dead case-lambda branch
END
""
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(: f (case-> (Symbol Symbol -> String)))
(define f
  (case-lambda
    (w "hello")
    ((x y) (add1 "hello"))))

(: g (case-> (Symbol -> String)))
(define g
  (case-lambda
    ((x) "hello")
    ((x y) (add1 "hello"))))
