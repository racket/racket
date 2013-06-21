#;#;
#<<END
TR opt: case-lambda-dead-branch.rkt 13:5 (x y) -- dead case-lambda branch
TR opt: case-lambda-dead-branch.rkt 19:5 (x y) -- dead case-lambda branch
END
""
#lang typed/racket

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
