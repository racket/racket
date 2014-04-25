#;#;
#<<END
TR opt: dead-case-lambda.rkt 4:10 () -- dead case-lambda branch
TR opt: dead-case-lambda.rkt 6:10 (d . rst) -- dead case-lambda branch
END
#<<END
(arity-at-least 0)

END

#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(procedure-arity
  (ann (case-lambda
         [() (void)]
         [(d) (void)]
         [(d . rst) (void)])
          (Any -> Any)))
