#;#;
#<<END
TR opt: with-type.rkt 6:18 (#%app + x val) -- binary float
END
#<<END
34.6

END
#lang racket
#reader tests/typed-racket/optimizer/reset-port

(require typed/racket)

(with-type ([fun (Float -> Float)]
            [val Float])
  (define (fun x) (+ x val))
  (define val 17.3))

(fun val)
