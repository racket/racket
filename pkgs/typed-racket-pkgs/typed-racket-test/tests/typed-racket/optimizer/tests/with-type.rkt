#;
#<<END
TR opt: with-type.rkt 14:18 (#%app + x val) -- binary float
34.6

END

#lang racket

(require typed/racket)

(with-type ([fun (Float -> Float)]
            [val Float])
  (define (fun x) (+ x val))
  (define val 17.3))

(fun val)
