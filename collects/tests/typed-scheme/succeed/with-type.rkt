#lang scheme
(require typed/scheme)

(with-type #:result Number 3)

(let ([x "hello"])
  (with-type #:result String
    #:freevars ([x String])
    (string-append x ", world")))

(define-values (a b)
  (with-type #:result (values Number String)
             (values 3 "foo")))

(with-type ([fun (Number -> Number)]
            [val Number])
  (define (fun x) x)
  (define val 17))

(fun val)
val

