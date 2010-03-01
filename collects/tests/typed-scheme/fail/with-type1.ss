#;
(exn-pred exn:fail:contract?)
#lang scheme
(require typed/scheme)

((with-type #:result (Number -> Number)
   (lambda: ([x : Number]) (add1 x)))
 #f)
