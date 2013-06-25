#;
(exn-pred 2)
#lang typed-scheme

(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (case-lambda: (([x : Number] . [y : Number ... a]) x))
                 w))

(plambda: (a ...) ([z : String] . [w : Number *])
          (apply (case-lambda: (([x : Number] . [y : Number ... a]) x)
                               (([x : String] [y : String] . [z : String *]) 0)
                               ([y : String *] 0))
                 w))
