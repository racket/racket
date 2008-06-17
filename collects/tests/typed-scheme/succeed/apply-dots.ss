#lang typed-scheme

(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (lambda: ([x : Number] . [y : Number ... a]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number ... a])
          (apply (lambda: ([x : Number] . [y : Number]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number])
          (apply (lambda: ([x : Number] . [y : Number]) x)
                 1 w))

(plambda: (a ...) ([z : String] . [w : Number])
          (apply (case-lambda: (([x : Number] . [y : Number ... a]) x)
                               (([x : String] [y : String] . [z : String]) 0)
                               ([y : Number] 0))
                 w))