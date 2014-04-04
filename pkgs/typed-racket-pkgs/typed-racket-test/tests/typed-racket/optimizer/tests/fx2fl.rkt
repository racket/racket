#;#;
#<<END
TR missed opt: fx2fl.rkt 15:40 (* 1024 i) -- out of fixnum range
TR opt: fx2fl.rkt 10:20 (+ s (/ 1.0 (fx->fl i))) -- binary float
TR opt: fx2fl.rkt 10:25 (/ 1.0 (fx->fl i)) -- binary float
TR opt: fx2fl.rkt 10:32 (fx->fl i) -- fixnum to float
TR opt: fx2fl.rkt 12:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl (* 1024 i))))) -- dead else branch
TR opt: fx2fl.rkt 12:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl (* 1024 i))))) -- dead else branch
TR opt: fx2fl.rkt 12:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl (* 1024 i))))) -- dead else branch
TR opt: fx2fl.rkt 13:22 s -- dead else branch
TR opt: fx2fl.rkt 13:22 s -- dead else branch
TR opt: fx2fl.rkt 15:20 (+ s (/ 1.0 (fx->fl (* 1024 i)))) -- binary float
TR opt: fx2fl.rkt 15:25 (/ 1.0 (fx->fl (* 1024 i))) -- binary float
TR opt: fx2fl.rkt 7:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl i)))) -- dead else branch
TR opt: fx2fl.rkt 7:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl i)))) -- dead else branch
TR opt: fx2fl.rkt 7:9 (for/fold: : Float ((s : Float 0.0)) ((i : Fixnum (in-range 1 100000001))) (+ s (/ 1.0 (fx->fl i)))) -- dead else branch
TR opt: fx2fl.rkt 8:22 s -- dead else branch
TR opt: fx2fl.rkt 8:22 s -- dead else branch
END
""
#lang typed/racket
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(require racket/fixnum)

;; from http://stackoverflow.com/questions/22868795/how-to-optimize-this-piece-of-racket-code

(define: (test) : Float
         (for/fold: : Float
                    ([s : Float 0.0])
                    ([i : Fixnum (in-range 1 100000001)])
                    (+ s (/ 1.0 (fx->fl i))))) ; should be optimized
(define: (test2) : Float
         (for/fold: : Float
                    ([s : Float 0.0])
                    ([i : Fixnum (in-range 1 100000001)])
                    (+ s (/ 1.0 (fx->fl (* 1024 i)))))) ; should not
