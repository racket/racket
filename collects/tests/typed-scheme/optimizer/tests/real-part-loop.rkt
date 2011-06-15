#;
(
TR opt: real-part-loop.rkt 30:1 (letrec-values (((loop) (lambda (v) (if (#%app > (#%app real-part v) (quote 70000.2)) (quote 0) (#%app loop (#%app + v (quote 3.6))))))) loop) -- unboxed call site
TR opt: real-part-loop.rkt 30:6 loop -- fun -> unboxed fun
TR opt: real-part-loop.rkt 30:6 loop -- unboxed function -> table
TR opt: real-part-loop.rkt 30:6 loop -- unboxed let loop
TR opt: real-part-loop.rkt 30:13 v -- unboxed var -> table
TR opt: real-part-loop.rkt 30:15 0.0+1.0i -- unboxed literal
TR opt: real-part-loop.rkt 31:7 > -- binary float comp
TR opt: real-part-loop.rkt 31:9 (#%app real-part v) -- unboxed float complex->float
TR opt: real-part-loop.rkt 31:10 real-part -- unboxed float complex
TR opt: real-part-loop.rkt 31:10 real-part -- unboxed unary float complex
TR opt: real-part-loop.rkt 31:20 v -- leave var unboxed
TR opt: real-part-loop.rkt 31:20 v -- unbox float-complex
TR opt: real-part-loop.rkt 31:20 v -- unboxed complex variable
TR opt: real-part-loop.rkt 33:7 loop -- call to fun with unboxed args
TR opt: real-part-loop.rkt 33:7 loop -- unboxed call site
TR opt: real-part-loop.rkt 33:13 + -- unboxed binary float complex
TR opt: real-part-loop.rkt 33:15 v -- leave var unboxed
TR opt: real-part-loop.rkt 33:17 (quote 3.6) -- float-arg-expr in complex ops
0
)

#lang typed/racket/base
#:optimize



(ann
 (let loop ([v 0.0+1.0i])
  (if (> (real-part v) 70000.2) 
      0
      (loop (+ v 3.6))))
 Integer)
