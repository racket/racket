#;
(
TR opt: real-part-loop.rkt 29:1 (let loop ((v 0.0+1.0i)) (if (> (real-part v) 70000.2) 0 (loop (+ v 3.6)))) -- unboxed call site
TR opt: real-part-loop.rkt 29:6 loop -- fun -> unboxed fun
TR opt: real-part-loop.rkt 29:6 loop -- unboxed function -> table
TR opt: real-part-loop.rkt 29:6 loop -- unboxed let loop
TR opt: real-part-loop.rkt 29:13 v -- unboxed var -> table
TR opt: real-part-loop.rkt 29:15 0.0+1.0i -- unboxed literal
TR opt: real-part-loop.rkt 30:6 (> (real-part v) 70000.2) -- binary float comp
TR opt: real-part-loop.rkt 30:9 (real-part v) -- complex accessor elimination
TR opt: real-part-loop.rkt 30:9 (real-part v) -- unboxed unary float complex
TR opt: real-part-loop.rkt 30:20 v -- leave var unboxed
TR opt: real-part-loop.rkt 30:20 v -- unbox float-complex
TR opt: real-part-loop.rkt 30:20 v -- unboxed complex variable
TR opt: real-part-loop.rkt 32:6 (loop (+ v 3.6)) -- call to fun with unboxed args
TR opt: real-part-loop.rkt 32:6 (loop (+ v 3.6)) -- unboxed call site
TR opt: real-part-loop.rkt 32:12 (+ v 3.6) -- unboxed binary float complex
TR opt: real-part-loop.rkt 32:15 v -- leave var unboxed
TR opt: real-part-loop.rkt 32:17 3.6 -- float-arg-expr in complex ops
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
