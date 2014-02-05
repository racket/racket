#;#;
#<<END
TR opt: real-part-loop.rkt 3:1 (let loop ((v 0.0+1.0i)) (if (> (real-part v) 70000.2) 0 (loop (+ v 3.6)))) -- unboxed call site
TR opt: real-part-loop.rkt 3:13 v -- unboxed var -> table
TR opt: real-part-loop.rkt 3:15 0.0+1.0i -- unboxed literal
TR opt: real-part-loop.rkt 3:6 loop -- fun -> unboxed fun
TR opt: real-part-loop.rkt 3:6 loop -- unboxed let loop
TR opt: real-part-loop.rkt 4:20 v -- leave var unboxed
TR opt: real-part-loop.rkt 4:6 (> (real-part v) 70000.2) -- binary float comp
TR opt: real-part-loop.rkt 4:9 (real-part v) -- complex accessor elimination
TR opt: real-part-loop.rkt 6:12 (+ v 3.6) -- unboxed binary float complex
TR opt: real-part-loop.rkt 6:15 v -- leave var unboxed
TR opt: real-part-loop.rkt 6:17 3.6 -- float in complex ops
TR opt: real-part-loop.rkt 6:6 (loop (+ v 3.6)) -- call to fun with unboxed args
TR opt: real-part-loop.rkt 6:6 (loop (+ v 3.6)) -- unboxed call site
END
#<<END
0

END
#lang typed/racket/base
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(ann
 (let loop ([v 0.0+1.0i])
  (if (> (real-part v) 70000.2)
      0
      (loop (+ v 3.6))))
 Integer)
