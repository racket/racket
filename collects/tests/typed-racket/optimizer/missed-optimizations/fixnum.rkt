#;
(
TR opt: fixnum.rkt 35:10 (* x y) -- fixnum bounded expr
TR missed opt: fixnum.rkt 38:0 (+ (ann z Fixnum) 234) -- out of fixnum range
TR missed opt: fixnum.rkt 39:0 (* (ann x Index) (ann y Index)) -- out of fixnum range
TR missed opt: fixnum.rkt 40:0 (fx* (ann x Index) (ann y Index)) -- out of fixnum range
TR missed opt: fixnum.rkt 41:0 (abs (ann -3 Fixnum)) -- out of fixnum range
TR opt: fixnum.rkt 42:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 42:15 (+ 301 302) -- fixnum bounded expr
TR missed opt: fixnum.rkt 42:0 (+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR opt: fixnum.rkt 42:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 42:15 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 42:3 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 42:15 (+ 301 302) -- fixnum bounded expr
TR opt: fixnum.rkt 43:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 43:17 (+ 301 302) -- fixnum bounded expr
TR missed opt: fixnum.rkt 43:0 (fx+ (+ 300 301) (+ 301 302)) -- out of fixnum range
TR opt: fixnum.rkt 43:5 (+ 300 301) -- fixnum bounded expr
TR opt: fixnum.rkt 43:17 (+ 301 302) -- fixnum bounded expr
TR missed opt: fixnum.rkt 44:0 (fxquotient -4 -5) -- out of fixnum range
468
234
234
3
1204
1204
0
)
#lang typed/racket

(require racket/fixnum)

(define x 3)
(define y 78)
(define z (* x y)) ; this should be optimized

;; this should not, (+ Fixnum Byte), but it may look like it should
(+ (ann z Fixnum) 234)
(* (ann x Index) (ann y Index))
(fx* (ann x Index) (ann y Index))
(abs (ann -3 Fixnum))
(+ (+ 300 301) (+ 301 302))
(fx+ (+ 300 301) (+ 301 302))
(fxquotient -4 -5)
