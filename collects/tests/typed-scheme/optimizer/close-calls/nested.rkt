#;
(
nested.rkt 15:0 (#%app * (quote 2.0) (#%app * (quote 3.0) (quote 4))) -- binary, args all float-arg-expr, return type not Float -- caused by: 15:19 (quote 4)
nested.rkt 19:0 (#%app * (#%app * (quote 3.0) (quote 4)) (#%app * (quote 3.0) (quote 4))) -- binary, args all float-arg-expr, return type not Float -- caused by: 19:15 (quote 4) 19:39 (quote 4)
24.0
144.0
 )

#lang typed/racket

;; when a single "bubble" causes missed optimizations to cascade, a single
;; close call should be reported, with the outermost expression being reported
;; but with the innermost flagged as the cause
;; TODO also report number of missed opts, etc.

(* 2.0 (* 3.0 (ann 4 Integer)))

;; here, multiple subexpressions are to blame, but a single close call with
;; two causes should be reported
(* (* 3.0 (ann 4 Integer)) (* 3.0 (ann 4 Integer)))
