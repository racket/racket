#;
(exn-pred exn:fail:syntax?)
#lang typed/racket
;; singleton types for values who runtime type depends is platform-dependent are unsafe
;; PR13501
(define: z : 10000000000000 10000000000000)

(ann (if (and #t (fixnum? z)) z 0) (U 0 1))
