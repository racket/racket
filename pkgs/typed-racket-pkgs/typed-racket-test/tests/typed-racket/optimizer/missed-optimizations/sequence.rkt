#;#;
#<<END
TR missed opt: sequence.rkt 10:22 s -- non-specialized for clause
END
""
#lang typed/racket/base

(: seq-generic : (Sequenceof Integer) -> Void)
(define (seq-generic s)
  (for: ([x : Integer s])
    (void)))
