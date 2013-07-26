#lang racket/base
(require drracket/tool racket/unit framework "stamp.rkt")

(provide tool@)
(define tool@
  (unit (import drscheme:tool^) (export drscheme:tool-exports^)
    (define (phase1) (void))
    (define (phase2) (void))
    (when stamp
      (version:add-spec '-- stamp))))
