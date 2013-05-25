#lang typed/racket/base

(require/typed future-visualizer [visualize-futures-thunk (All (A) ((-> A) -> A))])

(define-syntax-rule (visualize-futures e ...)
  (visualize-futures-thunk (lambda () e ...)))

(provide visualize-futures-thunk visualize-futures)
