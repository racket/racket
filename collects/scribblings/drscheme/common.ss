#lang scheme/base

(require scribble/manual
         (for-label scheme))

(provide HtDP
         drlang
         (all-from-out scribble/manual)
         (for-label (all-from-out scheme)))

(define HtDP
  (italic "How to Design Programs"))

(define (drlang . s)
  (apply onscreen s))
