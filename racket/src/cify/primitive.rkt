#lang racket/base
(require (only-in '#%linklet primitive-in-category?))

(provide direct-call-primitive?
         immediate-primitive?)

(define (direct-call-primitive? rator prim-knowns)
  (primitive-in-category? rator 'noncm))

(define (immediate-primitive? rator prim-knowns)
  (primitive-in-category? rator 'immediate))
