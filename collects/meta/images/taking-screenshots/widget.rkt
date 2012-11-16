#lang racket/gui

(provide showcase)

(define (showcase [name "widget"] #:hmult (hmult 1))
  (define frame (new frame% [label name]))
  (define panel (new panel% [parent frame]))
  (define showcase-panel
    (new horizontal-panel%
         [parent panel] [alignment '(center center)]
         [min-width 242] [min-height (* hmult 75)]
         [style '(border)] [border 20]))
  (send frame show #t)
  showcase-panel)
