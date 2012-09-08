#lang racket/gui

(provide showcase)

(define showcase (lambda (#:hmult (hmult 1) . name)
                   (define frame (new frame% (label (if (null? name)
                                                        "widget"
                                                        (car name)))))
                   (define panel (new panel% (parent frame)))
                   (define showcase-panel (new horizontal-panel%
                                               (parent panel)
                                               (alignment '(center center))
                                               (style '(border))
                                               (min-width 242)
                                               (min-height (* hmult 75))
                                               (border 20)))
                   (send frame show #t)
                   showcase-panel))
