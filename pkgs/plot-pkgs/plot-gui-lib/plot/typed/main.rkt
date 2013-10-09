#lang typed/racket/base

(require plot/typed/no-gui
         "private/gui/plot2d.rkt"
         "private/gui/plot3d.rkt")

(provide (all-from-out
          plot/typed/no-gui
          "private/gui/plot2d.rkt"
          "private/gui/plot3d.rkt"))
