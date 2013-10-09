#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide surface3d)

(require/typed*
 plot/no-gui
 
 [surface3d (((Real Real -> Real))
             ((Option Real)
              (Option Real)
              (Option Real)
              (Option Real)
              [#:z-min (Option Real)]
              [#:z-max (Option Real)]
              [#:samples Integer]
              [#:color Plot-Color]     
              [#:style Plot-Brush-Style]
              [#:line-color Plot-Color]
              [#:line-width Real]
              [#:line-style Plot-Pen-Style]
              [#:alpha Real]
              [#:label (Option String)])
             ->* renderer3d)]
 )
