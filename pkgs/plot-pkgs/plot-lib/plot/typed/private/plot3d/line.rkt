#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide lines3d parametric3d)

(require/typed*
 plot/no-gui
 
 [lines3d ((Sequenceof (Sequenceof Real))
           [#:x-min (Option Real)]
           [#:x-max (Option Real)]
           [#:y-min (Option Real)]
           [#:y-max (Option Real)]
           [#:z-min (Option Real)]
           [#:z-max (Option Real)]
           [#:color Plot-Color]
           [#:width Real]
           [#:style Plot-Pen-Style]
           [#:alpha Real]
           [#:label (Option String)]
           -> renderer3d)]
 
 [parametric3d ((Real -> (Sequenceof Real))
                Real
                Real
                [#:x-min (Option Real)]
                [#:x-max (Option Real)]
                [#:y-min (Option Real)]
                [#:y-max (Option Real)] 
                [#:z-min (Option Real)]
                [#:z-max (Option Real)] 
                [#:samples Integer]
                [#:color Plot-Color]
                [#:width Real]
                [#:style Plot-Pen-Style]
                [#:alpha Real]
                [#:label (Option String)]
                -> renderer3d)]
 )
