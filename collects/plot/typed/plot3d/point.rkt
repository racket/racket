#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide points3d vector-field3d)

(require/typed*
 plot
 
 [points3d ((Sequenceof (Sequenceof Real))
            [#:x-min (Option Real)]
            [#:x-max (Option Real)]
            [#:y-min (Option Real)]
            [#:y-max (Option Real)] 
            [#:z-min (Option Real)]
            [#:z-max (Option Real)] 
            [#:sym Point-Sym]
            [#:color Plot-Color]
            [#:fill-color (U Plot-Color 'auto)]
            [#:size Real]
            [#:line-width Real]
            [#:alpha Real]
            [#:label (Option String)]
            -> renderer3d)]
 
 [vector-field3d (((U (Real Real Real -> (Sequenceof Real))
                      ((Vector Real Real Real) -> (Sequenceof Real))))
                  ((Option Real)
                   (Option Real)
                   (Option Real)
                   (Option Real) 
                   (Option Real)
                   (Option Real) 
                   [#:samples Integer]
                   [#:scale (U Real 'auto 'normalized)]
                   [#:color Plot-Color]
                   [#:line-width Real]
                   [#:line-style Plot-Pen-Style]
                   [#:alpha Real]
                   [#:label (Option String)])
                  ->* renderer3d)]
 )
