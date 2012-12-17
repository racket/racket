#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide points vector-field error-bars)

(require/typed*
 plot
 
 [points ((Sequenceof (Sequenceof Real))
          [#:x-min (Option Real)]
          [#:x-max (Option Real)]
          [#:y-min (Option Real)]
          [#:y-max (Option Real)]
          [#:sym Point-Sym]
          [#:color Plot-Color]
          [#:fill-color (U Plot-Color 'auto)]
          [#:size Real]
          [#:line-width Real]
          [#:alpha Real]
          [#:label (Option String)]
          -> renderer2d)]
 
 [vector-field  (((Real Real -> (Sequenceof Real)))
                 ((Option Real)
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
                 ->* renderer2d)]
 
 [error-bars ((Sequenceof (Sequenceof Real))
              [#:x-min (Option Real)]
              [#:x-max (Option Real)]
              [#:y-min (Option Real)]
              [#:y-max (Option Real)]
              [#:color Color]
              [#:line-width Real]
              [#:line-style Plot-Pen-Style]
              [#:width Real]
              [#:alpha Real]
              -> renderer2d)]
 )
