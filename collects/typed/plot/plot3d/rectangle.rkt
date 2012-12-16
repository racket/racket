#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide rectangles3d discrete-histogram3d stacked-histogram3d)

(require/typed*
 plot
 
 [rectangles3d ((Sequenceof (Sequenceof ivl))
                [#:x-min (Option Real)]
                [#:x-max (Option Real)]
                [#:y-min (Option Real)]
                [#:y-max (Option Real)]
                [#:z-min (Option Real)]
                [#:z-max (Option Real)]
                [#:color Plot-Color]
                [#:style Plot-Brush-Style]
                [#:line-color Plot-Color]
                [#:line-width Real]
                [#:line-style Plot-Pen-Style]
                [#:alpha Real] 
                [#:label (Option String)]
                -> renderer3d)]
 
 [discrete-histogram3d ((Sequenceof (U (Vector Any Any (Option (U Real ivl)))
                                       (List Any Any (Option (U Real ivl)))))
                        [#:x-min (Option Real)]
                        [#:x-max (Option Real)]
                        [#:y-min (Option Real)]
                        [#:y-max (Option Real)]
                        [#:z-min (Option Real)]
                        [#:z-max (Option Real)]
                        [#:gap Real]
                        [#:color Plot-Color]
                        [#:style Plot-Brush-Style]
                        [#:line-color Plot-Color]
                        [#:line-width Real]
                        [#:line-style Plot-Pen-Style]
                        [#:alpha Real]
                        [#:label (Option String)]
                        [#:add-x-ticks? Boolean]
                        [#:add-y-ticks? Boolean]
                        [#:x-far-ticks? Boolean]
                        [#:y-far-ticks? Boolean]
                        -> renderer3d)]
 
 [stacked-histogram3d ((Sequenceof (U (Vector Any Any (Sequenceof Real))
                                      (List Any Any (Sequenceof Real))))
                       [#:x-min (Option Real)]
                       [#:x-max (Option Real)]
                       [#:y-min (Option Real)]
                       [#:y-max (Option Real)]
                       [#:z-min (Option Real)]
                       [#:z-max (Option Real)]
                       [#:gap Real]
                       [#:colors (Plot-Colors Natural)]
                       [#:styles (Plot-Brush-Styles Natural)]
                       [#:line-colors (Plot-Colors Natural)]
                       [#:line-widths (Pen-Widths Natural)]
                       [#:line-styles (Plot-Pen-Styles Natural)]
                       [#:alphas (Alphas Natural)]
                       [#:labels (Labels Natural)]
                       [#:add-x-ticks? Boolean]
                       [#:add-y-ticks? Boolean]
                       [#:x-far-ticks? Boolean]
                       [#:y-far-ticks? Boolean]
                       -> (Listof renderer3d))]
 )
