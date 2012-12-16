#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide rectangles area-histogram discrete-histogram stacked-histogram)

(require/typed*
 plot
 
 [rectangles ((Sequenceof (Sequenceof ivl))
              [#:x-min (Option Real)]
              [#:x-max (Option Real)]
              [#:y-min (Option Real)]
              [#:y-max (Option Real)]
              [#:color Plot-Color]
              [#:style Plot-Brush-Style]
              [#:line-color Plot-Color]
              [#:line-width Real]
              [#:line-style Plot-Pen-Style]
              [#:alpha Real]
              [#:label (Option String)]
              -> renderer2d)]
 
 [area-histogram ((Real -> Real) 
                  (Sequenceof Real)
                  [#:x-min (Option Real)]
                  [#:x-max (Option Real)]
                  [#:y-min (Option Real)]
                  [#:y-max (Option Real)]
                  [#:samples Integer]                   
                  [#:color Plot-Color]
                  [#:style Plot-Brush-Style]
                  [#:line-color Plot-Color]
                  [#:line-width Real]
                  [#:line-style Plot-Pen-Style]
                  [#:alpha Real]
                  [#:label (Option String)]
                  -> renderer2d)]
 
 [discrete-histogram ((Sequenceof (U (Vector Any (U Real ivl #f))
                                     (List Any (U Real ivl #f))))
                      [#:x-min (Option Real)]
                      [#:x-max (Option Real)]
                      [#:y-min (Option Real)]
                      [#:y-max (Option Real)]
                      [#:gap Real]
                      [#:skip Real]
                      [#:invert? Boolean]
                      [#:color Plot-Color]
                      [#:style Plot-Brush-Style]
                      [#:line-color Plot-Color]
                      [#:line-width Real]
                      [#:line-style Plot-Pen-Style]
                      [#:alpha Real]
                      [#:label (Option String)]
                      [#:add-ticks? Boolean]
                      [#:far-ticks? Boolean]
                      -> renderer2d)]
 
 [stacked-histogram ((Sequenceof (U (Vector Any (Sequenceof Real))
                                    (List Any (Sequenceof Real))))
                     [#:x-min (Option Real)]
                     [#:x-max (Option Real)]
                     [#:y-min (Option Real)]
                     [#:y-max (Option Real)]
                     [#:gap Real]
                     [#:skip Real]
                     [#:invert? Boolean]
                     [#:colors (Plot-Colors Natural)]
                     [#:styles (Plot-Brush-Styles Natural)]
                     [#:line-colors (Plot-Colors Natural)]
                     [#:line-widths (Pen-Widths Natural)]
                     [#:line-styles (Plot-Pen-Styles Natural)]
                     [#:alphas (Alphas Natural)]
                     [#:labels (Labels Natural)]
                     [#:add-ticks? Boolean]
                     [#:far-ticks? Boolean]
                     -> (Listof renderer2d))]
 )
