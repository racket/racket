#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide lines-interval parametric-interval polar-interval function-interval inverse-interval)

(require/typed*
 plot
 
 [lines-interval ((Sequenceof (Sequenceof Real))
                  (Sequenceof (Sequenceof Real))
                  [#:x-min (Option Real)]
                  [#:x-max (Option Real)]
                  [#:y-min (Option Real)]
                  [#:y-max (Option Real)]
                  [#:color Plot-Color]
                  [#:style Plot-Brush-Style]
                  [#:line1-color Plot-Color]
                  [#:line1-width Real]
                  [#:line1-style Plot-Pen-Style]
                  [#:line2-color Plot-Color]
                  [#:line2-width Real]
                  [#:line2-style Plot-Pen-Style]
                  [#:alpha Real]
                  [#:label (Option String)]
                  -> renderer2d)]
 
 [parametric-interval ((Real -> (Sequenceof Real))
                       (Real -> (Sequenceof Real))
                       Real
                       Real
                       [#:x-min (Option Real)]
                       [#:x-max (Option Real)]
                       [#:y-min (Option Real)]
                       [#:y-max (Option Real)]
                       [#:samples Integer]
                       [#:color Plot-Color]
                       [#:style Plot-Brush-Style]
                       [#:line1-color Plot-Color]
                       [#:line1-width Real]
                       [#:line1-style Plot-Pen-Style]
                       [#:line2-color Plot-Color]
                       [#:line2-width Real]
                       [#:line2-style Plot-Pen-Style]
                       [#:alpha Real]
                       [#:label (Option String)]
                       -> renderer2d)]
 
 [polar-interval (((Real -> Real)
                   (Real -> Real))
                  (Real
                   Real
                   [#:x-min (Option Real)]
                   [#:x-max (Option Real)]
                   [#:y-min (Option Real)]
                   [#:y-max (Option Real)]
                   [#:samples Integer]
                   [#:color Plot-Color]
                   [#:style Plot-Brush-Style]
                   [#:line1-color Plot-Color]
                   [#:line1-width Real]
                   [#:line1-style Plot-Pen-Style]
                   [#:line2-color Plot-Color]
                   [#:line2-width Real]
                   [#:line2-style Plot-Pen-Style]
                   [#:alpha Real]
                   [#:label (Option String)])
                  ->* renderer2d)]
 
 [function-interval (((Real -> Real)
                      (Real -> Real))
                     ((Option Real)
                      (Option Real)
                      [#:y-min (Option Real)]
                      [#:y-max (Option Real)]
                      [#:samples Integer]
                      [#:color Plot-Color]
                      [#:style Plot-Brush-Style]
                      [#:line1-color Plot-Color]
                      [#:line1-width Real]
                      [#:line1-style Plot-Pen-Style]
                      [#:line2-color Plot-Color]
                      [#:line2-width Real]
                      [#:line2-style Plot-Pen-Style]
                      [#:alpha Real]
                      [#:label (Option String)])
                     ->* renderer2d)]
 
 [inverse-interval (((Real -> Real)
                     (Real -> Real))
                    ((Option Real)
                     (Option Real)
                     [#:x-min (Option Real)]
                     [#:x-max (Option Real)]
                     [#:samples Integer]
                     [#:color Plot-Color]
                     [#:style Plot-Brush-Style]
                     [#:line1-color Plot-Color]
                     [#:line1-width Real]
                     [#:line1-style Plot-Pen-Style]
                     [#:line2-color Plot-Color]
                     [#:line2-width Real]
                     [#:line2-style Plot-Pen-Style]
                     [#:alpha Real]
                     [#:label (Option String)])
                    ->* renderer2d)]
 )
