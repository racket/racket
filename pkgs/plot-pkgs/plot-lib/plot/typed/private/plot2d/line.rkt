#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide lines parametric polar function inverse density)

(require/typed*
 plot/no-gui
 
 [lines ((Sequenceof (Sequenceof Real))
         [#:x-min (Option Real)]
         [#:x-max (Option Real)]
         [#:y-min (Option Real)]
         [#:y-max (Option Real)]
         [#:color Plot-Color]
         [#:width Real]
         [#:style Plot-Pen-Style]
         [#:alpha Real]
         [#:label (Option String)]
         -> renderer2d)]
 
 [parametric ((Real -> (Sequenceof Real))
              Real
              Real
              [#:x-min (Option Real)]
              [#:x-max (Option Real)]
              [#:y-min (Option Real)]
              [#:y-max (Option Real)]
              [#:samples Integer]
              [#:color Plot-Color]
              [#:width Real]
              [#:style Plot-Pen-Style]
              [#:alpha Real]
              [#:label (Option String)]
              -> renderer2d)]
 
 [polar (((Real -> Real))
         (Real
          Real
          [#:x-min (Option Real)]
          [#:x-max (Option Real)]
          [#:y-min   Real]
          [#:y-max   Real]
          [#:samples Integer]
          [#:color   Plot-Color]
          [#:width   Real]
          [#:style   Plot-Pen-Style]
          [#:alpha   Real]
          [#:label  (Option String)])
         ->* renderer2d)]
 
 [function (((Real -> Real))
            ((Option Real)
             (Option Real)
             [#:y-min (Option Real)]
             [#:y-max (Option Real)]
             [#:samples Integer]
             [#:color Plot-Color]
             [#:width Real]
             [#:style Plot-Pen-Style]
             [#:alpha Real]
             [#:label (Option String)])
            ->* renderer2d)]
 
 [inverse (((Real -> Real))
           ((Option Real)
            (Option Real)
            [#:x-min (Option Real)]
            [#:x-max (Option Real)]
            [#:samples Integer]
            [#:color Plot-Color]
            [#:width Real]
            [#:style Plot-Pen-Style]
            [#:alpha Real]
            [#:label (Option String)])
           ->* renderer2d)]
 
 [density (((Sequenceof Real))
           (Real
            (Option (Sequenceof Real))
            [#:x-min (Option Real)]
            [#:x-max (Option Real)]
            [#:y-min (Option Real)]
            [#:y-max (Option Real)]
            [#:samples Integer]
            [#:color Plot-Color]
            [#:width Real]
            [#:style Plot-Pen-Style]
            [#:alpha Real]
            [#:label (Option String)])
           ->* renderer2d)]
 )
