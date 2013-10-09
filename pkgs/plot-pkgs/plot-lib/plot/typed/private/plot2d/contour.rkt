#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide isoline contours contour-intervals)

(require/typed*
 plot/no-gui
 
 [isoline (((Real Real -> Real) Real)
           ((Option Real)
            (Option Real)
            (Option Real)
            (Option Real)   
            [#:samples Integer]
            [#:color Plot-Color]
            [#:width Real]
            [#:style Plot-Pen-Style]
            [#:alpha Real]
            [#:label (Option String)])
           ->* renderer2d)]
 
 [contours (((Real Real -> Real))
            ((Option Real)
             (Option Real)
             (Option Real)
             (Option Real)
             [#:samples Integer]
             [#:levels Contour-Levels]
             [#:colors (Plot-Colors (Listof Real))]
             [#:widths (Pen-Widths (Listof Real))]
             [#:styles (Plot-Pen-Styles (Listof Real))]
             [#:alphas (Alphas (Listof Real))]
             [#:label (Option String)])
            ->* renderer2d)]
 
 [contour-intervals (((Real Real -> Real))
                     ((Option Real)
                      (Option Real)
                      (Option Real)
                      (Option Real)
                      [#:samples Integer]
                      [#:levels Contour-Levels]
                      [#:colors (Plot-Colors (Listof ivl))]
                      [#:styles (Plot-Brush-Styles (Listof ivl))]
                      [#:contour-colors (Plot-Colors (Listof Real))]
                      [#:contour-widths (Pen-Widths (Listof Real))]
                      [#:contour-styles (Plot-Pen-Styles (Listof Real))]
                      [#:alphas (Alphas (Listof ivl))]
                      [#:label (Option String)])
                     ->* renderer2d)]
 )
