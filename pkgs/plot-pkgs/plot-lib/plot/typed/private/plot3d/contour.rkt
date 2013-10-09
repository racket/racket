#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide isoline3d contours3d contour-intervals3d)

(require/typed*
 plot/no-gui
 
 [isoline3d (((Real Real -> Real) Real)
             ((Option Real)
              (Option Real)
              (Option Real)
              (Option Real)
              [#:z-min (Option Real)]
              [#:z-max (Option Real)]
              [#:samples Integer]
              [#:color Plot-Color]     
              [#:width Real]
              [#:style Plot-Pen-Style]
              [#:alpha Real]
              [#:label (Option String)])
             ->* renderer3d)]
 
 [contours3d (((Real Real -> Real))
              ((Option Real)
               (Option Real)
               (Option Real)
               (Option Real)
               [#:z-min (Option Real)]
               [#:z-max (Option Real)]
               [#:samples Integer]
               [#:levels Contour-Levels]
               [#:colors (Plot-Colors (Listof Real))]
               [#:widths (Pen-Widths (Listof Real))]
               [#:styles (Plot-Pen-Styles (Listof Real))]
               [#:alphas (Alphas (Listof Real))]
               [#:label (Option String)])
              ->* renderer3d)]
 
 [contour-intervals3d (((Real Real -> Real))
                       ((Option Real)
                        (Option Real)
                        (Option Real)
                        (Option Real)
                        [#:z-min (Option Real)]
                        [#:z-max (Option Real)]
                        [#:samples Integer]
                        [#:levels Contour-Levels]
                        [#:colors (Plot-Colors (Listof ivl))]  
                        [#:styles (Plot-Brush-Styles (Listof ivl))]
                        [#:line-colors (Plot-Colors (Listof ivl))]
                        [#:line-widths (Pen-Widths (Listof ivl))]
                        [#:line-styles (Plot-Pen-Styles (Listof ivl))]
                        [#:contour-colors (Plot-Colors (Listof Real))]
                        [#:contour-widths (Plot-Colors (Listof Real))]
                        [#:contour-styles (Plot-Pen-Styles (Listof Real))]
                        [#:alphas (Alphas (Listof ivl))]
                        [#:label (Option String)])
                       ->* renderer3d)]
 )
