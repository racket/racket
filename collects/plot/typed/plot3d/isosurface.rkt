#lang typed/racket/base

(require "../common/types.rkt"
         "../syntax.rkt")

(provide isosurface3d isosurfaces3d polar3d)

(require/typed*
 plot
 
 [isosurface3d (((Real Real Real -> Real) Real)
                ((Option Real)
                 (Option Real)
                 (Option Real)
                 (Option Real)
                 (Option Real)
                 (Option Real)
                 [#:samples Integer]
                 [#:color Plot-Color]  
                 [#:style Plot-Brush-Style]
                 [#:line-color Plot-Color]
                 [#:line-width Real]
                 [#:line-style Plot-Pen-Style]
                 [#:alpha Real]
                 [#:label (Option String)])
                ->* renderer3d)]
 
 [isosurfaces3d (((Real Real Real -> Real))
                 ((Option Real)
                  (Option Real)
                  (Option Real)
                  (Option Real)
                  (Option Real)
                  (Option Real)
                  [#:d-min (Option Real)]
                  [#:d-max (Option Real)]
                  [#:samples Integer]
                  [#:levels Contour-Levels]
                  [#:colors (Plot-Colors (Listof Real))]
                  [#:styles (Plot-Brush-Styles (Listof Real))]
                  [#:line-colors (Plot-Colors (Listof Real))]
                  [#:line-widths (Pen-Widths (Listof Real))]
                  [#:line-styles (Plot-Pen-Styles (Listof Real))]
                  [#:alphas (Alphas (Listof Real))]
                  [#:label (Option String)])
                 ->* renderer3d)]
 
 [polar3d ((Real Real -> Real)
           [#:x-min (Option Real)]
           [#:x-max (Option Real)]
           [#:y-min (Option Real)]
           [#:y-max (Option Real)]
           [#:z-min (Option Real)]
           [#:z-max (Option Real)]
           [#:samples Integer]
           [#:color Plot-Color]     
           [#:style Plot-Brush-Style]
           [#:line-color Plot-Color]
           [#:line-width Real]
           [#:line-style Plot-Pen-Style]
           [#:alpha Real]
           [#:label (Option String)]
           -> renderer3d)]
 )
