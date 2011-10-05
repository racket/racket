#lang scribble/manual

@(require "common.rkt")

@title[#:tag "todo"]{To Do}

@itemlist[
 @item{Planned new renderers
       @itemlist[
        @item{2D kernel density estimator}
        @item{3D kernel density estimator}
        @item{2D implicit curve}
        @item{3D implicit surface}
        @item{3D decorations: labeled points, axes, grids}
       ]
 }
 @item{Possible new renderers
       @itemlist[
        @item{R × R -> R parametric (turn into 3D implicit surface by solving for minimum distance?)}
        @item{3D vector field}
        @item{Head-to-tail vector fields}
       ]
 }
 @item{Minor fixes
       @itemlist[
        @item{Subdivide nonlinearly transformed 3D lines/polygons (port from @(racket 2d-plot-area%))}
       ]
 }
 @item{Minor enhancements
       @itemlist[
        @item{Better depth sorting (possibly split intersecting polygons; look into BSP tree)}
        @item{Legend entries have minimum sizes}
        @item{Log-scale tick functions (i.e. major ticks are 10^0, 10^1, 10^2, ...)}
        @item{Label contour heights on the contour lines}
        @item{3D support for exact rational functions (i.e. polynomial at [big..big+ε])}
        @item{Join 2D contour lines}
        @item{More appearance options (i.e. draw 2D tick labels on right/top)}
        @item{Manually exclude discontinuous points from function renderers: allow values @(racket (hole p1 p2)), @(racket (left-hole p1 p2)), @(racket (right-hole p1 p2))}
        @item{@(racket histogram-list) to plot multiple histograms without manually calculating @(racket #:x-min)}
        ]
 }
]
