#lang scribble/manual

@(require "common.rkt")

@title[#:tag "todo"]{To Do}

@itemlist[
 @item{Planned new renderers
       @itemlist[
        @item{Functions with integer domains}
        @item{2D kernel density estimator}
        @item{3D kernel density estimator}
        @item{3D decorations: labeled points, axes, grids}
        @item{3D vector field}
        @item{2D and 3D stacked histograms}
        @item{2D grouped histograms}
       ]
 }
 @item{Possible minor enhancements
       @itemlist[
        @item{Better depth sorting (possibly split intersecting polygons; look into BSP tree)}
        @item{Legend entries have minimum sizes}
        @item{Label contour heights on the contour lines}
        @item{3D support for exact rational functions (i.e. polynomial at [big..big+ε])}
        @item{Join 2D contour lines}
        @item{Manually exclude discontinuous points from function renderers: allow values @(racket (hole p1 p2)), @(racket (left-hole p1 p2)), @(racket (right-hole p1 p2))}
        @item{@(racket histogram-list) to plot multiple histograms without manually calculating @(racket #:x-min)}
        ]
 }
]
