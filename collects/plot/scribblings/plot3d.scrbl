#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "plot3d"]{3D Plot Procedures}

Each 3D plot procedure corresponds with a @(secref "plot2d") procedure. Each behaves the same way as its corresponding 2D procedure, but takes the additional keyword arguments @(racket #:z-min), @(racket #:z-max), @(racket #:angle), @(racket #:altitude) and @(racket #:z-label).

@defproc[(plot3d [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                 [#:width width exact-positive-integer? (plot-width)]
                 [#:height height exact-positive-integer? (plot-height)]
                 [#:angle angle real? (plot3d-angle)]
                 [#:altitude altitude real? (plot3d-altitude)]
                 [#:title title (or/c string? #f) (plot-title)]
                 [#:x-label x-label (or/c string? #f) (plot-x-label)]
                 [#:y-label y-label (or/c string? #f) (plot-y-label)]
                 [#:z-label z-label (or/c string? #f) (plot-z-label)]
                 [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                 [#:out-file out-file (or/c path-string? output-port? #f) #f]
                 [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                 ) (or/c (is-a?/c image-snip%) void?)]{
This procedure corresponds with @(racket plot). It plots a 3D renderer or list of renderers (or more generally, a tree of renderers), as returned by @(racket points3d), @(racket parametric3d), @(racket surface3d), @(racket isosurface3d), and others.

When the parameter @(racket plot-new-window?) is @(racket #t), @(racket plot3d) opens a new window to display the plot and returns @(racket (void)).

When @(racket #:out-file) is given, @(racket plot3d) writes the plot to a file using @(racket plot3d-file) as well as returning a @(racket image-snip%) or opening a new window.

When given, the @(racket x-min), @(racket x-max), @(racket y-min), @(racket y-max), @(racket z-min) and @(racket z-max) arguments determine the bounds of the plot, but not the bounds of the renderers.

@bold{Deprecated keywords.} The @(racket #:fgcolor) and @(racket #:bgcolor) keyword arguments are currently supported for backward compatibility, but may not be in the future.
Please set the @(racket plot-foreground) and @(racket plot-background) parameters instead of using these keyword arguments.
The @(racket #:lncolor) keyword argument is also accepted for backward compatibility but deprecated. It does nothing.

The @(racket #:az) and @(racket #:alt) keyword arguments are backward-compatible, deprecated aliases for @(racket #:angle) and @(racket #:altitude), respectively.
}

@deftogether[
(@defproc[(plot3d-file [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                       [output (or/c path-string? output-port?)]
                       [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                       [#:<plot-keyword> <plot-keyword> <plot-keyword-contract>] ...) void?]
 @defproc[(plot3d-pict [renderer-tree (treeof (or/c renderer3d? nonrenderer?))] ...) pict?]
 @defproc[(plot3d-bitmap [renderer-tree (treeof (or/c renderer3d? nonrenderer?))] ...) (is-a?/c bitmap%)]
 @defproc[(plot3d-snip [renderer-tree (treeof (or/c renderer3d? nonrenderer?))] ...) (is-a?/c image-snip%)]
 @defproc[(plot3d-frame [renderer-tree (treeof (or/c renderer3d? nonrenderer?))] ...) (is-a?/c frame%)])]{
Plot to different backends. Each of these procedures has the same keyword arguments as @(racket plot3d), except for deprecated keywords.

These procedures correspond with  @(racket plot-file), @(racket plot-pict), @(racket plot-bitmap), @(racket plot-snip) and @(racket plot-frame).
}

@doc-apply[plot3d/dc]{
Plots to an arbitrary device context, in the rectangle with width @(racket width), height @(racket height), and upper-left corner @(racket x),@(racket y).

Every @secref{plot3d} procedure is defined in terms of @(racket plot3d/dc).

Use this if you need to continually update a plot on a @(racket canvas%), or to create other @(racket plot3d)-like functions with different backends.

This procedure corresponds with @(racket plot/dc). 
}
