#lang scribble/manual

@(require (for-label slideshow
                     racket/gui/dynamic)
          "common.rkt")

@declare-exporting[plot]

@title[#:tag "plotting"]{2D and 3D Plotting Procedures}

The plotting procedures exported by @racketmodname[plot/no-gui] produce @racket[bitmap%] and @racket[pict] instances, and write to files.
They do not require @racketmodname[racket/gui], so they work in headless environments; for example, a Linux terminal with @tt{DISPLAY} unset.

The @racketmodname[plot] module re-exports everything exported by @racketmodname[plot/no-gui], as well as @racket[plot], @racket[plot3d], and other procedures that create interactive plots and plot frames.
Interactive plotting procedures can always be imported, but fail when called if there is no working display or @racketmodname[racket/gui] is not present.

Each 3D plotting procedure behaves the same way as its corresponding 2D procedure, but takes the additional keyword arguments @(racket #:z-min), @(racket #:z-max), @(racket #:angle), @(racket #:altitude) and @(racket #:z-label).

@section{GUI Plotting Procedures}

@defmodule*/no-declare[(plot plot/typed) #:link-target? #f]

@defproc[(plot [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
               [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
               [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
               [#:width width exact-positive-integer? (plot-width)]
               [#:height height exact-positive-integer? (plot-height)]
               [#:title title (or/c string? #f) (plot-title)]
               [#:x-label x-label (or/c string? #f) (plot-x-label)]
               [#:y-label y-label (or/c string? #f) (plot-y-label)]
               [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
               [#:out-file out-file (or/c path-string? output-port? #f) #f]
               [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
               ) (or/c (is-a?/c image-snip%) void?)]{
Plots a 2D renderer or list of renderers (or more generally, a tree of renderers), as returned by @(racket points), @(racket function), @(racket contours), @(racket discrete-histogram), and others.

By default, @(racket plot) produces a Racket value that is displayed as an image and can be manipulated like any other value.
For example, they may be put in lists:

@interaction[#:eval plot-eval
                    (parameterize ([plot-width    150]
                                   [plot-height   150]
                                   [plot-x-label  #f]
                                   [plot-y-label  #f])
                      (list (plot (function sin (- pi) pi))
                            (plot (function sqr -2 2))))]

When the parameter @(racket plot-new-window?) is @(racket #t), @(racket plot) opens a new window to display the plot and returns @(racket (void)).

When @(racket #:out-file) is given, @(racket plot) writes the plot to a file using @(racket plot-file) as well as returning an @(racket image-snip%) or opening a new window.

When given, the @(racket x-min), @(racket x-max), @(racket y-min) and @(racket y-max) arguments determine the bounds of the plot, but not the bounds of the renderers. For example,

@interaction[#:eval plot-eval
(plot (function (λ (x) (sin (* 4 x))) -1 1)
      #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5)]

Here, the renderer draws in [-1,1] × [-1,1], but the plot area is [-1.5,1.5] × [-1.5,1.5].

@bold{Deprecated keywords.} The @(racket #:fgcolor) and @(racket #:bgcolor) keyword arguments are currently supported for backward compatibility, but may not be in the future.
Please set the @(racket plot-foreground) and @(racket plot-background) parameters instead of using these keyword arguments.
The @(racket #:lncolor) keyword argument is also accepted for backward compatibility but deprecated. It does nothing.
}

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
Plots a 3D renderer or list of renderers (or more generally, a tree of renderers), as returned by @(racket points3d), @(racket parametric3d), @(racket surface3d), @(racket isosurface3d), and others.

When the parameter @(racket plot-new-window?) is @(racket #t), @(racket plot3d) opens a new window to display the plot and returns @(racket (void)).

When @(racket #:out-file) is given, @(racket plot3d) writes the plot to a file using @(racket plot3d-file) as well as returning a @(racket image-snip%) or opening a new window.

When given, the @(racket x-min), @(racket x-max), @(racket y-min), @(racket y-max), @(racket z-min) and @(racket z-max) arguments determine the bounds of the plot, but not the bounds of the renderers.

@bold{Deprecated keywords.} The @(racket #:fgcolor) and @(racket #:bgcolor) keyword arguments are currently supported for backward compatibility, but may not be in the future.
Please set the @(racket plot-foreground) and @(racket plot-background) parameters instead of using these keyword arguments.
The @(racket #:lncolor) keyword argument is also accepted for backward compatibility but deprecated. It does nothing.

The @(racket #:az) and @(racket #:alt) keyword arguments are backward-compatible, deprecated aliases for @(racket #:angle) and @(racket #:altitude), respectively.
}

@defproc[(plot-snip [<plot-argument> <plot-argument-contract>] ...)
         (is-a?/c image-snip%)]
@defproc[(plot3d-snip [<plot-argument> <plot-argument-contract>] ...)
         (is-a?/c image-snip%)]
@defproc[(plot-frame [<plot-argument> <plot-argument-contract>] ...)
         (is-a?/c frame%)]
@defproc[(plot3d-frame [<plot-argument> <plot-argument-contract>] ...)
         (is-a?/c frame%)]{
Plot to different GUI backends.
These procedures accept the same arguments as @(racket plot) and @(racket plot3d), except deprecated keywords, and @racket[#:out-file] and @racket[#:out-kind].

Use @(racket plot-frame) and @(racket plot3d-frame) to create a @(racket frame%) regardless of the value of @(racket plot-new-window?). The frame is initially hidden.

Use @(racket plot-snip) and @(racket plot3d-snip) to create an interactive @(racket image-snip%) regardless of the value of @(racket plot-new-window?).
}

@section{Non-GUI Plotting Procedures}

@defmodule*/no-declare[(plot/no-gui plot/typed/no-gui)]

@defproc[(plot-file [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [output (or/c path-string? output-port?)]
                    [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                    [#:<plot-keyword> <plot-keyword> <plot-keyword-contract>] ...)
         void?]
@defproc[(plot3d-file [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                      [output (or/c path-string? output-port?)]
                      [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                      [#:<plot3d-keyword> <plot3d-keyword> <plot3d-keyword-contract>] ...)
         void?]
@defproc[(plot-pict [<plot-argument> <plot-argument-contract>] ...)
         pict?]
@defproc[(plot3d-pict [<plot3d-argument> <plot3d-argument-contract>] ...)
         pict?]
@defproc[(plot-bitmap [<plot-argument> <plot-argument-contract>] ...)
         (is-a?/c bitmap%)]
@defproc[(plot3d-bitmap [<plot3d-argument> <plot3d-argument-contract>] ...)
         (is-a?/c bitmap%)]{
Plot to different non-GUI backends.
These procedures accept the same arguments as @(racket plot) and @(racket plot3d), except deprecated keywords, and @racket[#:out-file] and @racket[#:out-kind].

Use @(racket plot-file) or @(racket plot3d-file) to save a plot to a file.
When creating a JPEG file, the parameter @(racket plot-jpeg-quality) determines its quality.
When creating a PostScript or PDF file, the parameter @(racket plot-ps/pdf-interactive?) determines whether the user is given a dialog to set printing parameters.
(See @(racket post-script-dc%) and @(racket pdf-dc%).)
When @(racket kind) is @(racket 'auto), @(racket plot-file) and @(racket plot3d-file) try to determine from the file name extension the kind of file to write.

Use @(racket plot-pict) or @(racket plot3d-pict) to create a @(racket pict).
For example, this program creates a slide containing a 2D plot of a parabola:
@racketmod[slideshow
(require plot)

(plot-font-size (current-font-size))
(plot-width (current-para-width))
(plot-height 600)
(plot-background-alpha 1/2)

(slide
 #:title "A 2D Parabola"
 (plot-pict (function sqr -1 1 #:label "y = x^2")))]

Use @(racket plot-bitmap) or @(racket plot3d-bitmap) to create a @(racket bitmap%).
}

@defproc[(plot/dc [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                  [dc (is-a?/c dc<%>)]
                  [x real?]
                  [y real?]
                  [width (>=/c 0)]
                  [height (>=/c 0)]
                  [#:<plot-keyword> <plot-keyword> <plot-keyword-contract>] ...)
         void?]
@defproc[(plot3d/dc [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                    [dc (is-a?/c dc<%>)]
                    [x real?]
                    [y real?]
                    [width (>=/c 0)]
                    [height (>=/c 0)]
                    [#:<plot3d-keyword> <plot3d-keyword> <plot3d-keyword-contract>] ...)
         void?]{
Plot to an arbitrary device context, in the rectangle with width @(racket width), height @(racket height), and upper-left corner @(racket x),@(racket y).
These procedures accept the same arguments as @(racket plot) and @(racket plot3d), except deprecated keywords, and @racket[#:out-file] and @racket[#:out-kind].

Use these if you need to continually update a plot on a @(racket canvas%), or to create other @(racket plot)-like functions with different backends.
}

@section{Pict-Plotting Work-a-Likes}

@declare-exporting[plot/pict]
@defmodule*/no-declare[(plot/pict plot/typed/pict)]

When setting up an evaluator for a Scribble manual, require @racketmodname[plot/pict] instead of @racketmodname[plot].
Evaluation will produce picts instead of snips, which scale nicely in PDF-rendered documentation.

For example, this is how the evaluator for the @(plot-name) documentation is defined:
@racketblock[
(define plot-eval
  (let ([eval  (make-base-eval)])
    (eval '(begin
             (require racket/math
                      racket/match
                      racket/list
                      racket/draw
                      racket/class
                      plot/pict
                      plot/utils)))
    eval))]

If you use @racket[(require (for-label plot))], links in example code should resolve to documentation for the functions exported by @racketmodname[plot].

@defproc[(plot [<plot-argument> <plot-argument-contract>] ...) pict?]
@defproc[(plot3d [<plot3d-argument> <plot3d-argument-contract>] ...) pict?]{
Like the functions of the same name exported from @racketmodname[plot], but these produce @racket[pict] instances instead of interactive snips.
}

@section{Bitmap-Plotting Work-a-Likes}

@declare-exporting[plot/bitmap]
@defmodule*/no-declare[(plot/bitmap plot/typed/bitmap)]

When plotting in an environment where @racket[bitmap%] instances can be shown but @racket[snip%] instances cannot (for example, on a web page that evaluates Racket code), require @racketmodname[plot/bitmap] instead of @racketmodname[plot].

@defproc[(plot [<plot-argument> <plot-argument-contract>] ...) (is-a?/c bitmap%)]
@defproc[(plot3d [<plot3d-argument> <plot3d-argument-contract>] ...) (is-a?/c bitmap%)]{
Like the functions of the same name exported from @racketmodname[plot], but these produce @racket[bitmap%] instances instead of interactive snips.
}
