#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[panel% object% (area-container-window<%> subwindow<%>)]{

A panel is a both a container and a containee window. It serves mainly
 as a geometry management device, but the @scheme['border] creates a
 container with a border. Unlike a @scheme[pane%] object, a @scheme[panel%]
 object can be hidden or disabled.

A @scheme[panel%] object has a degenerate placement strategy for
 managing its children; it places them all in the upper left corner
 and does not stretch any of them.  The @scheme[horizontal-panel%]
 and @scheme[vertical-panel%] classes provide useful geometry
 management.


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'border 'deleted)) null]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center center)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If the @scheme['border] style is specified, the window is created with
 a thin border (only in this case, the client size of the panel may be
 less than its total size). @DeletedStyleNote{panel}

@WindowKWs[] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]

}}

