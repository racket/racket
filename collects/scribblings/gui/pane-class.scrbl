#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[pane% object% (area-container<%> subarea<%>)]{

A pane is a both a container and a containee area. It serves only
 as a geometry management device. A @scheme[pane%]
 cannot be hidden or disabled like a @scheme[panel%] object.

A @scheme[pane%] object has a degenerate placement strategy for
 managing its children; it places them all in the upper left corner
 and does not stretch any of them. The @scheme[horizontal-pane%] and
 @scheme[vertical-pane%] classes provide useful geometry management.

See also @scheme[grow-box-spacer-pane%].


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

@SubareaKWs[] @AreaContKWs[] @AreaKWs[]

}}

