#lang scribble/doc
@(require "common.rkt")

@defclass/title[pane% object% (area-container<%> subarea<%>)]{

A pane is a both a container and a containee area. It serves only
 as a geometry management device. A @racket[pane%]
 cannot be hidden or disabled like a @racket[panel%] object.

A @racket[pane%] object has a degenerate placement strategy for
 managing its children; it places them all in the upper left corner
 and does not stretch any of them. The @racket[horizontal-pane%] and
 @racket[vertical-pane%] classes provide useful geometry management.

See also @racket[grow-box-spacer-pane%].


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

@SubareaKWs[] @AreaContKWs[] @AreaKWs[]

}}

