#lang scribble/doc
@(require "common.rkt")

@defclass/title[pane% object% (area-container<%> subarea<%>)]{

A pane is a both a container and a containee area. It serves only
 as a geometry management device. A @racket[pane%]
 cannot be hidden or disabled like a @racket[panel%] object.

A @racket[pane%] object has a degenerate placement strategy for
 managing its children: it places each child as if it was the only
 child of the panel.  The @racket[horizontal-pane%] and
 @racket[vertical-pane%] classes provide useful geometry management
 for multiple children.

See also @racket[grow-box-spacer-pane%].

@history[#:changed "1.3" @elem{Changed the placement strategy to
                               stretch and align children, instead of
                               placing all children at the top-left
                               corner.}]

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

