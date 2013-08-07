#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/group-box-panel}}

@defclass/title[group-box-panel% vertical-panel% ()]{

A group-box panel arranges its subwindows in a single column, but also
 draws an optional label at the top of the panel and a border around
 the panel content.

Unlike most panel classes, a group-box panel's horizontal and vertical
 margins default to @racket[2].


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'deleted)) null]
                 [font (is-a?/c font%) small-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

Creates a group pane whose title is @racket[label].

@DeletedStyleNote[@racket[style] @racket[parent]]{group panel}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]


}}

