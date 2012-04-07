#lang scribble/doc
@(require "common.rkt")

@defclass/title[vertical-panel% panel% ()]{

A vertical panel arranges its subwindows in a single column. See
 also @racket[panel%].




@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'border 'deleted
                                      'hscroll 'auto-hscroll
                                      'vscroll 'auto-vscroll)) null]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 0]
                 [horiz-margin (integer-in 0 1000) 0]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

The @racket[style] flags are the same as for @racket[panel%].

@WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]
}

@defmethod[(set-orientation [horizontal? boolean?]) void?]{
  Sets the orientation of the panel, switching it between
  the behavior of the @racket[vertical-panel%] and that of
  the @racket[horizontal-panel%].
}

@defmethod[(get-orientation) boolean?]{
  Initially returns @racket[#f], but if 
  @method[vertical-panel% set-orientation] is called,
  this method returns whatever the last value passed to it was.
}
}

