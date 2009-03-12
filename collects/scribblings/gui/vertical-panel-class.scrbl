#lang scribble/doc
@(require "common.ss")

@defclass/title[vertical-panel% panel% ()]{

A vertical panel arranges its subwindows in a single column. See
 also @scheme[panel%].




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
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If the @scheme['border] style is specified, the window is created with
 a thin border (only in this case, the client size of the panel may be
 less than its total size). @DeletedStyleNote[@scheme[style] @scheme[parent]]{panel}

@WindowKWs[@scheme[enabled]] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]
}

@defmethod[(set-orientation [horizontal? boolean?]) void?]{
  Sets the orientation of the panel, switching it between
  the behavior of the @scheme[vertical-panel%] and that of
  the @scheme[horizontal-panel%].
}

@defmethod[(get-orientation) boolean?]{
  Initially returns @scheme[#f], but if 
  @method[vertical-panel% set-orientation] is called,
  this method returns whatever the last value passed to it was.
}
}

