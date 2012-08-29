#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/check-box}}

@defclass/title[check-box% object% (control<%>)]{

A check box is a labeled box which is either checked or unchecked.

Whenever a check box is clicked by the user, the check box's value is
 toggled and its callback procedure is invoked. A callback procedure
 is provided as an initialization argument when each check box is
 created.




@defconstructor[([label (or/c label-string? (is-a?/c bitmap%))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c check-box%) (is-a?/c control-event%)
                            . -> . any) (lambda (c e) (void))]
                 [style (listof (or/c 'deleted)) null]
                 [value any/c #f]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a check box with a string or bitmap label. @bitmaplabeluse[label]

@labelstripped[@racket[label]
  @elem{ (when @racket[label] is a string)}
  @elem{effectively click the check box}]

The @racket[callback] procedure is called (with the event type
 @indexed-racket['check-box]) whenever the user clicks the check box.

@DeletedStyleNote[@racket[style] @racket[parent]]{check box}

If @racket[value] is true, it is passed to
@method[check-box% set-value] so that the box is initially checked.

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-value)
           boolean?]{
Gets the state of the check box: @racket[#t] if it is checked, @racket[#f]
 otherwise.

}

@defmethod[#:mode override
           (set-label [label (or/c label-string? (is-a?/c bitmap%))])
           void?]{

The same as @xmethod[window<%> set-label] when @racket[label] is a
 string.

Otherwise, sets the bitmap label for a bitmap check box.
@bitmaplabeluseisbm[label] @|bitmapiforiglabel|

}

@defmethod[(set-value [state any/c])
           void?]{

Sets the check box's state. (The control's callback procedure is
@italic{not} invoked.)

@MonitorCallback[@elem{The check box's state} @elem{the user clicking the control} @elem{state}]


If @racket[state] is @racket[#f], the box is
 unchecked, otherwise it is checked.

}}

