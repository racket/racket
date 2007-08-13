#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[check-box% object% (control<%>)]{

A check box is a labeled box which is either checked or unchecked.

Whenever a check box is clicked by the user, the check box's value is
 toggled and its callback procedure is invoked. A callback procedure
 is provided as an initialization argument when each check box is
 created.




@defconstructor[([label (or/c label-string? (is-a/c bitmap%))]
                 [parent (or/c (is-a/c frame%) (is-a/c dialog%)
                               (is-a/c panel%) (is-a/c pane%))]
                 [callback ((is-a/c check-box%) (is-a/c control-event%) . -> . any) (lambda (c e) (void))]
                 [style (listof (one-of/c 'deleted)) null]
                 [value any/c #f]
                 [font (is-a/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a check box with a string or bitmap label. @bitmaplabeluse[label]

@labelstripped[(scheme label) @elem{ (when @scheme[label] is a string)} @elem{effectively click the check box}]

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['check-box]) whenever the user clicks the check box.

@DeletedStyleNote{check box}

If @scheme[value] is true, it is passed to
@method[check-box% set-value] so that the box is initially checked.

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-value)
           boolean?]{
Gets the state of the check box: @scheme[#t] if it is checked, @scheme[#f]
 otherwise.

}

@defmethod[#:mode 'add 
           (set-label [label (is-a/c bitmap%)])
           void?]{

Sets the bitmap label for a bitmap check box.

@bitmaplabeluseisbm[label] @|bitmapiforiglabel|



}

@defmethod[(set-value [state any/c])
           void?]{

Sets the check box's state. (The control's callback procedure is
@italic{not} invoked.)

@MonitorCallback[@elem{The check box's state} @elem{the user clicking the control} @elem{state}]


If @scheme[state] is @scheme[#f], the box is
 unchecked, otherwise it is checked.

}}

