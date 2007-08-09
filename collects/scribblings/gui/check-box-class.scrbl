#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[check-box% object% (control<%>)]{

A check box is a labeled box which is either checked or unchecked.

Whenever a check box is clicked by the user, the check box's value is
 toggled and its callback procedure is invoked. A callback procedure
 is provided as an initialization argument when each check box is
 created.




@defconstructor[[label {\labelstring} or @scheme[bitmap%] object]
                [parent (or/c (is-a/c frame%) (is-a/c dialog%) (is-a/c panel%) (is-a/c pane%))]
                [callback procedure of two arguments: a @scheme[check-box%] object and a @scheme[control-event%] object @scheme[(\scmk{lambda] (@scheme[c] @scheme[e]) (void))}]
                [style (symbols/c deleted) null]
                [value any/c #f]
                [font (is-a/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 2]
                [horiz-margin (integer-in 0 1000) 2]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c #f]
                [stretchable-height any/c #f]]{

Creates a check box with a string or bitmap label. @bitmaplabeluse[label]

@labelstripped[(scheme label) @elem{ (when @scheme[label] is a string)} @elem{effectively click the check box}]

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['check-box]) whenever the user clicks the check box.

\DeletedStyleNote{check box}

If @scheme[value] is true, it is passed to
@method[check-box% set-value] so that the box is initially checked.

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-value)
           boolean?]{
@spec{

Gets the state of the check box: @scheme[#t] if it is checked, @scheme[#f]
 otherwise.

}}

@defmethod[#:mode 'add 
           (set-label [label (is-a/c bitmap%)])
           void?]{
@spec{

Sets the bitmap label for a bitmap check box.

}
@impl{

@bitmaplabeluseisbm[label] @|bitmapiforiglabel|



}}

@defmethod[(set-value [state any/c])
           void?]{
@spec{

Sets the check box's state. (The control's callback procedure is {\em
not} invoked.)

@MonitorCallback[@elem{The check box's state} @elem{the user clicking the control} @elem{state}]

}
@impl{

If @scheme[state] is @scheme[#f], the box is
 unchecked, otherwise it is checked.




}}}

