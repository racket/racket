#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[slider% object% (control<%>)]{

A @scheme[slider] object is a panel item with a handle that the user can
 drag to change the control's value. Each slider has a fixed minimum
 and maximum value.

Whenever the user changes the value of a slider, its callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each slider is created.




@defconstructor[[label (or/c label-string? false/c)]
                [min-value (integer-in -10000 10000)]
                [max-value (integer-in -10000 10000)]
                [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) (is-a?/c panel%) (is-a?/c pane%))]
                [callback procedure of two arguments: a @scheme[slider%] object and a @scheme[control-event%] object @scheme[(\scmk{lambda] (@scheme[s] @scheme[e]) (void))}]
                [init-value (integer-in -10000 10000) @scheme[min-value]]
                [style (symbols/c deleted horizontal-label vertical-label plain vertical horizontal) '(horizontal)]
                [font (is-a?/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 2]
                [horiz-margin (integer-in 0 1000) 2]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c \#t {\rm for} @scheme['horizontal] {\rm style}, \#f {\rm for} @scheme['vertical]]
                [stretchable-height any/c \#t {\rm for} @scheme['vertical] {\rm style}, \#f {\rm for} @scheme['horizontal]]]{

If @scheme[label] is a string, it is used as the label for the slider.
 Otherwise, the slider does not display its
 label. 

@labelstripped[(scheme label) @elem{} @elem{move the keyboard focus to the slider}]

The @scheme[min-value] and @scheme[max-value] arguments specify the range of
 the slider, inclusive. The @scheme[init-value] argument optionally
 specifies the slider's initial value. If the sequence
 [@scheme[min-value], @scheme[initial-value], @scheme[maximum-value]] is not
 increasing, @|MismatchExn|}.

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['slider]) when the user changes the slider's value.

The @scheme[style] argument must include either @scheme['vertical] for a
 vertical slider, or @scheme['horizontal] for a horizontal slider. If
 @scheme[style] includes @scheme['plain], the slider does not display
 numbers for its range and current value to the user.
 \HVLabelNote{slider} \DeletedStyleNote{slider}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-value)
           (integer-in -10000 10000)]{
@spec{

Gets the current slider value.

}}

@defmethod[(set-value [value (integer-in -10000 10000)])
           void?]{
@spec{

Sets the value (and displayed position) of the slider. (The control's
 callback procedure is {\em not} invoked.)

@MonitorCallback[@elem{A slider's value} @elem{the user clicking the control} @elem{value}]

}
@impl{

If @scheme[value] is outside the slider's minimum and maximum range,
 @|MismatchExn|}.




}}}

