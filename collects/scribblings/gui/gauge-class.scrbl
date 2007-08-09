#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[gauge% object% (control<%>)]{

A gauge is a horizontal or vertical bar for displaying the output
 value of a bounded integer quantity. Each gauge has an adjustable
 range, and the gauge's current value is always between 0 and its
 range, inclusive. Use
@method[gauge% set-value] to set the value of the gauge.




@defconstructor[[label (or/c label-string? false/c)]
                [range (integer-in 1 10000)]
                [parent (or/c (is-a/c frame%) (is-a/c dialog%) (is-a/c panel%) (is-a/c pane%))]
                [style (symbols/c deleted horizontal-label vertical-label vertical horizontal) '(horizontal)]
                [font (is-a/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 2]
                [horiz-margin (integer-in 0 1000) 2]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c \#t {\rm for} @scheme['horizontal] {\rm style}, \#f {\rm for} @scheme['vertical]]
                [stretchable-height any/c \#t {\rm for} @scheme['vertical] {\rm style}, \#f {\rm for} @scheme['horizontal]]]{

If @scheme[label] is a string, it is used as the gauge label; otherwise
 the gauge does not display a label.

@labelstripped[(scheme label) @elem{gauge}]

The @scheme[range] argument is an integer specifying the maximum value of
 the gauge (inclusive). The minimum gauge value is always @scheme[0].

The @scheme[style] list must include either @scheme['horizontal],
 specifying a horizontal gauge, or @scheme['vertical], specifying
 a vertical gauge. \HVLabelNote{gauge} \DeletedStyleNote{gauge}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(set-range [range (integer-in 1 10000)])
           void?]{
@spec{

Sets the range (maximum value) of the gauge.

}}

@defmethod[(get-range)
           (integer-in 1 10000)]{
@spec{

Returns the range (maximum value) of the gauge.

}}

@defmethod[(set-value [pos (integer-in 0 10000)])
           void?]{
@spec{

Sets the gauge's current value. If the specified value is larger than
 the gauge's range, @|MismatchExn|}.

}}

@defmethod[(get-value)
           (integer-in 0 10000)]{
@spec{

Returns the gauge's current value.

}
@impl{





}}}

