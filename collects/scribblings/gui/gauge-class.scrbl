#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["control-intf.scrbl"]

@defclass[gauge% object% (control<%>)]{

A gauge is a horizontal or vertical bar for displaying the output
value of a bounded integer quantity. Each gauge has an adjustable
range, and the gauge's current value is always between 0 and its
range, inclusive. Use @method[gauge% set-value] to set the value
of the gauge.


@defconstructor[([label (or/c label-string? false/c)]
                 [range (integer-in 1 10000)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'horizontal 'vertical 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(horizontal)]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c (memq 'horizontal style)]
                 [stretchable-height any/c (memq 'vertical style)])]{

If @scheme[label] is a string, it is used as the gauge label; otherwise
 the gauge does not display a label.

@labelsimplestripped[(scheme label) @elem{gauge}]

The @scheme[range] argument is an integer specifying the maximum value of
 the gauge (inclusive). The minimum gauge value is always @scheme[0].

The @scheme[style] list must include either @scheme['horizontal],
 specifying a horizontal gauge, or @scheme['vertical], specifying
 a vertical gauge. @HVLabelNote{gauge} @DeletedStyleNote{gauge}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-range)
           (integer-in 1 10000)]{
Returns the range (maximum value) of the gauge.

}

@defmethod[(get-value)
           (integer-in 0 10000)]{

Returns the gauge's current value.

}

@defmethod[(set-range [range (integer-in 1 10000)])
           void?]{

Sets the range (maximum value) of the gauge.

}

@defmethod[(set-value [pos (integer-in 0 10000)])
           void?]{

Sets the gauge's current value. If the specified value is larger than
 the gauge's range, @|MismatchExn|.

}}

