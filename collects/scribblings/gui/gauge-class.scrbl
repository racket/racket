#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/gauge}}

@defclass/title[gauge% object% (control<%>)]{

A gauge is a horizontal or vertical bar for displaying the output
value of a bounded integer quantity. Each gauge has an adjustable
range, and the gauge's current value is always between 0 and its
range, inclusive. Use @method[gauge% set-value] to set the value
of the gauge.


@defconstructor[([label (or/c label-string? #f)]
                 [range (integer-in 1 1000000)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'horizontal 'vertical 
                                      'vertical-label 'horizontal-label 
                                      'deleted)) 
                        '(horizontal)]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c (memq 'horizontal style)]
                 [stretchable-height any/c (memq 'vertical style)])]{

If @racket[label] is a string, it is used as the gauge label; otherwise
 the gauge does not display a label.

@labelsimplestripped[@racket[label] @elem{gauge}]

The @racket[range] argument is an integer specifying the maximum value of
 the gauge (inclusive). The minimum gauge value is always @racket[0].

The @racket[style] list must include either @racket['horizontal],
 specifying a horizontal gauge, or @racket['vertical], specifying a vertical
 gauge. @HVLabelNote[@racket[style]]{gauge} @DeletedStyleNote[@racket[style]
 @racket[parent]]{gauge}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]



}

@defmethod[(get-range)
           (integer-in 1 1000000)]{
Returns the range (maximum value) of the gauge.

}

@defmethod[(get-value)
           (integer-in 0 1000000)]{

Returns the gauge's current value.

}

@defmethod[(set-range [range (integer-in 1 1000000)])
           void?]{

Sets the range (maximum value) of the gauge.

}

@defmethod[(set-value [pos (integer-in 0 1000000)])
           void?]{

Sets the gauge's current value. If the specified value is larger than
 the gauge's range, @|MismatchExn|.

}}

