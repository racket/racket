#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/slider}}

@defclass/title[slider% object% (control<%>)]{

A @racket[slider] object is a panel item with a handle that the user can
 drag to change the control's value. Each slider has a fixed minimum
 and maximum value.

Whenever the user changes the value of a slider, its callback
 procedure is invoked. A callback procedure is provided as an
 initialization argument when each slider is created.




@defconstructor[([label (or/c label-string? #f)]
                 [min-value position-integer?]
                 [max-value position-integer?]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c slider%) (is-a?/c control-event%) . -> . any) (lambda (b e) (void))]
                 [init-value position-integer? min-value]
                 [style (listof (or/c 'horizontal 'vertical 'plain 
                                      'vertical-label 'horizontal-label 
                                      'deleted)) 
                        '(horizontal)]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c (memq 'horizontal style)]
                 [stretchable-height any/c (memq 'vertical style)])]{

If @racket[label] is a string, it is used as the label for the slider.
 Otherwise, the slider does not display its label.

@labelstripped[@racket[label]
  @elem{} @elem{move the keyboard focus to the slider}]

The @racket[min-value] and @racket[max-value] arguments specify the
 range of the slider, inclusive. The @racket[init-value] argument
 optionally specifies the slider's initial value. If the sequence
 [@racket[min-value], @racket[initial-value], @racket[maximum-value]]
 is not increasing, @|MismatchExn|.

The @racket[callback] procedure is called (with the event type
 @indexed-racket['slider]) when the user changes the slider's value.

The @racket[style] argument must include either @racket['vertical] for
 a vertical slider, or @racket['horizontal] for a horizontal
 slider. If @racket[style] includes @racket['plain], the slider does
 not display numbers for its range and current value to the user.
 @HVLabelNote[@racket[style]]{slider} @DeletedStyleNote[@racket[style] @racket[parent]]{slider}

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]


}

@defmethod[(get-value)
           position-integer?]{

Gets the current slider value.

}

@defmethod[(set-value [value position-integer?])
           void?]{

Sets the value (and displayed position) of the slider. (The control's
 callback procedure is @italic{not} invoked.) If @racket[value] is
 outside the slider's minimum and maximum range, @|MismatchExn|.

@MonitorCallback[@elem{A slider's value} @elem{the user clicking the control} @elem{value}]

}}

