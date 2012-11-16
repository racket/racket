#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/radio-box}}

@defclass/title[radio-box% object% (control<%>)]{


A @racket[radio-box%] control allows the user to select one of a
 number of mutually exclusive items. The items are displayed as a
 vertical column or horizontal row of labelled @defterm{radio
 buttons}. Unlike a @racket[list-control<%>], the set of items in a
 @racket[radio-box%] cannot be changed dynamically.

Whenever the user changes the selected radio button, the radio box's
 callback procedure is invoked. A callback procedure is provided as an
 initialization argument when each radio box is created.




@defconstructor[([label (or/c label-string? #f)]
                 [choices (or/c (listof label-string?) (listof (is-a?/c bitmap%)))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c radio-box%) (is-a?/c control-event%)
                            . -> . any) 
                           (lambda (r e) (void))]
                 [style (listof (or/c 'horizontal 'vertical 
                                      'vertical-label 'horizontal-label 
                                      'deleted)) 
                        '(vertical)]
                 [selection (or/c exact-nonnegative-integer? #f) 0]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a radio button set with string or bitmap labels. The
 @racket[choices] list specifies the radio button labels; the list of
 choices must be homogeneous, either all strings or all bitmaps.

@labelstripped[@racket[label]
  @elem{} @elem{move the keyboard focus to the radio box}]

Each string in @racket[choices] can also contain a @litchar{&}, which
 creates a mnemonic for clicking the corresponding radio button. As
 for @racket[label], a @litchar{&&} is converted to a @litchar{&}.

@bitmaplabelusearray[choices]

If @racket[label] is a string, it is used as the label for the radio
 box. Otherwise, the radio box does not display its
 label.

The @racket[callback] procedure is called (with the event type
 @indexed-racket['radio-box]) when the user changes the radio button
 selection.

The @racket[style] argument must include either @racket['vertical] for a
 collection of radio buttons vertically arranged, or
 @racket['horizontal] for a horizontal arrangement.
 @HVLabelNote[@racket[style]]{radio box} @DeletedStyleNote[@racket[style] @racket[parent]]{radio box}

By default, the first radio button is initially selected. If
 @racket[selection] is positive or @racket[#f], it is passed to
 @method[radio-box% set-selection] to set the initial radio button
 selection.

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

}


@defmethod*[#:mode override 
            ([(enable [enable? any/c])
              void?]
             [(enable [n exact-nonnegative-integer?]
                      [enable? any/c])
              void?])]{

If a single argument is provided, the entire radio box is enabled or disabled.

If two arguments are provided, then if @racket[enable?] is
 @racket[#f], the @racket[n]th radio button is disabled, otherwise it
 is enabled (assuming the entire radio box is enabled). Radio buttons
 are numbered from @racket[0].  If @racket[n] is equal to or larger
 than the number of radio buttons in the radio box, @|MismatchExn|.

}


@defmethod[(get-item-label [n exact-nonnegative-integer?])
           string?]{

Gets the label of a radio button by position. Radio buttons are
 numbered from @racket[0]. If @racket[n] is equal to or larger than
 the number of radio buttons in the radio box, @|MismatchExn|.

}

@defmethod[(get-item-plain-label [n exact-nonnegative-integer?])
           string?]{

Like @method[radio-box% get-item-label], except that the label must be
a string and @litchar{&}s in the label are removed.

}

@defmethod[(get-number)
           exact-nonnegative-integer?]{

Returns the number of radio buttons in the radio box.

}

@defmethod[(get-selection)
           (or/c exact-nonnegative-integer? #f)]{

Gets the position of the selected radio button, returning @racket[#f]
if no button is selected. Radio buttons are numbered from @racket[0].

}

@defmethod*[#:mode override 
            ([(is-enabled?)
              boolean?]
             [(is-enabled? [n exact-nonnegative-integer?])
              boolean?])]{

If no arguments are provided, the enable state of the entire radio box
is reported.

Otherwise, returns @racket[#f] if @racket[n]th radio button is
disabled (independent of disabling the entire radio box), @racket[#t]
otherwise. Radio buttons are numbered from @racket[0].  If @racket[n]
is equal to or larger than the number of radio buttons in the radio
box, @|MismatchExn|.

}

@defmethod[(set-selection [n (or/c exact-nonnegative-integer? #f)])
           void?]{

Sets the selected radio button by position, or deselects all radio
 buttons if @racket[n] is @racket[#f]. (The control's callback
 procedure is @italic{not} invoked.) Radio buttons are numbered from
 @racket[0]. If @racket[n] is equal to or larger than the number of
 radio buttons in the radio box, @|MismatchExn|.

@MonitorCallback[@elem{A radio box's selection} @elem{the user clicking the control} @elem{selection}]

}}

