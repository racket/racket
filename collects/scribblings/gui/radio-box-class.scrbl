#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[radio-box% object% (control<%>)]{


A @scheme[radio-box%] control allows the user to select one of
 number of mutually exclusive items. The items are displayed as a
 vertical column or horizontal row of labelled @defterm{radio
 buttons}. Unlike a @scheme[list-control<%>], the set of items in a
 @scheme[radio-box%] cannot be changed dynamically.

Whenever the user changes the selected radio button, the radio box's
 callback procedure is invoked. A callback procedure is provided as an
 initialization argument when each radio box is created.




@defconstructor[[label (or/c label-string? false/c)]
                [choices list of {\labelstrings} or @scheme[bitmap%] objects]
                [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) (is-a?/c panel%) (is-a?/c pane%))]
                [callback procedure of two arguments: a @scheme[radio-box%] object and a @scheme[control-event%] object @scheme[(\scmk{lambda] (@scheme[rb] @scheme[e]) (void))}]
                [style (symbols/c deleted horizontal-label vertical-label vertical horizontal) '(vertical)]
                [selection nonnegative-exact-integer? 0]
                [font (is-a?/c font%) @scheme[normal-control-font]]
                [enabled any/c #t]
                [vert-margin (integer-in 0 1000) 2]
                [horiz-margin (integer-in 0 1000) 2]
                [min-width (integer-in 0 10000) {\rm graphical minimum width}]
                [min-height (integer-in 0 10000) {\rm graphical minimum height}]
                [stretchable-width any/c #f]
                [stretchable-height any/c #f]]{

Creates a radio button set with string or bitmap labels. The
 @scheme[choices] list specifies the radio button labels; the list of
 choices must be homogeneous, either all strings or all bitmaps.

@labelstripped[(scheme label) @elem{} @elem{move the keyboard focus to the radio box}]

Each string in @scheme[choices] can also contain an ampersand, which
 creates a mnemonic for clicking the corresponding radio button. As
 for @scheme[label], a double ampersand is converted to a single
 ampersand.

@bitmaplabelusearray[choices]

If @scheme[label] is a string, it is used as the label for the radio
 box. Otherwise, the radio box does not display its
 label.

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['radio-box]) when the user changes the radio button
 selection.

The @scheme[style] argument must include either @scheme['vertical] for a
 collection of radio buttons vertically arranged, or
 @scheme['horizontal] for a horizontal arrangement.
 \HVLabelNote{radio box} \DeletedStyleNote{radio box}

By default, the first radio button is initially selected. If
 @scheme[selection] is positive, it is passed to
@method[radio-box% set-selection] to set the initial radio button selection.

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]



}

@defmethod*[#:mode 'override 
            ([(enable [enable? any/c])
              void?]
             [(enable [n nonnegative-exact-integer?]
                      [enable? any/c])
              void?])]{
@impl{
First case:


If @scheme[enable?] is @scheme[#f], the entire radio box is disabled,
 otherwise it is enabled.



Second case:


If @scheme[enable?] is @scheme[#f], the @scheme[n]th radio button is disabled,
 otherwise it is enabled (assuming the entire radio box is
 enabled). Radio buttons are numbered from @scheme[0].
 If @scheme[n] is equal to or larger than the number of radio buttons in
 the radio box, @|MismatchExn|}.



}}

@defmethod*[#:mode 'override 
            ([(is-enabled?)
              boolean?]
             [(is-enabled? [n nonnegative-exact-integer?])
              boolean?])]{
@impl{
First case:


Returns @scheme[#f] if the entire radio box is disabled,
 @scheme[#t] otherwise.



Second case:


Returns @scheme[#f] if @scheme[n]th radio button is disabled (independent of
 disabling the entire radio box), @scheme[#t] otherwise. Radio buttons
 are numbered from @scheme[0].  If @scheme[n] is equal to or larger than the
 number of radio buttons in the radio box, @|MismatchExn|}.



}}

@defmethod[(get-selection)
           nonnegative-exact-integer?]{
@spec{

Gets the position of the selected radio button. Radio buttons are
 numbered from @scheme[0].

}}

@defmethod[(get-number)
           nonnegative-exact-integer?]{
@spec{

Returns the number of radio buttons in the radio box.

}}

@defmethod[(set-selection [n nonnegative-exact-integer?])
           void?]{
@spec{

Sets the selected radio button by position. (The control's callback
 procedure is {\em not} invoked.) Radio buttons are numbered from
 @scheme[0].

@MonitorCallback[@elem{A radio box's selection} @elem{the user clicking the control} @elem{selection}]

}
@impl{

If @scheme[n] is equal to or larger than the number of radio buttons in
 the radio box, @|MismatchExn|}.



}}

@defmethod[(get-item-label [n nonnegative-exact-integer?])
           string]{
@spec{

Gets the label of a radio button by position. Radio buttons are
 numbered from @scheme[0].

}
@impl{

If @scheme[n] is equal to or larger than the number of radio buttons in
 the radio box, @|MismatchExn|}.



}}

@defmethod[(get-item-plain-label [n nonnegative-exact-integer?])
           string]{
@spec{

Like
@method[radio-box% get-item-label], except that the label must be a string and ampersands in the label
 are removed.

}
@impl{

If @scheme[n] is equal to or larger than the number of radio buttons in
 the radio box, @|MismatchExn|}.



}}}

