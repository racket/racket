#lang scribble/doc
@(require "common.ss")

@(define lbnumnote @elem{List box items are indexed from @scheme[0].})


@defclass/title[list-box% object% (list-control<%>)]{

A list box allows the user to select one or more string items from a
 scrolling list. A list box is either a single-selection control (if
 an item is selected, the previous selection is removed) or a
 multiple-selection control (clicking an item toggles the item on or
 off independently of other selections).

Whenever the user changes the selection in a list box, the list box's
 callback procedure is called. A callback procedure is provided as an
 initialization argument when each list box is created.

@|lbnumnote|

See also @scheme[choice%].



@defconstructor[([label (or/c label-string? false/c)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c list-box%) (is-a?/c control-event%) . -> . any) 
                           (lambda (c e) (void))]
                 [style (listof (one-of/c 'single 'multiple 'extended 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(single)]
                 [selection (or/c exact-nonnegative-integer? false/c) #f]
                 [font (is-a?/c font%) view-control-font]
                 [label-font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If @scheme[label] is not @scheme[#f], it is used as the list box
 label.  Otherwise, the list box will not display its label.

@labelstripped[(scheme label) @elem{} @elem{move the keyboard focus to the list box}]

The @scheme[choices] list specifies the initial list of items
 to appear in the list box.

The @scheme[callback] procedure is called when the user changes the list
 box selection, by either selecting, re-selecting, deselecting, or
 double-clicking an item.  The type of the event provided to the
 callback is @indexed-scheme['list-box-dclick] when the user double-clicks
 on an item, or @indexed-scheme['list-box] otherwise.

The @scheme[style] specification must include exactly one of the
 following:
@itemize{

 @item{@scheme['single] --- Creates a single-selection list.}

 @item{@scheme['multiple] --- Creates a multiple-selection list
 where a single click deselects other items and selects a new
 item. Use this style for a list when single-selection is common, but
 multiple selections are allowed.}

 @item{@scheme['extended] --- Creates a multiple-selection list where a
 single click extends or contracts the selection by toggling the
 clicked item. Use this style for a list when multiple selections are
 the rule rather than the exception.}

}
The @scheme['multiple] and @scheme['extended] styles determine a
 platform-independent interpretation of unmodified mouse clicks, but
 dragging, shift-clicking, control-clicking, etc. have
 platform-standard interpretations. Whatever the platform-specific
 interface, the user can always select disjoint sets of items or
 deselect items (and leave no items selected). On some platforms, the
 user can deselect the (sole) selected item in a @scheme['single] list
 box.

@HVLabelNote[@scheme[style]]{list box} @DeletedStyleNote[@scheme[style] @scheme[parent]]{list box}

If @scheme[selection] is an integer, it is passed to
@method[list-control<%> set-selection] to set the initial selection. The @scheme[selection] must be less than
 the length of @scheme[choices].

@FontLabelKWs[@scheme[font] @scheme[label-font]] @WindowKWs[@scheme[enabled]] @SubareaKWs[] @AreaKWs[]

}


@defmethod[#:mode override
           (append [item string]
                   [data any/c #f])
           void?]{

Adds a new item to the list box with an associated ``data'' object.
 The @scheme[data] object is not displayed in the list box; it is
 provided merely as a convenience for use with @method[list-box%
 get-data], possibly allowing a programmer to avoid managing a
 separate item-to-data mapping in addition to the list box control.

See also @xmethod[list-control<%> append].

}


@defmethod[(delete [n exact-nonnegative-integer?])
           void?]{

Deletes the item indexed by @scheme[n]. @|lbnumnote| If @scheme[n] is equal
 to or larger than the number of items in the control, @|MismatchExn|.

Selected items that are not deleted remain selected, and no other
 items are selected.

}

@defmethod[(get-data [n exact-nonnegative-integer?])
           any/c]{

Returns the data for the item indexed by @scheme[n], or @scheme[#f]
 if there is no associated data. @|lbnumnote| If
 @scheme[n] is equal to or larger than the number of choices,
 @|MismatchExn|.

See also @method[list-box% append] and @method[list-box% set-data].

}


@defmethod[(get-first-visible-item)
           exact-nonnegative-integer?]{

Reports the index of the item currently scrolled to the top of the
 list box. @|lbnumnote|

}

@defmethod[(get-label-font)
           (is-a?/c font%)]{

Returns the font used for the control's label, which is optionally
 supplied when a list box is created.

}

@defmethod[(get-selections)
           (listof exact-nonnegative-integer?)]{

Returns a list of indices for all currently selected items.
 @|lbnumnote|

For single-selection lists, the result is always either @scheme[null] or
 a list containing one number.

}


@defmethod[(is-selected? [n exact-nonnegative-integer?])
           boolean?]{

Returns @scheme[#t] if the item index by @scheme[n] is selected,
 @scheme[#f] otherwise. @|lbnumnote| If @scheme[n] is equal to or
 larger than the number of choices, @|MismatchExn|.


@MonitorCallback[@elem{A list box's selection} @elem{the user clicking the control} @elem{selection}]

}

@defmethod[(number-of-visible-items)
           exact-positive-integer?]{

Returns the maximum number of items in the list box that are visible
 to the user with the control's current size (rounding down if the
 exact answer is fractional, but returning at least @scheme[1]).

}

@defmethod[(select [n exact-nonnegative-integer?]
                   [select? any/c #t])
           void?]{

Selects or deselects an item. For selection in a single-selection list
 box, if a different choice is currently selected, it is automatically
 deselected. For selection in a multiple-selection list box, other
 selections are preserved, unlike
@method[list-control<%> set-selection].

If @scheme[select?] is @scheme[#f], the item indexed by @scheme[n] is
 deselected; otherwise it is selected. @|lbnumnote| If @scheme[n] is
 equal to or larger than the number of choices, @|MismatchExn|.

@MonitorCallback[@elem{A list box's selection} @elem{the user clicking the control} @elem{selection}]

The control's callback procedure is @italic{not} invoked.

}


@defmethod[(set [choices (listof label-string?)])
           void?]{

Clears the list box and installs a new list of items.

}


@defmethod[(set-data [n exact-nonnegative-integer?]
                     [data any/c])
           void?]{

Sets the associated data for item indexed by @scheme[n]. @|lbnumnote| If
 @scheme[n] is equal to or larger than the number of choices,
 @|MismatchExn|.

See also @method[list-box% append].

}


@defmethod[(set-first-visible-item [n exact-nonnegative-integer?])
           void?]{

Scrolls the list box so that the item indexed by @scheme[n] is at the
 top of the list box display. @|lbnumnote| If @scheme[n] is equal to
 or larger than the number of choices, @|MismatchExn|.

@Unmonitored[@elem{A list box's scroll position} @elem{the user clicking the control} @elem{the scroll position
 changes} @elem{@method[list-box% get-first-visible-item]}]

}


@defmethod[(set-string [n exact-nonnegative-integer?]
                       [label label-string?])
           void?]{

Sets the item indexed by @scheme[n]. @|lbnumnote| If @scheme[n] is
equal to or larger than the number of choices, @|MismatchExn|.

}
}
