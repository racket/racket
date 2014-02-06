#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/list-box}}

@(define lbnumnote @elem{List box rows are indexed from @racket[0].})
@(define lbcnumnote @elem{List box rows and columns are indexed from @racket[0].})


@defclass/title[list-box% object% (list-control<%>)]{

A list box allows the user to select one or more string items from a
 scrolling list. A list box is either a single-selection control (if
 an item is selected, the previous selection is removed) or a
 multiple-selection control (clicking an item toggles the item on or
 off independently of other selections).

Whenever the user changes the selection in a list box, the list box's
 callback procedure is called. A callback procedure is provided as an
 initialization argument when each list box is created.

A list box can have multiple columns with optional column headers. An
 item in the list corresponds to a row that spans all columns. When
 column headers are displayed, the column widths can be changed by a
 user. In addition, columns can optionally support dragging by the
 user to change the display order of columns, while the logical order
 remains fixed.

@|lbcnumnote|

See also @racket[choice%].


@defconstructor[([label (or/c label-string? #f)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c list-box%) (is-a?/c control-event%) 
                            . -> . any) 
                           (lambda (c e) (void))]
                 [style (listof (or/c 'single 'multiple 'extended 
                                      'vertical-label 'horizontal-label 
                                      'variable-columns 'column-headers 
                                      'clickable-headers 'reorderable-headers 
                                      'deleted)) 
                        '(single)]
                 [selection (or/c exact-nonnegative-integer? #f) #f]
                 [font (is-a?/c font%) view-control-font]
                 [label-font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t]
                 [columns (cons/c label-string? (listof label-string?))
                          '("Column")]
                 [column-order (or/c #f (listof exact-nonnegative-integer?)) #f])]{

If @racket[label] is not @racket[#f], it is used as the list box
 label.  Otherwise, the list box will not display its label.

@labelstripped[@racket[label] @elem{}
  @elem{move the keyboard focus to the list box}]

The @racket[choices] list specifies the initial list of items
 to appear in the list box. If the list box has multiple columns, 
 @racket[choices] determines the content of the first column, and
 other columns are initialized to the empty string.

The @racket[callback] procedure is called when the user changes the list
 box selection, by either selecting, re-selecting, deselecting, or
 double-clicking an item.  The type of the event provided to the
 callback is @indexed-racket['list-box-dclick] when the user double-clicks
 on an item, or @indexed-racket['list-box] otherwise.

The @racket[columns] list determines the number of columns in the list
 box. The column titles in @racket[columns] are shown only if
 @racket[style] includes @racket['column-headers]. If @racket[style]
 also includes @racket['clickable-headers], then a click on a header
 triggers a call to @racket[callback] with a
 @racket[column-control-event%] argument whose event type is
 @indexed-racket['list-box-column].

The @racket[style] specification must include exactly one of the
 following:
@itemize[

 @item{@racket['single] --- Creates a single-selection list.}

 @item{@racket['multiple] --- Creates a multiple-selection list
 where a single click deselects other items and selects a new
 item. Use this style for a list when single-selection is common, but
 multiple selections are allowed.}

 @item{@racket['extended] --- Creates a multiple-selection list where a
 single click extends or contracts the selection by toggling the
 clicked item. Use this style for a list when multiple selections are
 the rule rather than the exception.}

]
The @racket['multiple] and @racket['extended] styles determine a
 platform-independent interpretation of unmodified mouse clicks, but
 dragging, shift-clicking, control-clicking, etc. have
 platform-standard interpretations. Whatever the platform-specific
 interface, the user can always select disjoint sets of items or
 deselect items (and leave no items selected). On some platforms, the
 user can deselect the (sole) selected item in a @racket['single] list
 box.

@HVLabelNote[@racket[style]]{list box} @DeletedStyleNote[@racket[style] @racket[parent]]{list box}

If @racket[style] includes @racket['variable-columns], then the number
of columns in the list box can be changed via @method[list-box% append-column]
and @method[list-box% delete-column].

If @racket[selection] is an integer, it is passed to
@method[list-control<%> set-selection] to set the initial selection. The @racket[selection] must be less than
 the length of @racket[choices].

@FontLabelKWs[@racket[font] @racket[label-font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

It the @racket[column-order] argument is not @racket[#f], it
determines the order in which logical columns are initially displayed. See
@method[list-box% set-column-order] for more information. If
@racket[style] includes @racket['column-headers] and
@racket['reorderable-headers], then a user can reorder columns as
displayed (but the display order does not change the logical order of
the columns).

}


@defmethod[#:mode override
           (append [item label-string?]
                   [data any/c #f])
           void?]{

Adds a new item to the list box with an associated ``data'' object.
 The @racket[data] object is not displayed in the list box; it is
 provided merely as a convenience for use with @method[list-box%
 get-data], possibly allowing a programmer to avoid managing a
 separate item-to-data mapping in addition to the list box control.

See also @xmethod[list-control<%> append].

}


@defmethod[(append-column [label label-string?])
           void?]{

Adds a new column with title @racket[label] to the list box, but only
if the list box is created with the @racket['variable-columns]
style. The new column is logically the last column, and it is initially
displayed as the last column.}


@defmethod[(delete-column [n exact-nonnegative-integer?])
           void?]{

Deletes the column with logical position @racket[n], but only if the
list box is created with the @racket['variable-columns] style, and
only if the list box currently has more than one column (i.e., the
number of columns can never be zero).}


@defmethod[(get-column-labels) (cons/c label-string? (listof label-string?))]{

Returns the labels of the list box's columns, and the number of
returned strings indicates the number of columns in the list box.}


@defmethod[(get-column-order) (listof exact-nonnegative-integer?)]{

Returns the display order of logical columns. Each column is
represented by its logical position in the result list, and the order
of the column positions indicates the display order.

See also @method[list-box% set-column-order].}


@defmethod[(get-column-width [column exact-nonnegative-integer?])
           (values dimension-integer?
                   dimension-integer?
                   dimension-integer?)]{

Gets the width of the column identified by @racket[column] (in logical
positions, as opposed to display positions), which must be between 0
and one less than the number of columns.

The result includes the column's current width as well as its minimum
and maximum widths to constrain the column size as adjusted by a user.

See also @method[list-box set-column-width].}


@defmethod[(get-data [n exact-nonnegative-integer?])
           any/c]{

Returns the data for the item indexed by @racket[n], or @racket[#f]
 if there is no associated data. @|lbnumnote| If
 @racket[n] is equal to or larger than the number of choices,
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

For single-selection lists, the result is always either @racket[null] or
 a list containing one number.

}


@defmethod[(is-selected? [n exact-nonnegative-integer?])
           boolean?]{

Returns @racket[#t] if the items indexed by @racket[n] is selected,
 @racket[#f] otherwise. @|lbnumnote| If @racket[n] is equal to or
 larger than the number of choices, @|MismatchExn|.


@MonitorCallback[@elem{A list box's selection} @elem{the user clicking the control} @elem{selection}]

}

@defmethod[(number-of-visible-items)
           exact-positive-integer?]{

Returns the maximum number of items in the list box that are visible
 to the user with the control's current size (rounding down if the
 exact answer is fractional, but returning at least @racket[1]).

}

@defmethod[(select [n exact-nonnegative-integer?]
                   [select? any/c #t])
           void?]{

Selects or deselects an item. For selection in a single-selection list
 box, if a different choice is currently selected, it is automatically
 deselected. For selection in a multiple-selection list box, other
 selections are preserved, unlike
@method[list-control<%> set-selection].

If @racket[select?] is @racket[#f], the item indexed by @racket[n] is
 deselected; otherwise it is selected. @|lbnumnote| If @racket[n] is
 equal to or larger than the number of choices, @|MismatchExn|.

@MonitorCallback[@elem{A list box's selection} @elem{the user clicking the control} @elem{selection}]

The control's callback procedure is @italic{not} invoked.

}


@defmethod[(set [choices0 (listof label-string?)]
                [choices (listof label-string?)]
                ...)
           void?]{

Clears the list box and installs a new list of items. The number of
@racket[choices0] plus @racket[choices] lists must match the number of columns, and all
@racket[choices] lists must have the same number of items, otherwise
@|MismatchExn|.}


@defmethod[(set-column-label [column exact-nonnegative-integer?]
                             [label label-string?])
            void?]{

Sets the label of the column identified by @racket[column] (in logical
positions, as opposed to display positions), which must be between 0
and one less than the number of columns.}


@defmethod[(set-column-order [column-order (listof exact-nonnegative-integer?)])
            void?]{

Sets the order in which logical columns are displayed. Each element of
@racket[column-order] must identify a unique column by its logical
position, and all logical columns must be represented in the list.

See also @method[list-box% get-column-order].}


@defmethod[(set-column-width [column exact-nonnegative-integer?]
                             [width dimension-integer?]
                             [min-width dimension-integer?]
                             [max-width dimension-integer?])
            void?]{

Sets the width of the column identified by @racket[column] (in logical
positions, as opposed to display positions), which must be between 0
and one less than the number of columns.

The @racket[width] argument sets the current display width, while
@racket[min-width] and @racket[max-width] constrain the width of the
column when the user resizes it. The @racket[width] argument must be
no less than @racket[min-width] and no more than @racket[max-width].

The default width of a column is platform-specific, and the last
column of a list box may extend to the end of the control independent
of its requested size.

See also @method[list-box% get-column-width].}


@defmethod[(set-data [n exact-nonnegative-integer?]
                     [data any/c])
           void?]{

Sets the associated data for item indexed by @racket[n]. @|lbnumnote| If
 @racket[n] is equal to or larger than the number of choices,
 @|MismatchExn|.

See also @method[list-box% append].

}


@defmethod[(set-first-visible-item [n exact-nonnegative-integer?])
           void?]{

Scrolls the list box so that the item indexed by @racket[n] is at the
 top of the list box display. @|lbnumnote| If @racket[n] is equal to
 or larger than the number of choices, @|MismatchExn|.

@Unmonitored[@elem{A list box's scroll position} @elem{the user clicking the control} @elem{the scroll position
 changes} @elem{@method[list-box% get-first-visible-item]}]

}


@defmethod[(set-string [n exact-nonnegative-integer?]
                       [label label-string?]
                       [column exact-nonnegative-integer? 0])
           void?]{

Sets the item indexed by @racket[n] in logical column @racket[column]. 
@|lbcnumnote| If @racket[n] is
equal to or larger than the number of choices, or if @racket[column]
is equal to or larger than the number of columns, @|MismatchExn|.

}
}
