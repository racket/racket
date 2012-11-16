#lang scribble/doc
@(require "common.rkt")

@definterface/title[list-control<%> (control<%>)]{

A list control gives the user a list of string items to choose from.
 There are two built-in classes that implement
 @racket[list-control<%>]: 
@itemize[

 @item{@racket[choice%] --- presents the list in a popup menu (so
 the user can choose only one item at a time)}

 @item{@racket[list-box%] --- presents the list in a scrolling box,
 allowing the use to choose one item (if the style includes
 @racket['single]) or any number of items}

]
In either case, the set of user-selectable items can be changed
 dynamically.




@defmethod[(append [item label-string?])
           void?]{
Adds a new item to the list of user-selectable items. The current
 selection is unchanged (unless the list control is an empty choice
 control, in which case the new item is selected).

}

@defmethod[(clear)
           void?]{
Removes all user-selectable items from the control.

}

@defmethod[(delete [n exact-nonnegative-integer?])
           void?]{

Deletes the item indexed by @racket[n] (where items are indexed
 from @racket[0]). If @racket[n] is equal
 to or larger than the number of items in the control, @|MismatchExn|.

Selected items that are not deleted remain selected, and no other
 items are selected.}


@defmethod[(find-string [s string?])
           (or/c exact-nonnegative-integer? #f)]{
Finds a user-selectable item matching the given string. If no matching
 choice is found, @racket[#f] is returned, otherwise the index of the
 matching choice is returned (where items are indexed from @racket[0]).

}

@defmethod[(get-number)
           exact-nonnegative-integer?]{
Returns the number of user-selectable items in the control (which is
 also one more than the greatest index in the list control).

}

@defmethod[(get-selection)
           (or/c exact-nonnegative-integer? #f)]{
Returns the index of the currently selected item (where items are indexed
 from @racket[0]). If the choice item currently contains no choices or no
 selections, @racket[#f] is returned.  If multiple selections are
 allowed and multiple items are selected, the index of the first
 selection is returned.

}

@defmethod[(get-string [n exact-nonnegative-integer?])
           (and/c immutable? label-string?)]{

Returns the item for the given index (where items are indexed from
 @racket[0]). If the provided index is larger than the greatest index in
 the list control, @|MismatchExn|.

}

@defmethod[(get-string-selection)
           (or/c (and/c immutable? label-string?) #f)]{
Returns the currently selected item.  If the control currently
 contains no choices, @racket[#f] is returned. If multiple selections
 are allowed and multiple items are selected, the first selection is
 returned.

}

@defmethod[(set-selection [n exact-nonnegative-integer?])
           void?]{
Selects the item specified by the given index (where items are indexed from
 @racket[0]). If the given index larger than the greatest index in the
 list control, @|MismatchExn|.

In a list box control, all other items are deselected, even if multiple
 selections are allowed in the control. See also
@xmethod[list-box% select].

The control's callback procedure is @italic{not} invoked when this method
is called.

@MonitorCallback[@elem{The list control's selection} @elem{the user clicking the control} @elem{selection}]

}

@defmethod[(set-string-selection [s string?])
           void?]{
Selects the item that matches the given string.  If no match
 is found in the list control, @|MismatchExn|.

In a list box control, all other items are deselected, even if multiple
 selections are allowed in the control. See also
@xmethod[list-box% select].

The control's callback procedure is @italic{not} invoked when this method
is called.

@MonitorCallback[@elem{The list control's selection} @elem{the user clicking the control} @elem{selection}]

}

}

