#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[list-control<%> (control<%>)]{

A list control gives the user a list of string items to choose from.
 There are two built-in classes that implement
 @scheme[list-control<%>]: 
@itemize{

 @item{@scheme[choice%] --- presents the list in a popup menu (so
 the user can choose only one item at a time)}

 @item{@scheme[list-box%] --- presents the list in a scrolling box,
 allowing the use to choose one item (if the style includes
 @scheme['single]) or any number of items}

}
In either case, the set of user-selectable items can be changed
 dynamically.




@defmethod[(append [item string])
           void?]{
@spec{

Adds a new item to the list of user-selectable items. The current
 selection is unchanged (unless the list control is an empty choice
 control, in which case the new item is selected).

}}

@defmethod[(clear)
           void?]{
@spec{

Removes all user-selectable items from the control.

}}

@defmethod[(find-string [s string])
           (or/c nonnegative-exact-integer? false/c)]{
@spec{

Finds a user-selectable item matching the given string. If no matching
 choice is found, @scheme[#f] is returned, otherwise the index of the
 matching choice is returned (items are indexed from @scheme[0]).

}}

@defmethod[(get-selection)
           (or/c nonnegative-exact-integer? false/c)]{
@spec{

Returns the index of the currently selected item (items are indexed
 from @scheme[0]). If the choice item currently contains no choices or no
 selections, @scheme[#f] is returned.  If multiple selections are
 allowed and multiple items are selected, the index of the first
 selection is returned.

}}

@defmethod[(get-string-selection)
           (or/c immutable {\labelstring} false/c)]{
@spec{

Returns the currently selected item.  If the control currently
 contains no choices, @scheme[#f] is returned. If multiple selections
 are allowed and multiple items are selected, the first selection is
 returned.

}}

@defmethod[(get-number)
           nonnegative-exact-integer?]{
@spec{

Returns the number of user-selectable items in the control (which is
 also one more than the greatest index in the list control).

}}

@defmethod[(set-selection [n nonnegative-exact-integer?])
           void?]{
@spec{

Selects the item specified by the given index (items are indexed from
 @scheme[0]). If the given index larger than the greatest index in the
 list control, @|MismatchExn|}.

In a list box control, all other items are deselected, even if multiple
 selections are allowed in the control. See also
@xmethod[list-box% select].

The control's callback procedure is {\em not} invoked when this method
is called.

@MonitorCallback[@elem{The list control's selection} @elem{the user clicking the control} @elem{selection}]

}}

@defmethod[(set-string-selection [s string])
           void?]{
@spec{

Selects the item that matches the given string.  If no match
 is found in the list control, @|MismatchExn|}.

In a list box control, all other items are deselected, even if multiple
 selections are allowed in the control. See also
@xmethod[list-box% select].

The control's callback procedure is {\em not} invoked when this method
is called.

@MonitorCallback[@elem{The list control's selection} @elem{the user clicking the control} @elem{selection}]

}}

@defmethod[(get-string [n nonnegative-exact-integer?])
           immutable {\labelstring}]{
@spec{

Returns the item for the given index (items are indexed from
 @scheme[0]). If the provided index is larger than the greatest index in
 the list control, @|MismatchExn|}.

}
@impl{




}}}

