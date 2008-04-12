#lang scribble/doc
@(require "../common.ss"
          (for-label mrlib/hierlist))

@defclass/title[hierarchical-list% editor-canvas% ()]{

Creates a hierarchical-list control.


@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'no-border 'control-border 'combo 
                                          'no-hscroll 'no-vscroll 
                                          'hide-hscroll 'hide-vscroll 
                                          'auto-vscroll 'auto-hscroll 
                                          'resize-corner 'deleted 'transparent))
                        '(no-hscroll)])]{

Creates the control.}


@defmethod[(selected) (or/c (is-a?/c hierarchical-list-item<%>)
                            false/c)]{

Returns the currently selected item, if any.}


@defmethod[(new-item [mixin ((implementation?/c hierarchical-list-item<%>)
                             . -> .
                             (implementation?/c hierarchical-list-item<%>))
                             (lambda (%) %)])
           (is-a?/c hierarchical-list-item<%>)]{

Creates and returns a new (empty) item in the list. See
@scheme[hierarchical-list-item<%>] for methods to fill in the item's
label.

The @scheme[mixin] argument is applied to a class implementing
@scheme[hierarchical-list-item<%>], and the resulting class is
instantiated as the list item.}


@defmethod[(set-no-sublists [no-sublists? any/c]) void?]{

Enables/disables sublist mode. When sublists are disabled, space to
the left of the list items (that would normally align non-list items
with list items) is omitted. This method can be called only when the
list is empty.}


@defmethod[(new-list [mixin ((implementation?/c hierarchical-list-compound-item<%>)
                             . -> .
                             (implementation?/c hierarchical-list-compound-item<%>))
                             (lambda (%) %)])
           (is-a?/c hierarchical-list-compound-item<%>)]{

Creates and returns a new (empty) sub-list in the list. See
@scheme[hierarchical-list-compound-item<%>] for methods to fill in the
item's label and content.

The @scheme[mixin] argument is applied to a class implementing
@scheme[hierarchical-list-compound-item<%>], and the resulting class
is instantiated as the sub-list.}


@defmethod[(delete-item [i (is-a?/c hierarchical-list-item<%>)]) void?]{

Deletes immediate item or sub-list @scheme[i] from the list.}


@defmethod[(get-items) (listof (is-a?/c hierarchical-list-item<%>))]{

Returns a list of all immediate items in the list control.}


@defmethod*[([(selectable) boolean?]
             [(selectable [on? any/c]) void?])]{

Reports whether items are selectable, or enables/disables item
selection.}


@defmethod[(on-select [i (or/c (is-a?/c hierarchical-list-item<%>) false/c)]) any]{

Called for new select of @scheme[i], where @scheme[i] is @scheme[#f]
if no item is now selected.}


@defmethod[(on-click [i (is-a?/c hierarchical-list-item<%>)]) any]{

Called when an item is clicked on, but selection for that item is not
allowed. Selection can be disallowed by @method[hierarchical-list%
selectable] or @xmethod[hierarchical-list-item<%>
set-allow-selection].}


@defmethod[(on-double-select [i (is-a?/c hierarchical-list-item<%>)]) any]{

Called for a double-click on @scheme[i].}


@defmethod[(on-item-opened [i (is-a?/c hierarchical-list-compound-item<%>)]) any]{

Called when the arrow for @scheme[i] is turned down.}


@defmethod[(on-item-closed [i (is-a?/c hierarchical-list-compound-item<%>)]) any]{

Called when the arrow for @scheme[i] is turned up.}


@defmethod[(sort [less-than-proc ((is-a?/c hierarchical-list-item<%>)
                                  (is-a?/c hierarchical-list-item<%>)
                                  . -> . any/c)]
                 [recur? any/c #t])
           void?]{

Sorts items in the list by calling @scheme[less-than-proc] on pairs of
items. If @scheme[recur?] is true, items in sub-lists are sorted
recursively.}


@defmethod[(can-do-edit-operation? [op symbol?] [recursive? any/c #t])
           boolean?]{

Like @xmethod[editor<%> can-do-edit-operation?]. The default
implementation always returns @scheme[#f].}


@defmethod[(do-edit-operation [op symbol?] [recursive? any/c #t])
           void?]{

Like @xmethod[editor<%> do-edit-operation]. The default implementation
does nothing.}


@defmethod*[([(select-prev) void?]
             [(select-next) void?]
             [(select-first) void?]
             [(select-last) void?]
             [(select-in) void?]
             [(select-out) void?]
             [(page-up) void?]
             [(page-down) void?])]{

Move the selection, scroll, and call @method[hierarchical-list
on-select].}


@defmethod[(select [i (or/c (is-a?/c hierarchical-list-item<%>) false/c)]) void?]{

Moves the selection, scrolls as necessary to show it, and calls
@method[hierarchical-list% on-select] unless disabled via
@method[hierarchical-list% on-select-always].

The @method[hierarchical-list% allow-deselect] method controls whether
@scheme[i] is allowed to be @scheme[#f] to deselect the currently
selected item.}


@defmethod[(click-select [i (or/c (is-a?/c hierarchical-list-item<%>) false/c)]) void?]{

Like @method[hierarchical-list% select], but always calls
@method[hierarchical-list% on-select].}


@defmethod*[([(on-select-always) boolean?]
             [(on-select-always [always? any/c]) void?])]{

Gets/sets whether the @method[hierarchical-list% on-select] method is
called in response to @method[hierarchical-list% select] (as opposed
to @method[hierarchical-list% click-select]).

The initial mode enables @method[hierarchical-list% on-select] calls
always.}


@defmethod*[([(allow-deselect) boolean?]
             [(allow-deselect [allow? any/c]) void?])]{


Gets/sets whether the @method[hierarchical-list% on-select] can be
called with a @scheme[#f] argument to deselect the current item
(leaving none selected).

The initial mode does not allow deselection.}

}
