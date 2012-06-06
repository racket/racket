#lang scribble/doc
@(require "../common.rkt" (for-label mrlib/hierlist))

@definterface/title[hierarchical-list-item<%> ()]{

Instantiate this interface via @method[hierarchical-list% new-item].

@defmethod[(get-editor) (is-a?/c text%)]{

Returns a text-editor buffer whose content is the display
representation of the item. In other words, fill in this text editor
to set the item's label.}


@defmethod[(is-selected?) boolean?]{

Reports whether the item is selected.}


@defmethod*[([(select [on? any/c]) void?]
             [(click-select [on? any/c]) void?])]{

Calls @method[hierarchical-list% select] or @method[hierarchical-list%
click-select]. The @racket[on?] argument can be @racket[#f] only if
@xmethod[hierarchical-list% allow-deselect] allows it.}


@defmethod*[([(user-data) any/c]
             [(user-data [data any/c]) void?])]{

Gets/sets arbitrary data associated with the item.}


@defmethod[(get-clickable-snip) (is-a?/c snip%)]{

Returns the snip that (when clicked) selects this element the
list. This method is intended for use with an automatic test suite.}


@defmethod*[([(get-allow-selection?) boolean?]
             [(set-allow-selection [allow? any/c]) void?])]{

Gets/sets whether this item is allowed to be selected.}


@defmethod[(get-parent) (or/c (is-a?/c hierarchical-list-compound-item<%>) #f)]{

Returns the compound list item that contains the item or @racket[#f] if
none exists.}

}
