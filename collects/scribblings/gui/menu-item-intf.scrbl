#lang scribble/doc
@(require "common.rkt")

@definterface/title[menu-item<%> ()]{

A @racket[menu-item<%>] object is an element within a @racket[menu%],
 @racket[popup-menu%], or @racket[menu-bar%]. Operations that affect
 the parent --- such as renaming the item, deleting the item, or
 adding a check beside the item --- are accomplished via the
 @racket[menu-item<%>] object.

A menu item is either a @racket[separator-menu-item%] object (merely
 a separator), or a @racket[labelled-menu-item<%>] object; the latter
 is more specifically an instance of either @racket[menu-item%] (a
 plain menu item), @racket[checkable-menu-item%] (a checkable menu
 item), or @racket[menu%] (a submenu).


@defmethod[(delete)
           void?]{

Removes the item from its parent. If the menu item is already deleted,
@method[menu-item<%> delete] has no effect.

See also @method[menu-item<%> restore].


}

@defmethod[(get-parent)
           (or/c (is-a?/c menu%) (is-a?/c popup-menu%) (is-a?/c menu-bar%))]{

Returns the menu, popup menu, or menu bar containing the item. The
 parent for a menu item is specified when the menu item is created,
 and it cannot be changed.

}

@defmethod[(is-deleted?)
           boolean?]{

Returns @racket[#t] if the menu item is deleted from its parent,
 @racket[#f] otherwise.

}

@defmethod[(restore)
           void?]{

Adds a deleted item back into its parent. The item is always restored
 to the end of the parent, regardless of its original position. If the
 item is not currently deleted, @method[menu-item<%> restore] has no
 effect.

}}

