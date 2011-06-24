#lang scribble/doc
@(require "common.rkt")

@definterface/title[menu-item-container<%> ()]{

A @racket[menu-item-container<%>] object is a @racket[menu%],
 @racket[popup-menu%], or @racket[menu-bar%].


@defmethod[(get-items)
           (listof (is-a?/c menu-item<%>))]{
Returns a list of the items in the menu, popup menu, or menu bar. The
 order of the items in the returned list corresponds to the order as
 the user sees them in the menu or menu bar.

}


@defmethod[(on-demand)
           void?]{
@methspec{

Called when the user clicks on the container as a menu bar (before the
 user sees any menu items), just before the container as a popup menu
 is popped up, or just before inspecting the menu bar containing the
 item for a shortcut key binding.

If the container is not a menu bar or a popup menu, this method is
 normally called via the @method[menu-item-container<%> on-demand]
 method of the container's owning menu bar or popup menu, because the
 default implementation of the method chains to the
 @method[labelled-menu-item<%> on-demand] method of its
 items. However, the method can be overridden in a container such that
 it does not call the @method[labelled-menu-item<%> on-demand] method
 of its items.

}
@methimpl{

Calls the @racket[demand-callback] procedure that was provided when
 the object was created, then calls the @method[labelled-menu-item<%>
 on-demand] method of the contained items.

}}}
