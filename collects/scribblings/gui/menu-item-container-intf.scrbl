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
 user sees any menu items, except with Unity's global menu bar as
 noted below), just before the container as a popup menu
 is popped up, or just before inspecting the menu bar containing the
 item for a shortcut key binding.

If the container is not a @tech{menu bar} or a @tech{popup menu}, this method is
 normally called via the @method[menu-item-container<%> on-demand]
 method of the container's owning menu bar or popup menu, because the
 default implementation of the method chains to the
 @method[labelled-menu-item<%> on-demand] method of its
 items. However, the method can be overridden in a container such that
 it does not call the @method[labelled-menu-item<%> on-demand] method
 of its items.

On Unix with the Unity window manager using the global menu bar (which
 is the default on Ubuntu), @racket[racket/gui/base] receives no
 notification when the user clicks the menu bar. To approximate
 @method[menu-item-container<%> on-demand] triggered by user clicks of
 the menu bar, @method[menu-item-container<%> on-demand] is called for
 a @tech{menu bar} whenever its @racket[frame%] object loses the
 keyboard focus. Beware that if keyboard focus was lost because a menu
 was clicked, then items added to the clicked menu during an
 @method[menu-item-container<%> on-demand] invocation may not appear
 for the user.

}
@methimpl{

Calls the @racket[demand-callback] procedure that was provided when
 the object was created, then calls the @method[labelled-menu-item<%>
 on-demand] method of the contained items.

}}}
