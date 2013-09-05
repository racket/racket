#lang scribble/doc
@(require "common.rkt")

@definterface/title[labelled-menu-item<%> (menu-item<%>)]{

A @racket[labelled-menu-item<%>] object is a @racket[menu-item<%>] with
 a string label (i.e., any menu item other than a separator).  More
 specifically, it is an instance of either @racket[menu-item%] (a
 plain menu item), @racket[checkable-menu-item%] (a checkable menu
 item), or @racket[menu%] (a submenu).


@defmethod[(enable [enabled? any/c])
           void?]{

Enables or disables the menu item. If the item is a submenu (or menu
 in a menu bar), the entire menu is disabled, but each submenu item's
 @method[labelled-menu-item<%> is-enabled?] method returns @racket[#f]
 only if the item is specifically disabled (in addition to the
 submenu).

}

@defmethod[(get-help-string)
           (or/c label-string? #f)]{

Returns the help string for the menu item, or @racket[#f] if the item
 has no help string.

When an item has a @racket[help], the string may be used to
 display help information to the user.

}

@defmethod[(get-label)
           label-string?]{

Returns the item's label.

See also @method[labelled-menu-item<%> set-label] and
@method[labelled-menu-item<%> get-plain-label].


}

@defmethod[(get-plain-label)
           label-string?]{

Like @method[labelled-menu-item<%> get-label], except that
@litchar{&}s and tab characters in the label are stripped in
the same way as for @method[window<%> set-label].
}

@defmethod[(is-enabled?)
           boolean?]{

Returns @racket[#t] if the menu item is enabled, @racket[#f]
otherwise.

See also
@method[labelled-menu-item<%> enable].

}

@defmethod[(on-demand)
           void?]{
@methspec{

Normally called when the user clicks on the menu bar containing the
 item (before the user sees any menu items), just before the popup
 menu containing the item is popped up, or just before inspecting the
 menu bar containing the item for a shortcut key binding.
 See @xmethod[menu-item-container<%> on-demand] for further details.

A @xmethod[menu-item-container<%> on-demand] method can be overridden
in such a way that the container does not call the
@method[labelled-menu-item<%> on-demand] method of its items.

}
@methimpl{

Calls the @racket[demand-callback] procedure that was provided when the
 object was created.

}}

@defmethod[(set-help-string [help (or/c label-string? #f)])
           void?]{

Sets the help string for the menu item. Use @racket[#f] to remove the
 help string for an item.

}

@defmethod[(set-label [label label-string?])
           void?]{

Sets the menu item's label. If the item has a shortcut, the shortcut
 is not affected.

If the label contains @litchar{&} and the window is a control, the
 label is parsed specially; on Windows and Unix, the character
 following a @litchar{&} is underlined in the displayed menu to
 indicate a keyboard mnemonic. Pressing the Alt key with an underlined
 character from a menu's name in the menu bar causes the menu to be
 selected (via @method[frame% on-menu-char]). When a menu has the
 focus, the mnemonic characters are used for navigation without Alt. A
 @litchar{&&} in the label is replaced by a literal (non-navigation)
 @litchar{&}. On Mac OS X, @litchar{&}s in the label are parsed in
 the same way as for Unix and Windows, but no mnemonic underline is
 displayed. On Mac OS X, a parenthesized mnemonic character is
 removed (along with any surrounding space) before the label is
 displayed, since a parenthesized mnemonic is often used for non-Roman
 languages. Finally, for historical reasons, if a label contains a tab character, then the
 tab and all remaining characters are hidden in the displayed menu.
 All of these rules are consistent with label handling in @racket[button%]
 and other windows.

A @litchar{&} is always preserved in the label returned by
 @method[labelled-menu-item<%> get-label], but never preserved in the
 label returned by @method[labelled-menu-item<%> get-plain-label].

}}

