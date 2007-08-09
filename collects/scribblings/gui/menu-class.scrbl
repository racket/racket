#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[menu% object% (menu-item-container<%> labelled-menu-item<%>)]{

A @scheme[menu%] object is a submenu within a @scheme[menu%] or
 @scheme[popup-menu%], or as a top-level menu in a
 @scheme[menu-bar%].




@defconstructor[[label label-string?]
                [parent @scheme[menu%], @scheme[popup-menu%], or @scheme[menu-bar%] object]
                [help-string (or/c label-string? false/c) #f]
                [demand-callback procedure of one argument: a @scheme[menu%] object @scheme[void]]]{

Creates a new menu with the given label.

If @scheme[label] contains an ampersand (``\&''), it is handled
 specially; under Windows, the character following an ampersand is
 underlined in the displayed menu title to indicate a keyboard
 mnemonic.  Pressing and releasing the Alt key switches to
 menu-selection mode in the menu bar where mnemonic characters are
 used for navigation.  An Alt combination might select a specific menu
 via @method[frame% on-menu-char].  A double-ampersand in
 @scheme[label] is replaced by a literal (non-navigation)
 ampersand. Under X and Mac OS X, ampersands in the label are parsed
 in the same way as for Windows, but no mnemonic underline is
 displayed.

If @scheme[help] is not @scheme[#f], the menu has a help string. See
@method[labelled-menu-item<%> get-help-string] for more information.

The @scheme[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

\index{``About'' boxes} \index{``Help'' menus}
If the menu has the label ``Help'' in a menu bar, it is treated
 specially on some platforms. Under Mac OS X, the items of a
 ``Help'' menu are folded into the standard help menu. In addition,
 under Mac OS X, if the name of the first item in the ``Help''
 menu starts with ``About'', then the menu item is duplicated as the
 first item under the Apple menu.



}}

