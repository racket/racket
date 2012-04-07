#lang scribble/doc
@(require "common.rkt")

@definterface/title[selectable-menu-item<%> (labelled-menu-item<%>)]{

A @racket[selectable-menu-item<%>] object is a
 @racket[labelled-menu-item<%>] that the user can select. It may also
 have a keyboard shortcut; the shortcut is displayed in the menu, and
 the default @method[frame% on-subwindow-char] method in the menu's
 frame dispatches to the menu item when the shortcut key combination
 is pressed.


@defmethod[(command [event (is-a?/c control-event%)])
           void?]{

Invokes the menu item's callback procedure, which is supplied when an
 instance of
@racket[menu-item%] or
@racket[checkable-menu-item%] is created.

}

@defmethod[(get-shortcut)
           (or/c char? symbol? #f)]{

Gets the keyboard shortcut character or virtual key for the menu
 item. This character or key is combined with the shortcut prefix,
 which is reported by @method[selectable-menu-item<%>
 get-shortcut-prefix].

If the menu item has no shortcut, @racket[#f] is returned.

The shortcut part of a menu item name is not included in the label
 returned by @method[labelled-menu-item<%> get-label].

For a list of allowed key symbols, see @xmethod[key-event%
 get-key-code], except that the following are disallowed: 
 @racket['shift], @racket['control], @racket['numlock],
 @racket['scroll], @racket['wheel-up], @racket['wheel-down],
 @racket['release], and @racket['press].

}

@defmethod[(get-shortcut-prefix)
           (listof (or/c 'alt 'cmd 'meta 'ctl 'shift 'option))]{

Returns a list of symbols that indicates the keyboard prefix used for the menu
 item's keyboard shortcut. The allowed symbols for the list are the following:

@itemize[
@item{@racket['alt] --- Meta (Windows and X only)}
@item{@racket['cmd] --- Command (Mac OS X only)}
@item{@racket['meta] --- Meta (Unix only)}
@item{@racket['ctl] --- Control}
@item{@racket['shift] --- Shift}
@item{@racket['option] --- Option (Mac OS X only)}
]

On Unix, at most one of @racket['alt] and @racket['meta] can be
 supplied; the only difference between @racket['alt] and
 @racket['meta] is the key combination's display in a menu.

The default shortcut prefix is available from
 @racket[get-default-shortcut-prefix].

The shortcut key, as determined by @method[selectable-menu-item<%>
 get-shortcut], matches a key event using either the normally reported
 key code or the other-Shift/AltGr key code (as produced by
 @xmethod[key-event% get-other-shift-key-code], etc.). When the
 shortcut key is a key-code symbol or an ASCII letter or digit, then
 the shortcut matches only the exact combination of modifier keys
 listed in the prefix. For character shortcuts other than ASCII
 letters and digits, however, then the shortcut prefix merely
 determines a minimum set of modifier keys, because additional
 modifiers may be needed to access the character; an exception is
 that, on Windows or Unix, the Alt/Meta key press must match the
 prefix exactly (i.e., included or not). In all cases, the most
 precise match takes precedence; see @xmethod[keymap% map-function]
 for more information on match ranking.

An empty list can be used for a shortcut prefix. However, the default
 @xmethod[frame% on-menu-char] method checks for menu shortcuts only
 when the key event includes either a non-Shift modifier or a Function
 key. Thus, an empty shortcut prefix is normally useful only if the
 shortcut key is a Function key.

}


@defmethod[(set-shortcut [shortcut (or/c char? symbol? #f)])
           void?]{

Sets the keyboard shortcut character for the menu item. See
@method[selectable-menu-item<%> get-shortcut] for more information.

If the shortcut character is set to @racket[#f], then menu item has no
keyboard shortcut.

}

@defmethod[(set-shortcut-prefix [prefix (listof (or/c 'alt 'cmd 'meta 'ctl 'shift 'option))])
           void?]{

Sets a list of symbols to indicates the keyboard prefix used for the
menu item's keyboard shortcut.

See @method[selectable-menu-item<%> get-shortcut-prefix] for more
information.

}}

