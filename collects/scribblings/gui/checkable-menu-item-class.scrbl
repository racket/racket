#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[checkable-menu-item% object% (selectable-menu-item<%>)]{

A @scheme[checkable-menu-item%] is a string-labelled menu item that
 maintains a check mark. Its parent must be a @scheme[menu%] or
 @scheme[popup-menu%]. When the user selects the menu item, the
 item's check mark is toggled and its callback procedure is called.




@defconstructor[[label label-string?]
                [parent @scheme[menu%] or @scheme[popup-menu%] object]
                [callback procedure of two arguments: a @scheme[menu-item%] object and a @scheme[control-event%] object]
                [shortcut (or/c character false/c) #f]
                [help-string (or/c label-string? false/c) #f]
                [demand-callback procedure of one argument: a @scheme[checkable-menu-item%] object @scheme[void]]
                [checked any/c #f]
                [shortcut-prefix (symbols/c option shift ctl meta cmd alt) @scheme[(\iscmprocedure{get-default-shortcut-prefix])}]]{

Creates a new menu item in @scheme[parent]. The item is initially shown,
 appended to the end of its parent, and unchecked. The @scheme[callback]
 procedure is called (with the event type @indexed-scheme['menu]) when the
 menu item is selected (either via a menu bar,
@xmethod[window<%> popup-menu], or
@xmethod[editor-admin% popup-menu]).

See
@method[labelled-menu-item<%> set-label] for information about mnemonic ampersands (``\&'') in @scheme[label].

If @scheme[shortcut] is not @scheme[#f], the item has a shortcut. See
@method[selectable-menu-item<%> get-shortcut] for more information.  The @scheme[shortcut-prefix] argument determines the
set of modifier keys for the shortcut; see
@method[selectable-menu-item<%> get-shortcut-prefix].

If @scheme[help] is not @scheme[#f], the item has a help string. See
@method[labelled-menu-item<%> get-help-string] for more information.

The @scheme[demand-callback] procedure is called by the default
@method[labelled-menu-item<%> on-demand] method with the object itself.

By default, the menu item is initially unchecked. If @scheme[checked] is
 true, then
@method[checkable-menu-item% check] is called so that the menu item is initially checked.



}

@defmethod[(check [check? any/c])
           void?]{
@spec{

Checks or unchecks the menu item.

@MonitorCallbackX[@elem{A menu item's check state} @elem{the user selecting the item} @elem{check state} @elem{menu item}]

}}

@defmethod[(is-checked?)
           boolean?]{
@spec{

Returns @scheme[#t] if the item is checked, {\#f} otherwise.

}
@impl{



}}}

