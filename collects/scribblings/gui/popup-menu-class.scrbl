#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[popup-menu% object% (menu-item-container<%>)]{

A @scheme[popup-menu%] object is created without a
 parent. Dynamically display a @scheme[popup-menu%] with
,
@xmethod[window<%> popup-menu], or
@xmethod[editor-admin% popup-menu].

A popup menu is {\em not} a control. A @scheme[choice%] control,
 however, displays a single value that the user selects from a popup
 menu. A @scheme[choice%] control's popup menu is built into the
 control, and it is not accessible to the programmer.




@defconstructor[[title (or/c label-string? false/c) #f]
                [popdown-callback procedure of two arguments: a @scheme[popup-menu%] object and a @scheme[control-event%] object @scheme[(\scmk{lambda] (@scheme[m] @scheme[e]) (void))}]
                [demand-callback procedure of one argument: a @scheme[popup-menu%] object @scheme[void]]
                [font (is-a/c font%) @scheme[normal-control-font]]]{

If @scheme[title] is not @scheme[#f], it is used as a displayed title at
 the top of the popup menu.

If @scheme[title] contains an ampersand (``\&''), it is handled
 specially, the same as for @scheme[menu%] titles. A popup menu
 mnemonic is not useful, but it is supported for consistency with
 other menu labels.

The @scheme[popdown-callback] procedure is invoked when a popup menu is
 dismissed. If the popup menu is dismissed without an item being
 selected, @scheme[popdown-callback] is given a @scheme[control-event%]
 object with the event type @indexed-scheme['menu-popdown-none]. If the
 popup menu is dismissed via an item selection, the item's callback is
 invoked first, and then @scheme[popdown-callback] is given a
 @scheme[control-event%] object with the event type
 @indexed-scheme['menu-popdown].

The @scheme[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

The @scheme[font] argument determines the font for the popup menu's
 items.



}

@defmethod[(get-popup-target)
           (or/c (or/c (is-a/c window<%>) (is-a/c editor<%>)) false/c)]{
@spec{

Returns the context in which the popup menu is currently displayed, or
 @scheme[#f] if it is not popped up in any window.

The context is set before the
@method[menu-item-container<%> on-demand] method is called, and it is not removed until after the popup-menu's
 callback is invoked. (Consequently, it is also set while an item
 callback is invoked, if the user selected an item.)

}}

@defmethod[(get-font)
           (is-a/c font%)]{
@spec{

Returns the font used for the popup menu's items, which is optionally
 supplied when a popup menu is created.

}}

@defmethod[(set-min-width [width (integer-in 0 10000)])
           void?]{
@spec{

Sets the popup menu's minimum width in pixels.

}
@impl{



}}}

