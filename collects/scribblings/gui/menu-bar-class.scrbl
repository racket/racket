#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[menu-bar% object% (menu-item-container<%>)]{

A @scheme[menu-bar%] object is created for a particular
 @scheme[frame%] object. A frame can have at most one menu bar;
 @|MismatchExn|} when a new menu bar is created for a frame that
 already has a menu bar.



@defconstructor[[parent @scheme[frame%] object or @scheme['root]]
                [demand-callback procedure of one argument: a @scheme[menu-bar%] object @scheme[void]]]{

Creates a menu bar in the specified frame. The menu bar is initially
 empty. If @indexed-scheme['root] is supplied as @scheme[parent], the menu
 bar becomes active only when no other frames are shown. A
 @scheme['root] @scheme[parent] is allowed only when
 @scheme[current-eventspace-has-menu-root?] returns
 @scheme[#t], and only if no such menu bar has been created before,
 otherwise @|MismatchExn|}.

The @scheme[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.



}

@defmethod[(get-frame)
           (is-a/c frame%)]{
@spec{

Returns the menu bar's frame.

}}

@defmethod[(enable [enable? any/c])
           void?]{
@spec{

Enables or disables the menu bar (i.e., all of its menus).  Each
 menu's
@method[labelled-menu-item<%> is-enabled?] method returns @scheme[#f] only if the menu is specifically disabled (in
 addition to the menu bar).

}}

@defmethod[(is-enabled?)
           boolean?]{
@spec{

Returns @scheme[#t] if the menu bar is enabled, @scheme[#f] otherwise.

}
@impl{




}}}

