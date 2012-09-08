#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/menu-bar}}

@defclass/title[menu-bar% object% (menu-item-container<%>)]{

A @racket[menu-bar%] object is created for a particular
 @racket[frame%] object. A frame can have at most one menu bar;
 @|MismatchExn| when a new menu bar is created for a frame that
 already has a menu bar.



@defconstructor[([parent (or/c (is-a?/c frame%) 'root)]
                 [demand-callback ((is-a?/c menu-bar%) . -> . any) (lambda (m) (void))])]{

Creates a menu bar in the specified frame. The menu bar is initially
 empty. If @indexed-racket['root] is supplied as @racket[parent], the
 menu bar becomes active only when no other frames are shown. A
 @racket['root] @racket[parent] is allowed only when
 @racket[current-eventspace-has-menu-root?] returns @racket[#t], and
 only if no such menu bar has been created before, otherwise
 @|MismatchExn|.

The @racket[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

}


@defmethod[(enable [enable? any/c])
           void?]{

Enables or disables the menu bar (i.e., all of its menus).  Each
 menu's @method[labelled-menu-item<%> is-enabled?] method returns
 @racket[#f] only if the menu is specifically disabled (in addition to
 the menu bar).

}


@defmethod[(get-frame)
           (is-a?/c frame%)]{

Returns the menu bar's frame.

}


@defmethod[(is-enabled?)
           boolean?]{

Returns @racket[#t] if the menu bar is enabled, @racket[#f] otherwise.

}}
