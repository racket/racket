#lang scribble/doc
@(require "common.rkt")
@(tools-title "frame")

@defclass[drracket:frame:name-message% canvas% ()]{

This class implements the little filename button in the top-right hand
side of DrRacket's frame.



@defconstructor/make[([parent (is-a?/c area-container<%>)])]{}

@defmethod[(set-message [name (or/c string? false/c)]
                        [short-name string?])
           void?]{
@methspec{

Sets the names that the button shows.

}
@methimpl{

The string @racket[short-name] is the name that is shown on the button
and @racket[name] is shown when the button is clicked on, in a separate
window. If @racket[name] is @racket[#f], a message indicating that the file
hasn't been saved is shown.



}}}


@defmixin[drracket:frame:mixin (drracket:frame:basics<%> frame:text-info<%> frame:editor<%>) (drracket:frame:<%>)]{

Provides an implementation of 
@racket[drracket:frame:<%>] 
}


@defmixin[drracket:frame:basics-mixin (frame:standard-menus<%>) (drracket:frame:basics<%>)]{

Use this mixin to establish some common menu items across various DrRacket windows.



@defmethod[#:mode override 
           (edit-menu:between-find-and-preferences)
           void?]{

Adds a
@racket[separator-menu-item%]. Next, adds the
@racket["Keybindings"] menu item to the edit menu. Finally,
if the 
@racket[current-eventspace-has-standard-menus?] procedure returns @racket[#f], creates another 
@racket[separator-menu-item%].



}

@defmethod[#:mode override 
           (file-menu:between-open-and-revert [file-menu (is-a?/c menu%)])
           void?]{

Adds an ``Install .plt File...'' menu item, which
downloads and installs .plt files from the web, or installs
them from the local disk. After that, calls the super
method.


}

@defmethod[#:mode override 
           (file-menu:between-print-and-close [file-menu (is-a?/c menu%)])
           void?]{

Calls the super method. Then, creates a menu item for
multi-file searching. Finally,
adds a
@racket[separator-menu-item%].


}

@defmethod[#:mode override 
           (file-menu:new-callback [item (is-a?/c menu-item%)]
                                   [evt (is-a?/c control-event%)])
           void?]{

Opens a new, empty DrRacket window.


}

@defmethod[#:mode override 
           (file-menu:new-string)
           string?]{

Returns the empty string.


}

@defmethod[#:mode override 
           (file-menu:open-callback [item (is-a?/c menu-item%)]
                                    [evt (is-a?/c control-event%)])
           void?]{

Calls 
@racket[handler:edit-file].


}

@defmethod[#:mode override 
           (file-menu:open-string)
           string?]{

Returns the empty string.


}

@defmethod[(get-additional-important-urls)
           (listof (list string string))]{
@methspec{

Each string in the result of this method is added as a menu
item to DrRacket's ``Related Web Sites'' menu item. The
first string is the name of the menu item and the second
string is a url that, when the menu item is chosen, is sent
to the user's browser.

}
@methimpl{

Returns the empty list by default.


}}

@defmethod[#:mode override 
           (help-menu:about-callback [item (is-a?/c menu-item%)]
                                     [evt (is-a?/c control-event%)])
           void?]{

Opens an about box for DrRacket.


}

@defmethod[#:mode override 
           (help-menu:about-string)
           string?]{

Returns the string @racket["DrRacket"].


}

@defmethod[#:mode override 
           (help-menu:before-about [help-menu (is-a?/c menu%)])
           void?]{

Adds the Help Desk menu item and the Welcome to DrRacket menu item.
}

@defmethod[#:mode override 
           (help-menu:create-about?)
           boolean?]{

Returns @racket[#t].


}}


@definterface[drracket:frame:basics<%> (frame:standard-menus<%>)]{

This interface is the result of the @racket[drracket:frame:basics-mixin]

}


@definterface[drracket:frame:<%> (frame:editor<%> frame:text-info<%> drracket:frame:basics<%>)]{



@defmethod[(add-show-menu-items [show-menu (is-a?/c menu%)])
           void?]{
@methspec{

This method is called during the construction of the view
menu.  This method is intended to be overridden. It is
expected to add other Show/Hide menu items to the show menu.

See also
@method[drracket:frame:<%> get-show-menu].

}
@methimpl{

Does nothing.



}}

@defmethod[(get-show-menu)
           (is-a?/c menu%)]{
@index{View menu}

returns the view menu, for use by the
@method[drracket:frame:<%> update-shown] method.

See also
@method[drracket:frame:<%> add-show-menu-items].

The method (and others) uses the word @tt{show} to preserve
backwards compatibility from when the menu itself was named
the Show menu.

}

@defmethod[(not-running)
           void?]{
updates the status pane at the bottom of the window to show
that evaluation is not taking place in the user's program.

}

@defmethod[(running)
           void?]{
updates the status pane at the bottom of the window to show
that evaluation is taking place in the user's program.

}

@defmethod[(update-shown)
           void?]{
@methspec{

This method is intended to be overridden. It's job is to
update the @racket["View"] menu to match the state of the
visible windows. In the case of the standard DrRacket
window, it change the menu items to reflect the visibility of
the definitions and interaction @racket[editor-canvas%]s.

Call this method whenever the state of the show menu might
need to change.

See also
@method[drracket:frame:<%> get-show-menu].

}
@methimpl{

Does nothing.


}}}

@(tools-include "frame")
