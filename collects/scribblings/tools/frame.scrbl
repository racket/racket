#lang scribble/doc
@(require "common.rkt" scribble/core)
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

This method is called during the construction of the @onscreen{View}
menu.  This method is intended to be overridden with the
overriding methods adding other Show/Hide menu items to the @onscreen{View}
menu.

See also
@method[drracket:frame:<%> set-show-menu-sort-key] and
@method[drracket:frame:<%> get-show-menu].
}
@methimpl{
  Does nothing.
}}

@defmethod[(set-show-menu-sort-key [item (is-a?/c menu-item<%>)]
                                   [key (and/c real? positive?)])
           void?]{
  Controls the ordering of items in the @onscreen{View} menu.
                                        
The number determines the sorting order and where separators in the menu appear
(smaller numbers first). 

These are the numbers for many of the @onscreen{View} menu items that come
built-in to DrRacket:
@table[(style #f '())
       (let ()
         (define (add-blocks lol)
           (for/list ([strs (in-list lol)])
             (for/list ([str (in-list (reverse strs))]
                        [i (in-naturals)])
               @paragraph[(style #f '()) 
                          (if (zero? i)
                              (list str "\ua0\ua0\ua0\ua0\ua0")
                              str)])))
         (add-blocks
          (list (list @racket[1] @onscreen{Toolbar})
                (list @racket[2] @onscreen{Split})
                (list @racket[3] @onscreen{Collapse})
                (list @racket[101] @onscreen{Show Definitions})
                (list @racket[102] @onscreen{Show Interactions})
                (list @racket[103] @onscreen{Use Horizontal Layout})
                (list @racket[205] @onscreen{Show Log})
                (list @racket[206] @onscreen{Show Tracing})
                (list @racket[207] @onscreen{Hide Profile})
                (list @racket[301] @onscreen{Show Program Contour})
                (list @racket[302] @onscreen{Show Line Numbers})
                (list @racket[401] @onscreen{Show Module Browser}))))]

In addition, a separator is inserted for each 100. So, for example,
a separator is inserted between @onscreen{Collapse} and
@onscreen{Show Definitions}.

Note that the argument may be a rational number,
effectively allowing insertion between any two menu items already in the menu.
For this reason, avoid using @racket[0], or any number is that @racket[0]
modulo @racket[100].

}
                 
@defmethod[(get-show-menu)
           (is-a?/c menu%)]{
@index{View menu}

returns the @onscreen{View} menu, for use by the
@method[drracket:frame:<%> update-shown] method.

See also
@method[drracket:frame:<%> add-show-menu-items].

The method (and others) uses the word @tt{show} to preserve
backwards compatibility from when the menu itself was named
the @onscreen{Show} menu.

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
