#lang scribble/doc
@(require "common.rkt")

@defclass/title[menu% object% (menu-item-container<%> labelled-menu-item<%>)]{

A @racket[menu%] object is a submenu within a @racket[menu%] or
 @racket[popup-menu%], or as a top-level menu in a
 @racket[menu-bar%].


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%) 
                               (is-a?/c menu-bar%))]
                 [help-string (or/c label-string? #f) #f]
                 [demand-callback ((is-a?/c menu%) . -> . any) (lambda (m) (void))])]{

Creates a new menu with the given label.

If @racket[label] contains a @litchar{&} or tab characters, they are
 handled specially in the same way as for menu-item labels and buttons. See
 @method[labelled-menu-item<%> set-label] and @racket[button%].

If @racket[help-string] is not @racket[#f], the menu has a help
string. See @method[labelled-menu-item<%> get-help-string] for more
information.

The @racket[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

}}

