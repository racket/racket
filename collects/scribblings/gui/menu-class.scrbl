#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["labelled-menu-item-intf.scrbl"]
@require["menu-item-container-intf.scrbl"]

@define-class-doc[menu% object% (menu-item-container<%> labelled-menu-item<%>)]{

A @scheme[menu%] object is a submenu within a @scheme[menu%] or
 @scheme[popup-menu%], or as a top-level menu in a
 @scheme[menu-bar%].


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%) 
                               (is-a?/c menu-bar%))]
                 [help-string (or/c label-string? false/c) #f]
                 [demand-callback ((is-a?/c menu%) . -> . any) (lambda (m) (void))])]{

Creates a new menu with the given label.

If @scheme[label] contains a @litchar{&}, it is handled specially;
 under Windows, the character following a @litchar{&} is underlined in
 the displayed menu title to indicate a keyboard mnemonic.  Pressing
 and releasing the Alt key switches to menu-selection mode in the menu
 bar where mnemonic characters are used for navigation.  An Alt
 combination might select a specific menu via @method[frame%
 on-menu-char].  A @litchar{&&} in @scheme[label] is replaced by a
 literal (non-navigation) @litchar{&}ampersand. Under X and Mac OS X,
 @litchar{&}s in the label are parsed in the same way as for Windows,
 but no mnemonic underline is displayed.

If @scheme[help-string] is not @scheme[#f], the menu has a help
string. See @method[labelled-menu-item<%> get-help-string] for more
information.

The @scheme[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

}}

