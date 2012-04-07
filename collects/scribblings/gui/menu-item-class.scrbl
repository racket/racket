#lang scribble/doc
@(require "common.rkt")

@defclass/title[menu-item% object% (selectable-menu-item<%>)]{

A @racket[menu-item%] is a plain string-labelled menu item. Its
 parent must be a @racket[menu%] or @racket[popup-menu%]. When the
 user selects the menu item, its callback procedure is called.


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c menu%) (is-a?/c popup-menu%))]
                 [callback ((is-a?/c menu-item%) (is-a?/c control-event%) . -> . any)]
                 [shortcut (or/c char? symbol? #f) #f]
                 [help-string (or/c label-string? #f) #f]
                 [demand-callback ((is-a?/c menu-item%) . -> . any) 
                           (lambda (i) (void))]
                 [shortcut-prefix (listof (or/c 'alt 'cmd 'meta 'ctl 
                                                'shift 'option)) 
                                  (get-default-shortcut-prefix)])]{

Creates a new menu item in @racket[parent]. The item is initially
 shown, appended to the end of its parent. The @racket[callback]
 procedure is called (with the event type @indexed-racket['menu]) when
 the user selects the menu item (either via a menu bar,
 @xmethod[window<%> popup-menu], or @xmethod[editor-admin%
 popup-menu]).

See @method[labelled-menu-item<%> set-label] for information about
mnemonic @litchar{&}s in @racket[label].

If @racket[shortcut] is not @racket[#f], the item has a shortcut. See
@method[selectable-menu-item<%> get-shortcut] for more information.
The @racket[shortcut-prefix] argument determines the set of modifier
keys for the shortcut; see @method[selectable-menu-item<%>
get-shortcut-prefix].

If @racket[help] is not @racket[#f], the item has a help string. See
@method[labelled-menu-item<%> get-help-string] for more information.

The @racket[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

}}
