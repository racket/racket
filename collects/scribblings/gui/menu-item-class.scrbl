#lang scribble/doc
@(require "common.ss")

@defclass/title[menu-item% object% (selectable-menu-item<%>)]{

A @scheme[menu-item%] is a plain string-labelled menu item. Its
 parent must be a @scheme[menu%] or @scheme[popup-menu%]. When the
 user selects the menu item, its callback procedure is called.


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c menu% popup-menu%))]
                 [callback ((is-a?/c menu-item%) (is-a?/c control-event%) . -> . any) 
                           (lambda (i e) (void))]
                 [shortcut (or/c char? false/c) #f]
                 [help-string (or/c label-string? false/c) #f]
                 [demand-callback ((is-a?/c menu-item%) . -> . any) 
                           (lambda (i) (void))]
                 [shortcut-prefix (listof (one-of/c 'alt 'cmd 'meta 'ctl 
                                                    'shift 'option)) 
                                  (get-default-shortcut-prefix)])]{

Creates a new menu item in @scheme[parent]. The item is initially
 shown, appended to the end of its parent. The @scheme[callback]
 procedure is called (with the event type @indexed-scheme['menu]) when
 the user selects the menu item (either via a menu bar,
 @xmethod[window<%> popup-menu], or @xmethod[editor-admin%
 popup-menu]).

See @method[labelled-menu-item<%> set-label] for information about
mnemonic @litchar{&}s in @scheme[label].

If @scheme[shortcut] is not @scheme[#f], the item has a shortcut. See
@method[selectable-menu-item<%> get-shortcut] for more information.
The @scheme[shortcut-prefix] argument determines the set of modifier
keys for the shortcut; see @method[selectable-menu-item<%>
get-shortcut-prefix].

If @scheme[help] is not @scheme[#f], the item has a help string. See
@method[labelled-menu-item<%> get-help-string] for more information.

The @scheme[demand-callback] procedure is called by the default
@method[menu-item-container<%> on-demand] method with the object itself.

}}
