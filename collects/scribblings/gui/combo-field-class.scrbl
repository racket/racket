#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/combo-field}}

@defclass/title[combo-field% text-field% ()]{

A @racket[combo-field%] object is a @racket[text-field%]
 object that also resembles a @racket[choice%] object, because it
 has a small popup button to the right of the text field. Clicking 
 the button pops up a menu, and selecting a menu item typically copies
 the item into the text field.




@defconstructor[([label (or/c label-string? #f)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c combo-field%) (is-a?/c control-event%)
                            . -> . any) 
                           (lambda (c e) (void))]
                 [init-value string ""]
                 [style (listof (or/c 'horizontal-label 'vertical-label 
                                      'deleted)) 
                        null]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #f])]{

If @racket[label] is not @racket[#f], it is used as the combo label.
 Otherwise, the combo does not display its label.

@labelstripped[@racket[label]
  @elem{} @elem{move the keyboard focus to the combo}]

The @racket[choices] list specifies the initial list of items for the
 combo's popup menu. The
@method[combo-field% append] method adds a new item to the menu with a callback to install the
 appended item into the combo's text field. The
@method[combo-field% get-menu] method returns a menu that can be changed to
 adjust the content and actions of the combo's menu.

The @racket[callback] procedure is called when the user changes the text
 in the combo or presses the Enter key (and Enter is not handled by
 the combo's frame or dialog; see
@xmethod[top-level-window<%> on-traverse-char] ). If the user presses Enter, the type of event passed to the callback
 is @indexed-racket['text-field-enter], otherwise it is
 @indexed-racket['text-field].

If @racket[init-value] is not @racket[""], the minimum width of the text item
 is made wide enough to show @racket[init-value]. Otherwise, a built-in
 default width is selected.

@HVLabelNote[@racket[style]]{combo} @DeletedStyleNote[@racket[style] @racket[parent]]{combo}.

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]


}


@defmethod[(append [l label-string?])
           void?]{

Adds a new item to the combo's popup menu. The given label is used for
 the item's name, and the item's callback installs the label into the
 combo's text field.

}


@defmethod[(get-menu)
           (is-a?/c popup-menu%)]{
Returns a @racket[popup-menu%] that is effectively copied into the
 combo's popup menu when the combo is clicked. Only the labels and
 callbacks of the menu's items are used; the enable state, submenus,
 or separators are ignored.
}


@defmethod[(on-popup [event (is-a?/c control-event%)])
           void?]{

@methspec{

Called when the user clicks the combo's popup button. Override this method
to adjust the content of the combo menu on demand.

}
@methimpl{

Does nothing.

}}


}
