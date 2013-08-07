#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/choice}}

@defclass/title[choice% object% (list-control<%>)]{

A choice item allows the user to select one string item from a pop-up
 list of items. Unlike a list box, only the currently selection is
 visible until the user pops-up the menu of choices.

Whenever the selection of a choice item is changed by the user, the
 choice item's callback procedure is invoked. A callback procedure is
 provided as an initialization argument when each choice item is
 created.

See also @racket[list-box%].


@defconstructor[([label (or/c label-string? #f)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c choice%) (is-a?/c control-event%) . -> . any) 
                           (lambda (c e) (void))]
                 [style (listof (or/c 'horizontal-label 'vertical-label
                                      'deleted)) 
                   null]
                 [selection exact-nonnegative-integer? 0]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 2]
                 [horiz-margin spacing-integer? 2]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a choice item. If @racket[label] is a string, it is used as the
 label for the choice item.

@labelstripped[@racket[label]
  @elem{} @elem{move the keyboard focus to the choice item}]

The @racket[choices] list specifies the initial list of user-selectable
 items for the control. The initial set of choices determines the
 control's minimum graphical width (see @|geomdiscuss| for more
 information).

The @racket[callback] procedure is called (with the event type
 @indexed-racket['choice]) when the user selects a choice item (or
 re-selects the currently selected item).

@HVLabelNote[@racket[style]]{choice item}
@DeletedStyleNote[@racket[style] @racket[parent]]{choice item}

By default, the first choice (if any) is initially selected. If
 @racket[selection] is positive, it is passed to
@method[list-control<%> set-selection] to set the initial choice selection. Although @racket[selection] normally
 must be less than the length of @racket[choices], it can be @racket[0]
 when @racket[choices] is empty.

@FontKWs[@racket[font]] @WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaKWs[]

}}

