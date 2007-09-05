#reader(lib "defreader.ss" "scribble")
@require["common.ss"]
@require["list-control-intf.scrbl"]

@define-class-doc[choice% object% (list-control<%>)]{

A choice item allows the user to select one string item from a pop-up
 list of items. Unlike a list box, only the currently selection is
 visible until the user pops-up the menu of choices.

Whenever the selection of a choice item is changed by the user, the
 choice item's callback procedure is invoked. A callback procedure is
 provided as an initialization argument when each choice item is
 created.

See also
@scheme[list-box%].


@defconstructor[([label (or/c label-string? false/c)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [callback ((is-a?/c choice%) (is-a?/c control-event%) . -> . any) 
                           (lambda (c e) (void))]
                 [style (listof (one-of/c 'horizontal-label 'vertical-label
                                          'deleted)) 
                   null]
                 [selection nonnegative-exact-integer? 0]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{

Creates a choice item. If @scheme[label] is a string, it is used as the
 label for the choice item. 

@labelstripped[(scheme label) @elem{} @elem{move the keyboard focus to the choice item}]

The @scheme[choices] list specifies the initial list of user-selectable
 items for the control. The initial set of choices determines the
 control's minimum graphical width (see @|geomdiscuss| for more
 information).

The @scheme[callback] procedure is called (with the event type
 @indexed-scheme['choice]) when the user selects a choice item (or
 re-selects the currently selected item).

@HVLabelNote{choice item} @DeletedStyleNote{choice item}

By default, the first choice (if any) is initially selected. If
 @scheme[selection] is positive, it is passed to
@method[list-control<%> set-selection] to set the initial choice selection. Although @scheme[selection] normally
 must be less than the length of @scheme[choices], it can be @scheme[0]
 when @scheme[choices] is empty.

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]

}}

